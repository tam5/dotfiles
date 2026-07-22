;;; lisp/command-palette.el -*- lexical-binding: t; -*-

;; A deliberately small amount of code around Vertico's existing completion
;; stack.  The completion, sorting, annotations, and keymaps stay untouched;
;; this file is only responsible for presentation and child-frame lifecycle.

(require 'cl-lib)
(require 'subr-x)

(defgroup my-command-palette nil
  "A polished child-frame presentation for Vertico."
  :group 'minibuffer)

(defvar marginalia-annotators)
(defvar marginalia-align-offset)
(defvar marginalia-command-categories)
(defvar consult-preview-key)
(defvar posframe--frame)
(defvar read-extended-command-predicate)
(defvar vertico-count)
(defvar vertico-count-format)
(defvar vertico-group-format)
(defvar vertico-posframe-border-width)
(defvar vertico-posframe-font)
(defvar vertico-posframe-parameters)
(defvar vertico-posframe-poshandler)
(defvar vertico-posframe-size-function)
(defvar vertico-posframe-truncate-lines)
(defvar vertico-resize)

(declare-function nerd-icons-faicon "nerd-icons")
(declare-function posframe--create-posframe "posframe")
(declare-function posframe--make-frame-visible "posframe")
(declare-function vertico--metadata-get "vertico")
(declare-function vertico-posframe--get-border-color "vertico-posframe")
(declare-function vertico-posframe-last-window "vertico-posframe")

(defconst my/command-palette-background "#242c34")
(defconst my/command-palette-selection "#354650")
(defconst my/command-palette-foreground "#e0e6ec")
(defconst my/command-palette-muted "#94a1ad")
(defconst my/command-palette-accent "#80cbc4")

(defconst my/command-palette--backdrop-buffer-name
  " *command-palette-backdrop*")

(defvar my/command-palette--backdrop-frame nil)
(defvar my/command-palette--command-warmup-timer nil)
(defvar my/command-palette--mode-warmup-timer nil)
(defvar my/command-palette--command-generation 0)
(defvar my/command-palette--warmed-command-names nil)
(defvar my/command-palette--warmed-command-context nil)
(defvar my/command-palette--warmed-command-snapshot nil)

(defvar my/command-palette--reading-command nil
  "Non-nil while `read-extended-command-1' is setting up M-x.")

(defvar my/command-palette--consult-buffer-reading nil
  "Non-nil while Consult is reading a buffer or recent file.")

(defvar-local my/command-palette--input-overlay nil)
(defvar-local my/command-palette--caret-overlay nil)
(defvar-local my/command-palette--saved-face-remapping-alist nil)
(defvar-local my/command-palette--faces-remapped-p nil)
(defvar-local my/command-palette--saved-which-key-inhibit nil)
(defvar-local my/command-palette--which-key-inhibited-p nil)
(defvar-local my/command-palette--compact-p nil)

(defface my/command-palette-prompt
  `((t (:foreground ,my/command-palette-accent
        :weight semibold :height 1.30)))
  "Face used by the command palette's search glyph."
  :group 'my-command-palette)

(defface my/command-palette-path
  `((t (:foreground ,my/command-palette-muted)))
  "Face used by the directory portion of quick-open candidates."
  :group 'my-command-palette)

(defface my/command-palette-file
  `((t (:foreground ,my/command-palette-foreground :weight medium)))
  "Face used by the filename portion of quick-open candidates."
  :group 'my-command-palette)

(defface my/command-palette-match
  `((t (:foreground ,my/command-palette-accent :weight bold)))
  "Face used for fuzzy-match ink without Doom's colored match boxes."
  :group 'my-command-palette)

(defun my/command-palette--anchor-window ()
  "Return the editor window the palette should be visually anchored to."
  (let* ((last (and (fboundp 'vertico-posframe-last-window)
                    (vertico-posframe-last-window)))
         (last-frame (and (window-live-p last) (window-frame last)))
         ;; Posframe's cached window can itself belong to a child frame during
         ;; startup.  Always search the top-level editor frame; a child frame's
         ;; ordinary root window is not a minibuffer window even when it is
         ;; displaying *Minibuf-N*.
         (frame
          (let ((candidate
                 (or (and (frame-live-p last-frame) last-frame)
                     (selected-frame))))
            (while (frame-parent candidate)
              (setq candidate (frame-parent candidate)))
            candidate)))
    (if (and (window-live-p last)
             (eq (window-frame last) frame)
             (not (window-minibuffer-p last))
             (not (minibufferp (window-buffer last)))
             (not (window-parameter last 'window-side)))
        last
      (car
       (sort
        (cl-remove-if
         (lambda (window)
           (or (window-minibuffer-p window)
               (minibufferp (window-buffer window))
               (window-parameter window 'window-side)))
         (window-list frame 'no-minibuffer))
        (lambda (a b)
          (> (* (window-pixel-width a) (window-pixel-height a))
             (* (window-pixel-width b) (window-pixel-height b)))))))))

(defun my/command-palette-poshandler (info)
  "Place a palette described by INFO over the active editor, not its sidebar."
  (let* ((window (my/command-palette--anchor-window))
         (edges (and (window-live-p window) (window-pixel-edges window)))
         (left (or (nth 0 edges) 0))
         (top (or (nth 1 edges) 0))
         (width (if edges
                    (- (nth 2 edges) left)
                  (plist-get info :parent-frame-width)))
         (height (if edges
                     (- (nth 3 edges) top)
                   (plist-get info :parent-frame-height)))
         (parent-width (plist-get info :parent-frame-width))
         (parent-height (plist-get info :parent-frame-height))
         (palette-width (plist-get info :posframe-width))
         (palette-height (plist-get info :posframe-height))
         (margin 16)
         (wanted-x (+ left (/ (- width palette-width) 2)))
         (wanted-y (+ top (round (* height 0.055))))
         (max-x (max margin (- parent-width palette-width margin)))
         (max-y (max margin (- parent-height palette-height margin))))
    (cons (min max-x (max margin wanted-x))
          (min max-y (max margin wanted-y)))))

(defun my/command-palette--target-width-pixels ()
  "Return the responsive palette width for the current editor window."
  (let* ((window (my/command-palette--anchor-window))
         (parent (window-frame window))
         (editor-width (window-pixel-width window))
         (available-width
          (max 80
               (min editor-width
                    (- (frame-pixel-width parent) 32)))))
    (min
     available-width
     (if (< available-width 900)
         (max 160 (- available-width 48))
       (min 1500 (round (* available-width 0.60)))))))

(defun my/command-palette-size (buffer)
  "Return stable, responsive posframe dimensions for minibuffer BUFFER."
  (with-current-buffer buffer
    (let* ((window (my/command-palette--anchor-window))
           (parent (window-frame window))
           (char-width (max 1 (frame-char-width parent)))
           ;; A compact editor gets nearly all of its width.  Larger editors
           ;; use a broad, readable card without turning descriptions into a
           ;; single unscannable line.
           (target-pixels (my/command-palette--target-width-pixels))
           (width (max 1 (round (/ target-pixels char-width))))
           ;; `line-spacing' gives the rows their breathing room, so the
           ;; canonical frame height must be larger than the raw line count.
           ;; Keeping it fixed also prevents distracting resize jumps while
           ;; filtering from many results down to a handful.
           (char-height (max 1 (frame-char-height parent)))
           ;; Size from the target pixel rhythm instead of an arbitrary line
           ;; multiplier.  Cocoa rounds the result to a canonical line, while
           ;; the extra prompt height and frame line spacing determine the
           ;; rendered rows.  Every requested result must fit in full.
           (target-height (+ 58 (* vertico-count 49)))
           (height (ceiling (/ target-height (float char-height)))))
      (list :width width
            :min-width width
            :max-width width
            :height height
            :min-height height
            :max-height height))))

(defun my/command-palette--read-command-a (orig-fn &rest args)
  "Mark the M-x read performed by ORIG-FN for command-table snapshotting."
  (let ((my/command-palette--reading-command t))
    (apply orig-fn args)))

(defun my/command-palette--command-table
    (source predicate &optional initial-snapshot)
  "Return an M-x table that snapshots valid candidates from SOURCE once.
PREDICATE is applied by SOURCE while the snapshot is built.  Metadata still
comes from SOURCE, so command annotations and completion-category behavior are
unchanged."
  (let ((snapshot initial-snapshot)
        (snapshot-ready-p (and initial-snapshot t)))
    (lambda (string _predicate action)
      (cond
       ((eq action 'metadata)
        (funcall source string predicate action))
       ((eq (car-safe action) 'boundaries) nil)
       (t
        (unless snapshot-ready-p
          ;; Emacs' obarray table and mode-aware command predicate are the
          ;; expensive part of M-x.  Materialize them once for this invocation;
          ;; Hotfuzz can then filter the plain list in its native module.
          (setq snapshot (all-completions "" source predicate)
                snapshot-ready-p t))
        (if (and (eq action 'lambda)
                 (not (test-completion string snapshot nil)))
            ;; Preserve the stock escape hatch that accepts an obsolete command
            ;; when its exact name is entered, even though it is not advertised.
            (funcall source string predicate action)
          (complete-with-action action snapshot string nil)))))))

(defun my/command-palette--completing-read-a
    (orig-fn prompt collection &optional predicate require-match
             initial-input hist def inherit-input-method)
  "Snapshot M-x COLLECTION before delegating to ORIG-FN.
All other completion commands retain their original tables and predicates."
  (if (not my/command-palette--reading-command)
      (funcall orig-fn prompt collection predicate require-match
               initial-input hist def inherit-input-method)
    (let* ((buffer (current-buffer))
           (context (my/command-palette--command-context buffer))
           (warmed
            (and (equal context my/command-palette--warmed-command-context)
                 my/command-palette--warmed-command-snapshot)))
      (funcall orig-fn prompt
               (my/command-palette--command-table
                collection predicate warmed)
               nil require-match initial-input hist def inherit-input-method))))

(defun my/command-palette--format-candidate-a
    (orig-fn candidate prefix suffix index start)
  "Give each candidate handled by ORIG-FN horizontal breathing room."
  (let ((category (vertico--metadata-get 'category))
        (consult-kind
         (car-safe (get-text-property 0 'multi-category candidate))))
    (when (or (eq category 'my/project-file)
              (and my/command-palette--consult-buffer-reading
                   (or (eq category 'file)
                       (eq consult-kind 'file))))
    ;; The completion value stays untouched; this CAND is Vertico's display
    ;; copy.  Lead with the basename for fast scanning, then retain the parent
    ;; path as quiet context (the same hierarchy used by native file pickers).
    (if-let* ((slash (cl-position ?/ candidate :from-end t)))
        (let ((directory (substring candidate 0 (1+ slash)))
              (basename (substring candidate (1+ slash))))
          (add-face-text-property 0 (length directory)
                                  'my/command-palette-path nil directory)
          (add-face-text-property 0 (length basename)
                                  'my/command-palette-file nil basename)
          (setq candidate (concat basename "   " directory)))
      (add-face-text-property 0 (length candidate)
                              'my/command-palette-file nil candidate)))
  (if (and (bound-and-true-p vertico-posframe-mode)
           (minibufferp))
      (funcall orig-fn candidate (concat "      " prefix)
               (concat suffix "      ") index start)
    (funcall orig-fn candidate prefix suffix index start))))

(defun my/command-palette--caret-image ()
  "Return a small, optically centered caret image, or nil without SVG support."
  (when (image-type-available-p 'svg)
    (create-image
     (concat
      "<svg xmlns='http://www.w3.org/2000/svg' width='1' height='16' "
      "viewBox='0 0 1 16'><rect width='1' height='16' rx='0.5' "
      "fill='#f5f7f8'/></svg>")
     'svg t :ascent 'center)))

(defun my/command-palette--update-caret (&rest _)
  "Keep the palette's compact visual caret at the actual input point."
  (when (overlayp my/command-palette--caret-overlay)
    (move-overlay my/command-palette--caret-overlay (point) (point)))
  (setq-local cursor-type nil
              cursor-in-non-selected-windows nil))

(defun my/command-palette--hide-frame-cursor-a (orig-fn &rest args)
  "Let ORIG-FN render the palette with only its purpose-sized caret."
  (if (and (minibufferp)
           (overlayp my/command-palette--caret-overlay))
      (let ((cursor-type nil))
        (prog1 (apply orig-fn args)
          ;; Long proportional-font candidates can make Cocoa grow a new
          ;; child frame beyond Posframe's requested column width.  Reassert
          ;; the exact pixel width and recenter only when that happens.
          (when (and (frame-live-p posframe--frame)
                     (frame-parent posframe--frame))
            (let ((target (my/command-palette--target-width-pixels)))
              (unless (= (frame-pixel-width posframe--frame) target)
                (set-frame-size posframe--frame target
                                (frame-pixel-height posframe--frame) t))
              (let ((position
                     (my/command-palette-poshandler
                      (list
                       :parent-frame-width
                       (frame-pixel-width (frame-parent posframe--frame))
                       :parent-frame-height
                       (frame-pixel-height (frame-parent posframe--frame))
                       :posframe-width target
                       :posframe-height
                       (frame-pixel-height posframe--frame)))))
                (set-frame-position posframe--frame
                                    (car position) (cdr position)))))))
    (apply orig-fn args)))

(defun my/command-palette--initialize-window-cursor-state (window)
  "Give palette WINDOW the cursor state Emacs expects before redisplay."
  (when (window-live-p window)
    (let ((buffer (window-buffer window)))
      (when (and (buffer-live-p buffer)
                 (minibufferp buffer))
        (with-current-buffer buffer
          (let ((position (point)))
            (set-window-point window position)
            (set-window-parameter window 'cursor-intangible--last-point
                                  position)))))))

(defun my/command-palette--initialize-frame-before-show-a (orig-fn frame)
  "Initialize palette cursor state, then let ORIG-FN reveal FRAME."
  ;; A frame created by the idle warm-up can be replaced while Posframe
  ;; reconciles the real display arguments.  Initialize the final root window
  ;; at Posframe's visibility boundary, after any replacement but before Cocoa
  ;; can redisplay it.  Emacs 31's cursor sensor expects both values to be
  ;; numeric; Posframe normally records them only after the frame is visible.
  (when (and (frame-live-p frame)
             (frame-parent frame))
    (let* ((window (frame-root-window frame))
           (buffer (and (window-live-p window) (window-buffer window))))
      (when (and (buffer-live-p buffer)
                 (minibufferp buffer)
                 (buffer-local-value
                  'my/command-palette--faces-remapped-p buffer))
        (my/command-palette--initialize-window-cursor-state window))))
  (funcall orig-fn frame))

(defun my/command-palette--style-prompt ()
  "Replace the implementation-specific minibuffer prompt with a search glyph."
  (when (and (minibufferp)
             (bound-and-true-p vertico-posframe-mode)
             (bound-and-true-p vertico--input))
    (let ((inhibit-read-only t)
          (end (minibuffer-prompt-end)))
      (when (> end (point-min))
        (add-text-properties
         (point-min) end
         `(display
           ,(concat
             "      "
             (if (require 'nerd-icons nil t)
                 (nerd-icons-faicon
                  "nf-fa-magnifying_glass"
                  :face 'my/command-palette-prompt)
               (propertize "⌕" 'face 'my/command-palette-prompt))
             (propertize "   " 'face '(:height 1.12)))
           rear-nonsticky t))))
  ;; The icon establishes the taller header line.  This overlay gives typed
  ;; search text its own subtle hierarchy without enlarging candidate rows.
  (when (overlayp my/command-palette--input-overlay)
    (delete-overlay my/command-palette--input-overlay))
  (setq my/command-palette--input-overlay
        (make-overlay (minibuffer-prompt-end) (point-max) nil nil t))
  (overlay-put my/command-palette--input-overlay
               'face '(:height 1.12 :weight medium))
  (when (overlayp my/command-palette--caret-overlay)
    (delete-overlay my/command-palette--caret-overlay))
  (if-let* ((image (my/command-palette--caret-image)))
      (progn
        (setq my/command-palette--caret-overlay
              (make-overlay (point) (point) nil nil t))
        (overlay-put my/command-palette--caret-overlay
                     'after-string (propertize " " 'display image))
        (setq-local cursor-type nil)
        (add-hook 'post-command-hook
                  #'my/command-palette--update-caret nil t))
    (setq-local cursor-type '(bar . 1)))))

(defun my/command-palette--backdrop-buffer ()
  "Return the empty buffer displayed by the palette backdrop."
  (let ((buffer (get-buffer-create my/command-palette--backdrop-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq-local cursor-type nil
                  mode-line-format nil
                  header-line-format nil
                  display-line-numbers nil
                  buffer-read-only t))
    buffer))

(defun my/command-palette--delete-backdrop ()
  "Delete the cached backdrop frame, if it exists."
  (when (frame-live-p my/command-palette--backdrop-frame)
    (delete-frame my/command-palette--backdrop-frame t))
  (setq my/command-palette--backdrop-frame nil))

(defun my/command-palette--ensure-backdrop (parent)
  "Return an invisible, reusable dimming child frame for PARENT."
  (when (and (frame-live-p my/command-palette--backdrop-frame)
             (not (eq (frame-parent my/command-palette--backdrop-frame)
                      parent)))
    (my/command-palette--delete-backdrop))
  (unless (frame-live-p my/command-palette--backdrop-frame)
    (let ((after-make-frame-functions nil)
          (before-make-frame-hook nil)
          (buffer (my/command-palette--backdrop-buffer)))
      (setq my/command-palette--backdrop-frame
            (make-frame
             `((parent-frame . ,parent)
               (minibuffer . nil)
               (left . 0)
               (top . 0)
               (width . 1)
               (height . 1)
               (visibility . nil)
               (undecorated . t)
               (no-accept-focus . t)
               (no-focus-on-map . t)
               (skip-taskbar . t)
               (no-other-frame . t)
               (desktop-dont-save . t)
               (unsplittable . t)
               (internal-border-width . 0)
               (border-width . 0)
               (left-fringe . 0)
               (right-fringe . 0)
               (menu-bar-lines . 0)
               (tool-bar-lines . 0)
               (tab-bar-lines . 0)
               (vertical-scroll-bars . nil)
               (horizontal-scroll-bars . nil)
               (background-color . "#000000")
               ;; A translucent black child frame supplies the same quiet
               ;; backdrop Sublime uses without modifying any editor faces.
               ;; Use whole-frame alpha here: Cocoa child frames do not
               ;; composite `alpha-background' reliably.
               (alpha . 20))))
      (let ((window (frame-root-window my/command-palette--backdrop-frame)))
        (set-window-buffer window buffer)
        (set-window-dedicated-p window t))
      (redirect-frame-focus my/command-palette--backdrop-frame parent)))
  my/command-palette--backdrop-frame)

(defun my/command-palette--show-backdrop ()
  "Dim the current parent frame behind a graphical Vertico session."
  (when-let* ((window (minibuffer-selected-window))
              (parent (window-frame window))
              ((display-graphic-p parent))
              (backdrop (my/command-palette--ensure-backdrop parent)))
    (set-frame-position backdrop 0 0)
    (set-frame-size backdrop
                    (frame-pixel-width parent)
                    (frame-pixel-height parent)
                    t)
    (make-frame-visible backdrop)
    (raise-frame backdrop)))

(defun my/command-palette--hide-backdrop ()
  "Hide the palette backdrop after the outermost minibuffer exits."
  (when (and (= (minibuffer-depth) 1)
             (frame-live-p my/command-palette--backdrop-frame))
    (make-frame-invisible my/command-palette--backdrop-frame t)))

(defun my/command-palette--face-remaps ()
  "Return face remaps confined to the active palette minibuffer."
  `((default
      (:background ,my/command-palette-background
       :foreground ,my/command-palette-foreground)
      default)
    (vertico-current
     (:background ,my/command-palette-selection
      :foreground "#f5f7f8" :weight semibold :extend t)
     vertico-current)
    ;; Doom's Orderless faces carry four colored backgrounds.  Replacing the
    ;; faces wholesale preserves match emphasis without rectangular swatches.
    (orderless-match-face-0 my/command-palette-match)
    (orderless-match-face-1 my/command-palette-match)
    (orderless-match-face-2 my/command-palette-match)
    (orderless-match-face-3 my/command-palette-match)
    (marginalia-documentation
     (:foreground ,my/command-palette-muted :height 0.88 :weight regular)
     marginalia-documentation)
    (marginalia-key
     (:foreground "#9aa7b3" :height 0.88 :weight medium)
     marginalia-key)
    (completions-common-part my/command-palette-match)))

(defun my/command-palette--restore-local-state ()
  "Restore minibuffer-local UI state after the palette exits."
  (my/command-palette--hide-backdrop)
  (when my/command-palette--faces-remapped-p
    (setq-local face-remapping-alist
                my/command-palette--saved-face-remapping-alist)
    (setq my/command-palette--faces-remapped-p nil))
  (when my/command-palette--which-key-inhibited-p
    (set-default 'which-key-inhibit
                 my/command-palette--saved-which-key-inhibit)
    (setq my/command-palette--which-key-inhibited-p nil))
  (remove-hook 'post-command-hook #'my/command-palette--update-caret t)
  (dolist (overlay (list my/command-palette--input-overlay
                         my/command-palette--caret-overlay))
    (when (overlayp overlay)
      (delete-overlay overlay)))
  (setq my/command-palette--input-overlay nil
        my/command-palette--caret-overlay nil))

(defun my/command-palette--minibuffer-setup ()
  "Prepare the prompt and backdrop for a Vertico child-frame session."
  (when (and (bound-and-true-p vertico-posframe-mode)
             (bound-and-true-p vertico--input)
             (display-graphic-p))
    ;; Install cleanup before any state changes, especially the one global
    ;; Which Key inhibition below.
    (add-hook 'minibuffer-exit-hook
              #'my/command-palette--restore-local-state nil t)
    ;; The hidden warmed child already displays this minibuffer, and Emacs can
    ;; run its cursor sensor as soon as the prompt contents change—before
    ;; Posframe's show path.  Seed every existing view now; the visibility
    ;; advice below handles a child frame Posframe may replace later.
    (dolist (window (get-buffer-window-list (current-buffer) t t))
      (my/command-palette--initialize-window-cursor-state window))
    ;; Derive result count from the actual anchor window, not the full frame;
    ;; this keeps the card inside short horizontal splits.
    (let* ((anchor-height (window-pixel-height
                           (my/command-palette--anchor-window)))
           (available (max 0 (- anchor-height 58 32)))
           (rows (floor (/ available 49.0))))
      (setq-local vertico-count (max 3 (min 8 rows))
                  my/command-palette--compact-p
                  (< (my/command-palette--target-width-pixels) 900)))
    ;; Consult's source headings consume result slots and leave a nearly empty
    ;; card when buffer and recent-file sources meet.  The filename/path and
    ;; mode annotations already communicate that distinction more quietly.
    (when my/command-palette--consult-buffer-reading
      (setq-local vertico-group-format nil))
    ;; Solaire normally remaps minibuffer `default' to the editor background.
    ;; Keep every palette-specific face local so Corfu and non-posframe
    ;; completion UIs retain their own theme styling.
    (setq my/command-palette--saved-face-remapping-alist
          (copy-tree face-remapping-alist)
          my/command-palette--faces-remapped-p t)
    (setq-local face-remapping-alist
                (append
                 (my/command-palette--face-remaps)
                 (cl-remove-if
                  (lambda (entry)
                    (assq (car-safe entry)
                          (my/command-palette--face-remaps)))
                  face-remapping-alist)))
    (setq-local marginalia-align-offset 3)
    ;; A prefix map can briefly leave Which Key's independent child frame on
    ;; screen as C-x b opens.  Hide it for one coherent modal surface.
    (when (boundp 'which-key-inhibit)
      (setq my/command-palette--saved-which-key-inhibit
            (default-value 'which-key-inhibit)
            my/command-palette--which-key-inhibited-p t)
      (set-default 'which-key-inhibit t))
    (when (fboundp 'which-key--hide-popup-ignore-command)
      (ignore-errors (which-key--hide-popup-ignore-command)))
    (my/command-palette--style-prompt)
    (my/command-palette--show-backdrop)))

(defun my/command-palette--apply-faces ()
  "Apply only frame-level palette colors after every theme change."
  (dolist (face '(vertico-posframe-border
                  vertico-posframe-border-2
                  vertico-posframe-border-3
                  vertico-posframe-border-4
                  vertico-posframe-border-fallback))
    (when (facep face)
      (set-face-attribute face nil
                          :background my/command-palette-background)))
  (set-face-attribute 'vertico-posframe nil
                      :inherit 'default
                      :background my/command-palette-background
                      :foreground my/command-palette-foreground))

(defun my/command-palette--no-project-file-annotation (_candidate)
  "Keep Projectile's quick-open rows free of file permission metadata."
  nil)

(defun my/command-palette--annotate-command-a (orig-fn candidate)
  "Hide ORIG-FN's long command description in a compact palette."
  ;; Marginalia annotates from the originating editor buffer, not from the
  ;; minibuffer.  Inspect the active Vertico read explicitly; using
  ;; `(minibufferp)' here silently re-enables descriptions and clips them at a
  ;; compact frame's edge.  Derive the width directly because the first
  ;; affixation can precede our minibuffer setup hook.
  (unless
      (when-let* ((window (active-minibuffer-window))
                  (buffer (window-buffer window))
                  ((minibufferp buffer)))
        (with-current-buffer buffer
          (and (bound-and-true-p vertico-posframe-mode)
               (bound-and-true-p vertico--input)
               (< (my/command-palette--target-width-pixels) 900))))
    (funcall orig-fn candidate)))

(defun my/command-palette--command-context (buffer)
  "Return the cache key for M-x candidates applicable to BUFFER."
  (with-current-buffer buffer
    (list buffer
          major-mode
          (copy-sequence local-minor-modes)
          (copy-sequence global-minor-modes)
          read-extended-command-predicate
          my/command-palette--command-generation)))

(defun my/command-palette--command-advertised-p (name buffer)
  "Return non-nil when command NAME should be offered for BUFFER."
  (when-let* ((symbol (intern-soft name))
              ((commandp symbol))
              ((or (null read-extended-command-predicate)
                   (not (functionp read-extended-command-predicate))
                   (condition-case-unless-debug err
                       (funcall read-extended-command-predicate symbol buffer)
                     (error
                      (message "read-extended-command-predicate: %s: %s"
                               symbol (error-message-string err)))))))
    (let ((obsolete (get symbol 'byte-obsolete-info)))
      (or (not obsolete)
          (functionp (car obsolete))
          (condition-case nil
              (>= (car (version-to-list (caddr obsolete)))
                  emacs-major-version)
            (error t))))))

(defun my/command-palette--warm-command-table (&optional buffer)
  "Warm Emacs' command predicates before the user's first M-x."
  (let ((buffer
         (if (buffer-live-p buffer)
             buffer
           (window-buffer (my/command-palette--anchor-window)))))
    ;; The first obarray-wide `commandp' pass faults in many autoload and mode
    ;; tables.  Do it while idle, then prepare the same mode-sensitive subset
    ;; M-x needs.  A context mismatch falls back to Emacs' canonical source.
    ;; Refresh the raw names too: packages may define commands lazily after the
    ;; startup snapshot, and an idle refresh should never make M-x stale.
    (setq my/command-palette--warmed-command-names
          (all-completions "" obarray #'commandp))
    (let ((snapshot
           (cl-loop for name in my/command-palette--warmed-command-names
                    when (my/command-palette--command-advertised-p name buffer)
                    collect name)))
      ;; Capture the key last in case a predicate loads a library and advances
      ;; `my/command-palette--command-generation' while filtering.
      (setq my/command-palette--warmed-command-snapshot snapshot
            my/command-palette--warmed-command-context
            (my/command-palette--command-context buffer)))))

(defun my/command-palette--warm-active-editor ()
  "Warm M-x for the editor visible when this callback actually runs."
  (when-let* ((window (my/command-palette--anchor-window))
              ((window-live-p window)))
    (my/command-palette--warm-command-table (window-buffer window))))

(defun my/command-palette--schedule-command-warmup (&optional _buffer)
  "Refresh the idle M-x snapshot after entering a new major mode."
  ;; Resolve the buffer when the timer runs.  Mode hooks also fire in transient
  ;; buffers, which may be deleted—or merely irrelevant—before Emacs is idle.
  (when (timerp my/command-palette--command-warmup-timer)
    (cancel-timer my/command-palette--command-warmup-timer))
  (setq my/command-palette--command-warmup-timer
        (run-with-idle-timer 0 nil
                             #'my/command-palette--warm-active-editor)))

(defun my/command-palette--invalidate-command-cache (&rest _)
  "Invalidate M-x after a library adds or changes commands."
  (cl-incf my/command-palette--command-generation)
  (my/command-palette--schedule-command-warmup))

(defun my/command-palette--schedule-active-editor-warmup ()
  "Refresh M-x for the visible editor after a global mode transition."
  ;; Globalized modes run their hooks while still propagating to buffers.  A
  ;; short idle defer runs after propagation without interrupting typing.
  (when (timerp my/command-palette--mode-warmup-timer)
    (cancel-timer my/command-palette--mode-warmup-timer))
  (setq my/command-palette--mode-warmup-timer
        (run-with-idle-timer 0.5 nil
                             #'my/command-palette--warm-active-editor)))

(defun my/command-palette--precreate-hidden-frame ()
  "Create the first reusable palette child frame while Emacs is idle."
  (condition-case-unless-debug err
      (when (and (display-graphic-p)
                 (featurep 'vertico-posframe))
    ;; At depth zero Emacs only owns *Minibuf-0*; the first real prompt lazily
    ;; creates *Minibuf-1*.  Cycle through a prompt that exits in its setup hook
    ;; before redisplay, with the palette disabled, so the reusable Cocoa child
    ;; window can be built now instead of on the user's first M-x.
    (unless (or (active-minibuffer-window)
                (get-buffer " *Minibuf-1*"))
      (condition-case-unless-debug err
          (let ((vertico-posframe-mode nil)
                (minibuffer-setup-hook (list #'exit-minibuffer))
                (minibuffer-exit-hook nil)
                (inhibit-message t))
            (read-from-minibuffer ""))
        (error
         (message "Command palette minibuffer warm-up: %s"
                  (error-message-string err)))))
    (let* ((parent (selected-frame))
           ;; The permanent *Minibuf-0* never hosts a top-level read.  Only
           ;; preallocate the depth-one buffer used by the first interaction.
           (buffers
            (delq nil (list (get-buffer " *Minibuf-1*")))))
      ;; These arguments intentionally mirror `vertico-posframe--show'.
      ;; `posframe--create-posframe' leaves the frame hidden, so Cocoa/font
      ;; initialization moves off the user's first M-x without any flash.
      (save-selected-window
        (dolist (buffer buffers)
          (let* ((anchor-height
                  (window-pixel-height (my/command-palette--anchor-window)))
                 (rows
                  (max 3 (min 8 (floor (/ (max 0 (- anchor-height 90))
                                           49.0)))))
                 (frame
                  (posframe--create-posframe
                   buffer
                   :parent-frame parent
                   :foreground-color (face-attribute 'vertico-posframe
                                                     :foreground nil t)
                   :background-color (face-attribute 'vertico-posframe
                                                     :background nil t)
                   :border-width vertico-posframe-border-width
                   :border-color (vertico-posframe--get-border-color)
                   :font vertico-posframe-font
                   :cursor nil
                   :tty-non-selected-cursor t
                   :lines-truncate vertico-posframe-truncate-lines
                   :override-parameters vertico-posframe-parameters)))
            ;; Allocate the real Cocoa backing store while the frame is hidden.
            ;; Merely creating a 1×1 child defers most of the first-show cost.
            (set-frame-size frame
                            (my/command-palette--target-width-pixels)
                            (+ 72 (* rows 48)) t))))))
    (error
     (display-warning
      'my-command-palette
      (format "Skipping optional child-frame warm-up: %s"
              (error-message-string err))
      :warning)
     nil)))

(defun my/command-palette--consult-buffer-a (orig-fn &rest args)
  "Run ORIG-FN with palette-specific, inexpensive buffer metadata."
  (let ((my/command-palette--consult-buffer-reading t)
        ;; Keep the modal background stable.  Live preview can replace the
        ;; editor with Doom's dashboard, whose bright action hints look like a
        ;; second popup beneath the palette.
        (consult-preview-key nil))
    (apply orig-fn args)))

(defun my/command-palette--annotate-buffer-a (orig-fn candidate)
  "Replace noisy buffer annotations for Consult, otherwise call ORIG-FN."
  (if (not my/command-palette--consult-buffer-reading)
      (funcall orig-fn candidate)
    (when-let* ((buffer (or (and (stringp candidate)
                                 (get-text-property
                                  0 'uniquify-orig-buffer candidate))
                            (get-buffer candidate)))
                ((buffer-live-p buffer)))
      (with-current-buffer buffer
        (let* ((mode (string-trim (format-mode-line mode-name)))
               (file buffer-file-name)
               (directory
                (and file
                     (abbreviate-file-name
                      (file-name-directory file)))))
          (concat
           (propertize " " 'marginalia--align t)
           (propertize mode 'face 'marginalia-mode)
           (when directory
             (concat
              (propertize "   " 'face 'marginalia-documentation)
              (propertize directory 'face 'marginalia-file-name)))))))))

(defun my/command-palette--annotate-file-a (orig-fn candidate)
  "Avoid filesystem-stat metadata in Consult's buffer switcher."
  (unless my/command-palette--consult-buffer-reading
    (funcall orig-fn candidate)))

(defun my/command-palette--warm-up ()
  "Load palette code and prepare its reusable backdrop after startup."
  (condition-case-unless-debug err
      (when (and (display-graphic-p)
                 (require 'vertico-posframe nil t))
      ;; The palette remains usable with Emacs' built-in matcher if an optional
      ;; native module ever fails to load.
      (condition-case-unless-debug err
          (require 'hotfuzz nil t)
        (error
         (display-warning
          'hotfuzz
          (format "Native matcher unavailable; using completion fallback: %s"
                  (error-message-string err))
          :warning)))
      (condition-case-unless-debug err
          (let ((backdrop
                 (my/command-palette--ensure-backdrop (selected-frame))))
            (set-frame-size backdrop
                            (frame-pixel-width (selected-frame))
                            (frame-pixel-height (selected-frame)) t)
            (make-frame-invisible backdrop t))
        (error
         (display-warning
          'my-command-palette
          (format "Skipping optional backdrop warm-up: %s"
                  (error-message-string err))
          :warning)))
      (my/command-palette--precreate-hidden-frame)
      (my/command-palette--schedule-command-warmup))
    (error
     (display-warning
      'my-command-palette
      (format "Skipping optional palette startup warm-up: %s"
              (error-message-string err))
      :warning)
     nil)))

(after! vertico-posframe
  (setq vertico-count 9
        vertico-resize nil
        vertico-count-format nil
        vertico-posframe-truncate-lines t
        vertico-posframe-font
        "-*-Inter 1.5-regular-*-*-*-*-160-*-*-*-*-*-*"
        vertico-posframe-border-width 0
        vertico-posframe-poshandler #'my/command-palette-poshandler
        vertico-posframe-size-function #'my/command-palette-size
        vertico-posframe-parameters
        '((undecorated . nil)
          (undecorated-round . t)
          ;; Emacs otherwise puts every extra pixel below the baseline, making
          ;; the ink float high inside an otherwise exact 49px row.
          (line-spacing . (6 . 8))
          (alpha-background . 100)))

  (unless (advice-member-p #'my/command-palette--format-candidate-a
                           'vertico--format-candidate)
    (advice-add 'vertico--format-candidate
                :around #'my/command-palette--format-candidate-a))
  (unless (advice-member-p #'my/command-palette--hide-frame-cursor-a
                           'vertico-posframe--show)
    (advice-add 'vertico-posframe--show
                :around #'my/command-palette--hide-frame-cursor-a))
  (unless (advice-member-p
           #'my/command-palette--initialize-frame-before-show-a
           'posframe--make-frame-visible)
    (advice-add 'posframe--make-frame-visible
                :around
                #'my/command-palette--initialize-frame-before-show-a))
  (add-hook 'minibuffer-setup-hook #'my/command-palette--minibuffer-setup)
  (add-hook 'doom-load-theme-hook #'my/command-palette--apply-faces)
  (my/command-palette--apply-faces))

(after! marginalia
  (add-to-list 'marginalia-command-categories
               '(projectile-find-file . my/project-file))
  (add-to-list 'marginalia-annotators
               '(my/project-file
                 my/command-palette--no-project-file-annotation
                 builtin none))
  (unless (advice-member-p #'my/command-palette--annotate-buffer-a
                           'marginalia-annotate-buffer)
    (advice-add 'marginalia-annotate-buffer
                :around #'my/command-palette--annotate-buffer-a))
  (unless (advice-member-p #'my/command-palette--annotate-file-a
                           'marginalia-annotate-file)
    (advice-add 'marginalia-annotate-file
                :around #'my/command-palette--annotate-file-a))
  (unless (advice-member-p #'my/command-palette--annotate-command-a
                           'marginalia-annotate-command)
    (advice-add 'marginalia-annotate-command
                :around #'my/command-palette--annotate-command-a))
  (my/command-palette--apply-faces))

(after! consult
  (unless (advice-member-p #'my/command-palette--consult-buffer-a
                           'consult-buffer)
    (advice-add 'consult-buffer
                :around #'my/command-palette--consult-buffer-a)))

(after! orderless
  (my/command-palette--apply-faces))

(after! hotfuzz
  ;; The native matcher performs the same fuzzy-search job as the previous
  ;; Orderless flex fallback, but filters and sorts the entire command table in
  ;; one C call.  Keep Orderless for categories where its space-separated
  ;; component language is more valuable than global ranking.
  (setf (alist-get 'command completion-category-overrides)
        '((styles hotfuzz basic))
        (alist-get 'my/project-file completion-category-overrides)
        '((styles hotfuzz basic)))
  (my/command-palette--apply-faces))

(after! yasnippet
  ;; Yas enables globally after Doom's first buffer, after the early UI cache
  ;; is built.  Refresh on the actual state transition instead of guessing at
  ;; startup timing; the timer coalesces the per-buffer minor-mode hooks.
  (add-hook 'yas-global-mode-hook
            #'my/command-palette--schedule-active-editor-warmup)
  (add-hook 'yas-minor-mode-hook
            #'my/command-palette--schedule-active-editor-warmup)
  ;; If Yas loaded before this presentation module, its enabling hooks have
  ;; already fired; refresh once for that existing state as well.
  (my/command-palette--schedule-active-editor-warmup))

(after! git-commit
  ;; This global mode is enabled lazily on the first file and changes Emacs 31's
  ;; mode-sensitive M-x predicate just like Yasnippet does.
  (add-hook 'global-git-commit-mode-hook
            #'my/command-palette--schedule-active-editor-warmup)
  (my/command-palette--schedule-active-editor-warmup))

(unless (advice-member-p #'my/command-palette--read-command-a
                         'read-extended-command-1)
  (advice-add 'read-extended-command-1
              :around #'my/command-palette--read-command-a))

(unless (advice-member-p #'my/command-palette--completing-read-a
                         'completing-read)
  (advice-add 'completing-read
              :around #'my/command-palette--completing-read-a))

;; Loading Vertico's child-frame adapter during otherwise idle UI startup keeps
;; the first invocation just as immediate as every subsequent one.
(add-hook 'doom-init-ui-hook #'my/command-palette--warm-up)
(add-hook 'doom-first-buffer-hook
          #'my/command-palette--schedule-command-warmup 120)
(add-hook 'doom-first-file-hook
          #'my/command-palette--schedule-command-warmup 120)
(add-hook 'after-change-major-mode-hook
          #'my/command-palette--schedule-command-warmup)
(add-hook 'after-load-functions
          #'my/command-palette--invalidate-command-cache)

;; Evaluating new command definitions does not run `after-load-functions'.
;; Invalidate after the two primitive user-evaluation entry points as well.
(dolist (fn '(eval-buffer eval-region))
  (unless (advice-member-p #'my/command-palette--invalidate-command-cache fn)
    (advice-add fn :after #'my/command-palette--invalidate-command-cache)))
