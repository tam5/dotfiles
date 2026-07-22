;;; lisp/ui-tweaks.el -*- lexical-binding: t; -*-
;;;
;;;

(require 'cl-lib)

(defvar diff-hl-side)
(defvar flycheck-indication-mode)

(defcustom my/native-frame-corner-radius 5.0
  "Native corner radius, in pixels, for polished Cocoa frames."
  :type 'number
  :group 'frames)

(defvar my/sidebar-font nil
  "The font to use for the sidebar.

I separate this from the doom-fonts just for clarity, as I'll be
slightly tweaking which fonts are used where.")

(defvar-local my/left-fringe-indicator-present-p nil)
(defvar-local my/left-fringe-state-initialized-p nil)

(defun my/native-frame-surface-color (&optional frame)
  "Return the active theme's darker UI surface color for FRAME."
  (let ((surface (and (facep 'solaire-default-face)
                      (face-attribute 'solaire-default-face
                                      :background frame t))))
    (if (and (stringp surface) (color-defined-p surface frame))
        surface
      (face-attribute 'default :background frame t))))

(defun my/apply-native-frame-polish (&optional frame)
  "Apply native titlebar color and restrained corners to Cocoa FRAME.

The C primitives deliberately change only AppKit presentation.  In
particular, they never mutate the `default' face or Emacs's text geometry."
  (let ((frame (or frame (selected-frame))))
    (when (and (frame-live-p frame)
               (not (frame-parent frame))
               (with-selected-frame frame
                 (and (display-graphic-p) (eq window-system 'ns))))
      (when (fboundp 'ns-set-titlebar-background-color)
        (ns-set-titlebar-background-color
         (my/native-frame-surface-color frame) frame))
      (when (fboundp 'ns-set-frame-corner-radius)
        (ns-set-frame-corner-radius my/native-frame-corner-radius frame))
      (my/titlebar-remove-title frame))))

(defun my/apply-native-frame-polish-to-all (&rest _)
  "Apply native frame polish to every top-level graphical frame."
  (dolist (frame (frame-list))
    (my/apply-native-frame-polish frame)))

(defun my/titlebar-remove-title (frame)
  "Set the titlebar title to an empty string.

For whatever reason the only way we got this to work was to first set the value
to something and then to reset it back to empty."
  (with-selected-frame frame
    (set-frame-name "_hack_")
    (set-frame-name "")))

(defun my/editor-window-p (window)
  "Return non-nil when WINDOW is an ordinary top-level editor window."
  (and (window-live-p window)
       (not (window-minibuffer-p window))
       (not (window-parameter window 'window-side))
       (not (frame-parent (window-frame window)))))

(defun my/overlay-uses-left-fringe-p (overlay)
  "Return non-nil when OVERLAY currently supplies a left-fringe marker."
  (or (and (overlay-get overlay 'diff-hl)
           (boundp 'diff-hl-side)
           (eq diff-hl-side 'left))
      (and (overlay-get overlay 'flycheck-overlay)
           (boundp 'flycheck-indication-mode)
           (eq flycheck-indication-mode 'left-fringe))
      (let ((before (overlay-get overlay 'before-string)))
        (and (stringp before)
             (> (length before) 0)
             (let ((position 0)
                   found)
               (while (and (< position (length before)) (not found))
                 (let ((display (get-text-property position 'display before)))
                   (setq found (and (consp display)
                                    (eq (car display) 'left-fringe))
                         position
                         (next-single-property-change
                          position 'display before (length before)))))
               found)))))

(defun my/scan-current-buffer-left-fringe-p ()
  "Return non-nil when the current buffer has a left-fringe indicator."
  (pcase-let ((`(,front . ,rear) (overlay-lists)))
    (or (cl-some #'my/overlay-uses-left-fringe-p front)
        (cl-some #'my/overlay-uses-left-fringe-p rear))))

(defun my/refresh-current-buffer-left-fringe-state (&rest _)
  "Cache whether the current buffer has a visible left-fringe indicator."
  (setq my/left-fringe-indicator-present-p
        (my/scan-current-buffer-left-fringe-p)
        my/left-fringe-state-initialized-p t))

(defun my/buffer-needs-left-fringe-p (buffer)
  "Return non-nil when BUFFER has anything visible in its left fringe."
  (with-current-buffer buffer
    (unless my/left-fringe-state-initialized-p
      (my/refresh-current-buffer-left-fringe-state))
    my/left-fringe-indicator-present-p))

(defun my/restore-window-left-fringe (window)
  "Restore WINDOW's left fringe if this configuration collapsed it."
  (when-let* ((saved (window-parameter window
                                       'my/saved-left-fringe-width)))
    (pcase-let ((`(,_left ,right ,outside ,_persistent)
                 (window-fringes window)))
      (set-window-fringes window saved right outside))
    (set-window-parameter window 'my/saved-left-fringe-width nil)))

(defun my/update-window-left-fringe (window)
  "Show WINDOW's left fringe only while it contains a visible indicator."
  (if (not (my/editor-window-p window))
      (my/restore-window-left-fringe window)
    (let* ((buffer (window-buffer window))
           (needed (my/buffer-needs-left-fringe-p buffer))
           (saved (window-parameter window 'my/saved-left-fringe-width)))
      (pcase-let ((`(,left ,right ,outside ,_persistent)
                   (window-fringes window)))
        (cond
         (needed
          (my/restore-window-left-fringe window))
         (saved
          ;; Flycheck can restore its buffer-local default behind our back.
          ;; Re-collapse it until an actual indicator is present.
          (unless (zerop left)
            (set-window-fringes window 0 right outside)))
         ((> left 0)
          (set-window-parameter window 'my/saved-left-fringe-width left)
          (set-window-fringes window 0 right outside)))))))

(defun my/update-window-line-number-margin (window)
  "Keep one character of breathing room after line numbers in WINDOW."
  (when (window-live-p window)
    (let* ((eligible (my/editor-window-p window))
           (numbered (and eligible
                          (buffer-local-value
                           'display-line-numbers (window-buffer window))))
           (owned (window-parameter window
                                    'my/owns-line-number-margin))
           (margins (window-margins window))
           (left (or (car margins) 0))
           (right (cdr margins)))
      (cond
       (numbered
        (when (< left 1)
          (set-window-margins window 1 right)
          (set-window-parameter window 'my/owns-line-number-margin t)))
       (owned
        (when (= left 1)
          (set-window-margins window 0 right))
        (set-window-parameter window 'my/owns-line-number-margin nil))))))

(defun my/update-editor-window-layout (window)
  "Apply context-sensitive fringe and line-number spacing to WINDOW."
  (when (window-live-p window)
    (my/update-window-left-fringe window)
    (my/update-window-line-number-margin window)))

(defun my/update-editor-frame-layout (&optional frame)
  "Apply editor window spacing to every live window on FRAME."
  (let ((frame (if (frame-live-p frame) frame (selected-frame))))
    (dolist (window (window-list frame 'no-minibuffer))
      (my/update-editor-window-layout window))))

(defun my/update-current-buffer-editor-windows (&rest _)
  "Refresh spacing in all windows currently displaying this buffer."
  ;; Overlay scans happen only when a buffer is first shown or a provider such
  ;; as Diff-HL/Flycheck reports a change, never on every redisplay/window hop.
  (my/refresh-current-buffer-left-fringe-state)
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (my/update-editor-window-layout window)))

(defun my/hide-all-child-frames ()
  "Hide all child frames of the current frame."
  (interactive)
  (let ((parent-frame (selected-frame)))
    (dolist (frame (frame-list))
      (when (eq (frame-parent frame) parent-frame)
        (make-frame-invisible frame t)))))

(defun my/hide-fringes (&rest _)
  "Helper function to hide fringes in the current window.

Mostly for disabling fringes in the treemacs sidebar."
  (when (display-graphic-p)
    (set-window-fringes nil 0 0)))

;;;###autoload
(defun my/toggle-frame-decoration ()
  "Toggle the decoration of the current frame."
  (interactive)
  (let ((current-setting (frame-parameter nil 'undecorated)))
    (set-frame-parameter nil 'undecorated (not current-setting)))
  (my/hide-all-child-frames))
