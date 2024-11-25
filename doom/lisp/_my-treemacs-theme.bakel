;;; lisp/my-treemacs-theme.el -*- lexical-binding: t; -*-

(require 'dash)
(require 'treemacs)
(require 'nerd-icons)

(defface my/treemacs-theme-dir-icon-face
  '((t (:inherit nil)))
  "Face used for the directory and file icons in nerd-icons theme."
  :group 'treemacs-faces)

(defface my/treemacs-theme-file-icon-face
  '((t (:inherit nil)))
  "Face used for the directory and file icons in nerd-icons theme."
  :group 'treemacs-faces)

(defface my/treemacs-theme-current-file-face
  '((t (:inherit treemacs-file-face)))
  "Face used for the directory and file icons in nerd-icons theme."
  :group 'treemacs-faces)

(defface my/treemacs-theme-current-file-icon-face
  '((t (:inherit treemacs-file-face)))
  "Face used for the directory and file icons in nerd-icons theme."
  :group 'treemacs-faces)

(defvar my/treemacs-theme-padding--left (propertize " " 'display '(space :width 2)))
(defvar my/treemacs-theme-dir-spacer--left (propertize " " 'display '(space :width 0)))
(defvar my/treemacs-theme-dir-spacer--right (propertize " " 'display '(space :width 1.4)))
(defvar my/treemacs-theme-icon-spacer--right (propertize " " 'display '(space :width 1.6)))
(defvar my/treemacs-theme-icon-spacer--left (propertize " " 'display '(space :width 0.2)))

(defun my/treemacs-theme-hide-fringes (&rest _)
  "Remove fringes in treemacs window, mostly since it makes hl-line look bad."
  (when (display-graphic-p)
    (set-window-fringes nil 0 0)))

(treemacs-create-theme "my/treemacs-theme"
  :config
  (progn
    ;; directory icons
    (treemacs-create-icon :icon (format "%s%s%s%s"
                                        my/treemacs-theme-padding--left
                                        my/treemacs-theme-dir-spacer--left
                                        (nerd-icons-faicon "nf-fa-folder_o" :v-adjust -0.06 :height 1.12 :face 'my/treemacs-theme-dir-icon-face)
                                        my/treemacs-theme-dir-spacer--right)
                          :extensions (root-open dir-open)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s%s"
                                        my/treemacs-theme-padding--left
                                        my/treemacs-theme-dir-spacer--left
                                        (nerd-icons-faicon "nf-fa-folder" :v-adjust -0.06 :height 1.12 :face 'my/treemacs-theme-dir-icon-face)
                                        my/treemacs-theme-dir-spacer--right)
                          :extensions (root-closed dir-closed)
                          :fallback 'same-as-icon)
    ;; file icons
    (dolist (item nerd-icons-extension-icon-alist)
      (let* ((extension (car item))
             (func (cadr item))
             (args (append (list (cadr (cdr item))) '(:v-adjust -0.04 :height 0.92) (cdr (cddr item))))
             (icon (apply func args)))
        (let* ((icon-pair (cons (format "%s%s%s%s" my/treemacs-theme-padding--left my/treemacs-theme-icon-spacer--left icon my/treemacs-theme-icon-spacer--right)
                                (format "%s%s%s%s" my/treemacs-theme-padding--left my/treemacs-theme-icon-spacer--left icon my/treemacs-theme-icon-spacer--right)))
               (gui-icons (treemacs-theme->gui-icons treemacs--current-theme))
               (tui-icons (treemacs-theme->tui-icons treemacs--current-theme))
               (gui-icon  (car icon-pair))
               (tui-icon  (cdr icon-pair)))
          (ht-set! gui-icons extension gui-icon)
          (ht-set! tui-icons extension tui-icon))))
    ;; fallback
    (treemacs-create-icon :icon (format "%s%s%s%s"
                                        my/treemacs-theme-padding--left
                                        my/treemacs-theme-icon-spacer--left
                                        (nerd-icons-faicon "nf-fa-file_o" :face 'my/treemacs-theme-file-icon-face)
                                        my/treemacs-theme-icon-spacer--right)
                          :extensions (fallback)
                          :fallback 'same-as-icon)))



;;; WIP - fringe indicator ->
;;; this gets the fringe indicator all the way to the left, but i want it to stay on the selected file
;;; and not follow my cursor
;;; also it does look like the fringes get messed up all the time so we'll need to add that hook to make
;;; sure it keeps getting reset properly
;;; and then i also need to do something different with the cursor
;;; check 'treemacs-show-cursor'
;; (defun doom-themes-define-treemacs-fringe-indicator-bitmap ()
;;   "Defines `treemacs--fringe-indicator-bitmap'"
;;   (if (fboundp 'define-fringe-bitmap)
;;       (define-fringe-bitmap 'treemacs--fringe-indicator-bitmap
;;         (make-vector 26 #b111) nil 3)))

;; (add-hook 'treemacs-mode-hook #'doom-themes-define-treemacs-fringe-indicator-bitmap)

;; (setq treemacs--fringe-indicator-bitmap
;;       (define-fringe-bitmap 'treemacs--fringe-indicator-bitmap-default (make-vector 200 #b11100000)))

;;; WIP



;; (set-frame-parameter nil 'cursor-color "orange")
;; (set-face-attribute 'cursor nil :background "red" :foreground "yellow")

;; (defun my/set-cursor-color-for-treemacs ()
;;   "Set a specific cursor color in treemacs-mode."
;;   (setq-local cursor-type nil))

;; (add-hook 'treemacs-mode-hook #'my/set-cursor-color-for-treemacs)

;; (map!  :g "M-i" #'aritest)
;; (map!  :g "M-u" #'aritest)

;; (defun aritest ()
;;   (interactive)
;;   (setq cursor-type nil))


(defun my/treemacs-theme-reload ()
  ""
  (interactive)
  ;; TODO
  ;;
  ;; deal with hl-line
  ;; this one turns off the hl-line in the treemacs (assuming we have solaire on)
  ;; (set-face-attribute 'solaire-hl-line-face nil :background (face-attribute 'solaire-default-face :background))
  ;; problem though is i do want the hl-line when my cursor is in there, just not when i'm in reg buf

  ;; treemacs-hl-line-face
  ;;
  ;; TEMP - set all the faces to the right font and size,
  ;; there is something wrong with doom forcing treemacs git mode to be on
  ;; so we have to override it
  ;;
  ;; check on the root face and spacing
  ;; and the colors of the icons
  ;; add font to dotfiles
  ;; fringe indicator / hl-lien
  ;;
  ;; we don't need the echo messages of "written"
  (dolist (face '(treemacs-directory-face
                  treemacs-directory-collapsed-face
                  treemacs-file-face
                  treemacs-root-face
                  treemacs-git-conflict-face
                  treemacs-git-added-face
                  treemacs-git-untracked-face
                  treemacs-git-ignored-face
                  treemacs-git-renamed-face
                  treemacs-git-modified-face
                  treemacs-git-unmodified-face))
    (set-face-attribute face nil :font (font-spec :family "Inter 1.5" :size 12.0 :weight 'medium)))
  ;;

  (treemacs-load-theme "my/treemacs-theme"))

(provide 'my/treemacs-theme)
;;; my/treemacs-theme.el ends here




























(defvar-local my/treemacs-theme-current-file-overlay nil
  "Overlay used to highlight the current Treemacs node.")

(defvar-local my/treemacs-theme-current-file-icon-overlay nil
  "Overlay used to highlight the current Treemacs node.")

(defvar my-treemacs-theme-parent-node-overlays nil
  "List of overlays applied to parent nodes for cleanup.")

(defun my/treemacs-theme-real-buffer-p ()
  "Return t if the current buffer is a real (file-visiting) buffer.
Copied the logic from `solaire-mode-real-buffer-p`."
  (buffer-file-name (buffer-base-buffer)))

(defun my/treemacs-theme-current-highlight-reset ()
  "Reset all current Treemacs highlights."
  ;; Clear current file overlays
  (when my/treemacs-theme-current-file-overlay
    (delete-overlay my/treemacs-theme-current-file-overlay)
    (setq my/treemacs-theme-current-file-overlay nil))
  (when my/treemacs-theme-current-file-icon-overlay
    (delete-overlay my/treemacs-theme-current-file-icon-overlay)
    (setq my/treemacs-theme-current-file-icon-overlay nil))
  ;; Clear parent node overlays
  (mapc 'delete-overlay my-treemacs-theme-parent-node-overlays)
  (setq my-treemacs-theme-parent-node-overlays nil))

(defface my/treemacs-theme-parent-node-face
  '((t (:inherit nil)))
  "Face used for the directory and file icons in nerd-icons theme."
  :group 'treemacs-faces)

(set-face-attribute 'my/treemacs-theme-parent-node-face nil :foreground "orange")

(defun my/treemacs-theme-highlight-current-file (file)
  "Highlight the filename of the Treemacs node corresponding to `file` and its parent nodes."
  (with-current-buffer (treemacs-get-local-buffer)
    (my/treemacs-theme-current-highlight-reset)
    (-when-let* ((btn (ignore-errors (treemacs-find-visible-node file)))
                 (label-start (treemacs-button-start btn))
                 (label-end (treemacs-button-end btn))
                 (icon-start (- label-start 2))
                 (icon-end (1- label-start))

                 (icon-face (get-text-property icon-start 'face))
                 (icon-fg (get-effective-foreground-color-from-face icon-face))
                 (brighter-fg (my-brighten-color icon-fg)))
      (treemacs-with-writable-buffer
       ;; Highlight the current file
       (setq my/treemacs-theme-current-file-overlay (make-overlay label-start label-end))
       (overlay-put my/treemacs-theme-current-file-overlay 'face 'my/treemacs-theme-current-file-face)

       (setq my/treemacs-theme-current-file-icon-overlay (make-overlay icon-start icon-end))
       (overlay-put my/treemacs-theme-current-file-icon-overlay 'face `(:foreground ,brighter-fg))

       ;; Highlight all visible parent nodes
       (let ((current-btn btn))
         (while (setq current-btn (treemacs-button-get current-btn :parent))
           (let* ((parent-label-start (treemacs-button-start current-btn))
                  (parent-label-end (treemacs-button-end current-btn))
                  ;; Overlay for the parent node
                  (parent-overlay (make-overlay parent-label-start parent-label-end)))
             (overlay-put parent-overlay 'face 'my/treemacs-theme-parent-node-face)
             ;; Save the overlay for cleanup
             (push parent-overlay my-treemacs-theme-parent-node-overlays))))))))

(defun my-brighten-color (color)
  "Return a brightened version of the foreground color for FACE."
  (when color
    (unless (eq 'unspecified color)
      (color-lighten-name color 10)))) ;; TODO factorize

(defun get-effective-foreground-color-from-face (face)
  "Get the effective foreground color of the text at point, resolving all faces and properties."
  (interactive)
  (cond
   ;; If there's a face, resolve the foreground color
   ((facep face)
    (face-attribute face :foreground nil t))
   ;; If it's a list of faces, resolve the first one with a foreground
   ((and (listp face) (not (stringp face)))
    (cl-some (lambda (f)
               (when (facep f)
                 (face-attribute f :foreground nil t)))
             face))
   ;; Fallback to default foreground if nothing is found
   (t (face-attribute 'default :foreground nil t))))

(defun my/treemacs-theme-update-current-highlight (&rest _)
  "Automatically highlight the file in Treemacs corresponding to the current window's buffer."
  ;; TODO this will need some debounce
  (when (and (bufferp (treemacs-get-local-buffer))
             (my/treemacs-theme-real-buffer-p))
    (let ((file (buffer-file-name (window-buffer))))
      (when file
        (my/treemacs-theme-highlight-current-file file)))))

(defun my/aritest-fix (&rest _)
  (let ((file (buffer-file-name (window-buffer (previous-window)))))
    (when file
      (my/treemacs-theme-highlight-current-file file))))

;; (defun my/enable-treemacs-auto-highlight ()
;;   "Enable automatic highlighting of the current file in Treemacs on window focus change."
;;   (add-hook 'window-selection-change-functions #'my/treemacs-theme-auto-highlight)
;;   )

;; (defun my/disable-treemacs-auto-highlight ()
;;   "Disable automatic highlighting of the current file in Treemacs."
;;   (remove-hook 'window-selection-change-functions #'my/treemacs-theme-auto-highlight))

;; ;; Bind this function to enable the automatic behavior
;; (map! :g "M-i" #'my/enable-treemacs-auto-highlight)

;; TODO - now we just got to get it to apply when we switch files and/or when we change the layout of treemacs like open/close nodes bc something might now be visible that wasn't
;; also we should always have visual line mode on

;; Add the hook to track window selection changes
;; (add-hook 'window-selection-change-functions #'my/on-cursor-move)


;; maybe add advice here to get it to trigger the refresh too
;; treemacs-do-for-button-state
