;;; lisp/my-treemacs-theme.el -*- lexical-binding: t; -*-

(require 'dash)
(require 'treemacs)
(require 'nerd-icons)

(defface my/treemacs-theme-file-name-face
  '((t (:inherit nil)))
  "Face used for the file names in my/treemacs-theme."
  :group 'treemacs-faces)

(defface my/treemacs-theme-dir-icon-face
  '((t (:inherit nil)))
  "Face used for the directory icons in my/treemacs-theme."
  :group 'treemacs-faces)

(defface my/treemacs-theme-dir-name-face
  '((t (:inherit nil)))
  "Face used for the directory names in my/treemacs-theme."
  :group 'treemacs-faces)

(defface my/treemacs-theme-dir-open-icon-face
  '((t (:inherit my/treemaacs-icon-theme-dir-icon-face)))
  "Face used for the open directory icons in my/treemacs-theme."
  :group 'treemacs-faces)

(defface my/treemacs-theme-dir-open-name-face
  '((t (:inherit my/treemacs-theme-dir-name-face)))
  "Face used for the open directory names in my/treemacs-theme."
  :group 'treemacs-faces)

(defface my/treemacs-theme-dir-closed-icon-face
  '((t (:inherit my/treemaacs-icon-theme-dir-icon-face)))
  "Face used for the closed directory icons in my/treemacs-theme."
  :group 'treemacs-faces)

(defface my/treemacs-theme-dir-closed-name-face
  '((t (:inherit my/treemacs-theme-dir-name-face)))
  "Face used for the closed directory names in my/treemacs-theme."
  :group 'treemacs-faces)

(defface my/treemacs-theme-current-file-name-face
  '((t (:inherit treemacs-file-face)))
  "Face used to highlgiht the file name of the currently selected file
when `my/treemacs-theme-highlight-current-mode` is enabled."
  :group 'treemacs-faces)

(defface my/treemacs-theme-current-file-icon-face
  '((t (:inherit treemacs-file-face)))
  "Face used to highlgiht the file icon of the currently selected file
when `my/treemacs-theme-highlight-current-mode` is enabled."
  :group 'treemacs-faces)

(defface my/treemacs-theme-current-dir-icon-face
  '((t (:inherit my/treemacs-theme-dir-icon-face)))
  "Face used to highlight directory icons containing the currently selected file
when `my/treemacs-theme-highlight-current-mode` is enabled."
  :group 'treemacs-faces)

(defface my/treemacs-theme-current-dir-name-face
  '((t (:inherit my/treemacs-theme-dir-name-face)))
  "Face used to highlight directory names containing the currently selected file
when `my/treemacs-theme-highlight-current-mode` is enabled."
  :group 'treemacs-faces)

(defvar-local my/treemacs-theme--current-file-name-overlay nil
  "Overlay used to highlgiht the file name of the currently selected file
when `my/treemacs-theme-highlight-current-mode` is enabled.")

(defvar-local my/treemacs-theme--current-file-icon-overlay nil
  "Overlay used to highlgiht the file icon of the currently selected file
when `my/treemacs-theme-highlight-current-mode` is enabled.")

(defvar-local my/treemacs-theme--current-dir-name-overlays nil
  "Overlays used to highlight directory names containing the currently selected
file when `my/treemacs-theme-highlight-current-mode` is enabled.")

(defvar-local my/treemacs-theme--current-dir-icon-overlays nil
  "Overlays used to highlight directory icons containing the currently selected
file when `my/treemacs-theme-highlight-current-mode` is enabled.")

;; (defvar my/treemacs-theme-padding-left 2
;;   "The amount of padding to the left of all treemacs nodes.

;; TODO: check what the :width can support here. ")

;; TODO: dynamically calculate these
(defvar my/treemacs-theme-padding--left (propertize " " 'display '(space :width 2)))
(defvar my/treemacs-theme-dir-spacer--left (propertize " " 'display '(space :width 0)))
(defvar my/treemacs-theme-dir-spacer--right (propertize " " 'display '(space :width 1.4)))
(defvar my/treemacs-theme-icon-spacer--right (propertize " " 'display '(space :width 1.6)))
(defvar my/treemacs-theme-icon-spacer--left (propertize " " 'display '(space :width 0.2)))

(defun my/treemacs-theme-real-buffer-p ()
  "Return t if the current buffer is a real (file-visiting) buffer.
Copied the logic from `solaire-mode-real-buffer-p`."
  (buffer-file-name (buffer-base-buffer)))

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



(provide 'my/treemacs-theme)
;;; my/treemacs-theme.el ends here




(defun my/treemacs-theme-current-highlight-clear ()
  "Clear all current node highlights."
  (when my/treemacs-theme--current-file-name-overlay
    (delete-overlay my/treemacs-theme--current-file-name-overlay)
    (setq my/treemacs-theme--current-file-name-overlay nil))
  (when my/treemacs-theme--current-file-icon-overlay
    (delete-overlay my/treemacs-theme--current-file-icon-overlay)
    (setq my/treemacs-theme--current-file-icon-overlay nil))
  (mapc 'delete-overlay my/treemacs-theme--current-dir-name-overlays)
  (setq my/treemacs-theme--current-dir-name-overlays nil))

;; (set-face-attribute 'my/treemacs-theme-parent-node-face nil :foreground "orange")

(defun my/treemacs-theme-highlight-current-file (file)
  "Highlight the filename of the Treemacs node corresponding to `file` and its parent nodes."
  (with-current-buffer (treemacs-get-local-buffer)
    (my/treemacs-theme-current-highlight-clear)
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
       (setq my/treemacs-theme--current-file-name-overlay (make-overlay label-start label-end))
       (overlay-put my/treemacs-theme--current-file-name-overlay 'face 'my/treemacs-theme-current-file-name-face)

       (setq my/treemacs-theme--current-file-icon-overlay (make-overlay icon-start icon-end))
       (overlay-put my/treemacs-theme--current-file-icon-overlay 'face `(:foreground ,brighter-fg))

       ;; Highlight all visible parent nodes
       (let ((current-btn btn))
         (while (setq current-btn (treemacs-button-get current-btn :parent))
           (let* ((parent-label-start (treemacs-button-start current-btn))
                  (parent-label-end (treemacs-button-end current-btn))
                  ;; Overlay for the parent node
                  (parent-overlay (make-overlay parent-label-start parent-label-end)))
             (overlay-put parent-overlay 'face 'my/treemacs-theme-parent-node-face)
             ;; Save the overlay for cleanup
             (push parent-overlay my/treemacs-theme--current-dir-name-overlays))))))))

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
  (when (and (bufferp (treemacs-get-local-buffer))
             (my/treemacs-theme-real-buffer-p))
    (let ((file (buffer-file-name (window-buffer))))
      (when file
        (my/treemacs-theme-highlight-current-file file)))))

(defun my/aritest-fix (&rest _)
  (let ((file (buffer-file-name (window-buffer (previous-window)))))
    (when file
      (my/treemacs-theme-highlight-current-file file))))

(defun my/treemacs-theme--enable-highlight-current-mode ()
  "Setup for `my/treemacs-theme-highlight-current-mode'."
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

  (set-face-attribute 'my/treemacs-theme-current-file-name-face nil :foreground "white")
  (set-face-attribute 'my/treemacs-theme-current-dir-name-face nil :foreground "orange")

  (add-hook 'buffer-list-update-hook #'my/treemacs-theme-update-current-highlight)
  (add-hook 'window-selection-change-functions #'my/treemacs-theme-update-current-highlight)
  (advice-add #'treemacs-RET-action :after #'my/aritest-fix))

(defun my/treemacs-theme--disable-highlight-current-mode ()
  "Tear-down for `my/treemacs-theme-highlight-current-mode'."
  (remove-hook 'buffer-list-update-hook #'my/treemacs-theme-update-current-highlight)
  (remove-hook 'window-selection-change-functions #'my/treemacs-theme-update-current-highlight)
  (advice-remove #'treemacs-RET-action #'my/aritest-fix))

;;;###autoload
(define-minor-mode my/treemacs-theme-highlight-current-mode
  "Minor mode to highlight the currently selected file in Treemacs."

  :init-value nil
  :global     t
  :lighter    nil
  :group      'treemacs
  (if my/treemacs-theme-highlight-current-mode
      (my/treemacs-theme--enable-highlight-current-mode)
    (my/treemacs-theme--disable-highlight-current-mode)))
