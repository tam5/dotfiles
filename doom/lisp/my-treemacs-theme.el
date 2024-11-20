;;; lisp/my-treemacs-theme.el -*- lexical-binding: t; -*-

(require 'nerd-icons)
(require 'treemacs)

(defface my/treemacs-theme-dir-icon-face
  '((t (:inherit nil)))
  "Face used for the directory and file icons in nerd-icons theme."
  :group 'treemacs-faces)

(defface my/treemacs-theme-file-icon-face
  '((t (:inherit nil)))
  "Face used for the directory and file icons in nerd-icons theme."
  :group 'treemacs-faces)

(defvar my/treemacs-theme-dir-spacer--left (propertize " " 'display '(space :width 0)))
(defvar my/treemacs-theme-dir-spacer--right (propertize " " 'display '(space :width 1.2)))
(defvar my/treemacs-theme-icon-spacer--right (propertize " " 'display '(space :width 1.4)))
(defvar my/treemacs-theme-icon-spacer--left (propertize " " 'display '(space :width 0.2)))

(defun my/treemacs-theme-hide-fringes-maybe (&rest _)
  "Remove fringes in current window if `treemacs-fringe-indicator-mode' is nil"
  (when (display-graphic-p)
    (if treemacs-fringe-indicator-mode
        (set-window-fringes nil 3 0)
      (set-window-fringes nil 0 0))))

(treemacs-create-theme "my/treemacs-theme"
  :config
  (progn
    (dolist (item nerd-icons-extension-icon-alist)
      (let* ((extension (car item))
             (func (cadr item))
             (args (append (list (cadr (cdr item))) '(:v-adjust -0.04 :height 0.92) (cdr (cddr item))))
             (icon (apply func args)))
        (let* ((icon-pair (cons (format "%s%s%s" my/treemacs-theme-icon-spacer--left icon my/treemacs-theme-icon-spacer--right) (format "%s%s%s" my/treemacs-theme-icon-spacer--left icon my/treemacs-theme-icon-spacer--right)))
               (gui-icons (treemacs-theme->gui-icons treemacs--current-theme))
               (tui-icons (treemacs-theme->tui-icons treemacs--current-theme))
               (gui-icon  (car icon-pair))
               (tui-icon  (cdr icon-pair)))
          (ht-set! gui-icons extension gui-icon)
          (ht-set! tui-icons extension tui-icon))))

    ;; directory and other icons
    (treemacs-create-icon :icon (format "%s%s%s" my/treemacs-theme-dir-spacer--left " " my/treemacs-theme-dir-spacer--right)
                          :extensions (root-open dir-open)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" my/treemacs-theme-dir-spacer--left (nerd-icons-faicon "nf-fa-folder" :v-adjust -0.06 :height 1.12 :face 'my/treemacs-theme-dir-icon-face) my/treemacs-theme-dir-spacer--right)
                          :extensions (root-closed dir-closed)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" my/treemacs-theme-icon-spacer--left (nerd-icons-faicon "nf-fa-file_o" :face 'my/treemacs-theme-file-icon-face) my/treemacs-theme-icon-spacer--right)
                          :extensions (fallback)
                          :fallback 'same-as-icon)))


(defun my/treemacs-theme-reload ()
  ""
  (interactive)
  ;; TODO
  ;;
  ;; deal with hl-line
  ;; this one turns off the hl-line in the treemacs (assuming we have solaire on)
  ;; (set-face-attribute 'solaire-hl-line-face nil :background nil)
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
    (set-face-attribute face nil :font (font-spec :family "Inter 1.4" :size 12.0 :weight 'medium)))
  ;;

  (treemacs-load-theme "my/treemacs-theme"))

(set-face-attribute 'treemacs-root-face nil :height 0.5)

(provide 'my/treemacs-theme)
;;; my/treemacs-theme.el ends here

