;;; lisp/doom-themes-ext-treemacs.el --- ... -*- lexical-binding: t; no-byte-compile: t -*-
;;
;; Copyright (C) 2018-2022 Henrik Lissner
;;
;; Author: Henrik Lissner <contact@henrik.io>
;; Maintainer: Henrik Lissner <contact@henrik.io>
;; Created: July 10, 2018
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;; Code:

(defgroup doom-themes-treemacs nil
  "Options for doom's treemacs theme."
  :group 'doom-themes)


;;
;;; Variables

(defcustom doom-themes-treemacs-enable-variable-pitch t
  "If non-nil, remap file, folder & project labels to `variable-pitch'.

See `doom-themes-treemacs-variable-pitch-face'."
  :type 'boolean
  :group 'doom-themes-treemacs)

(defcustom doom-themes-treemacs-line-spacing 1
  "Line-spacing for treemacs buffer."
  :type 'integer
  :group 'doom-themes-treemacs)

(defcustom doom-themes-treemacs-theme "my-treemacs-icon-theme"
  "Default treemacs theme."
  :type '(radio (const :doc "A minimalistic icon theme" "my-treemacs-icon-theme"))
  :group 'doom-themes-treemacs)

(defcustom doom-themes-treemacs-bitmap-indicator-width 3
  "Default treemacs bitmap indicators width."
  :type 'integer
  :group 'doom-themes-treemacs)

(defcustom doom-themes-treemacs-variable-pitch-face 'variable-pitch
  "The face to remap file/directory labels to.

Only takes effect if `doom-themes-treemacs-enable-variable-pitch' is non-nil."
  :type 'face
  :group 'doom-themes-treemacs)


;;
;;; Faces
(defface doom-themes-treemacs-root-face
  '((t (:inherit font-lock-string-face)))
  "Face used for the root icon in doom themes' treemacs theme."
  :group 'doom-themes-treemacs)

(defface doom-themes-treemacs-file-face
  '((t (:inherit font-lock-doc-face :slant normal)))
  "Face used for the directory and file icons in doom themes' treemacs theme."
  :group 'doom-themes-treemacs)

;;
;;; Library

(defun doom-themes-hide-fringes-maybe (&rest _)
  "Remove fringes in current window if `treemacs-fringe-indicator-mode' is nil"
  (when (display-graphic-p)
    (if treemacs-fringe-indicator-mode
        (set-window-fringes nil doom-themes-treemacs-bitmap-indicator-width 0)
      (set-window-fringes nil 0 0))))

(defun doom-themes-setup-tab-width (&rest _)
  "Set `tab-width' to 1, so tab characters don't ruin formatting."
  (setq tab-width 1))

(defun doom-themes-define-treemacs-fringe-indicator-bitmap ()
  "Defines `treemacs--fringe-indicator-bitmap'"
  (if (fboundp 'define-fringe-bitmap)
      (define-fringe-bitmap 'treemacs--fringe-indicator-bitmap
        (make-vector 26 #b111) nil doom-themes-treemacs-bitmap-indicator-width)))

(defun doom-themes-setup-line-spacing ()
  "Set `line-spacing' in treemacs buffers."
  (setq line-spacing doom-themes-treemacs-line-spacing))

(defun doom-themes-hide-modeline ()
  (setq mode-line-format nil))

(defun doom-themes-enable-treemacs-variable-pitch-labels (&rest _)
  (when doom-themes-treemacs-enable-variable-pitch
    (dolist (face '(treemacs-root-face
                    treemacs-git-unmodified-face
                    treemacs-git-modified-face
                    treemacs-git-renamed-face
                    treemacs-git-ignored-face
                    treemacs-git-untracked-face
                    treemacs-git-added-face
                    treemacs-git-conflict-face
                    treemacs-directory-face
                    treemacs-directory-collapsed-face
                    treemacs-file-face
                    treemacs-tags-face))
      (let ((faces (face-attribute face :inherit nil)))
        (set-face-attribute
         face nil :inherit
         `(,doom-themes-treemacs-variable-pitch-face
           ,@(delq 'unspecified (if (listp faces) faces (list faces)))))))))

(defun doom-themes-fix-treemacs-icons-dired-mode ()
  "Set `tab-width' to 1 in dired-mode if `treemacs-icons-dired-mode' is active."
  (funcall (if treemacs-icons-dired-mode #'add-hook #'remove-hook)
           'dired-mode-hook
           #'doom-themes-setup-tab-width))

;;
;;; Bootstrap

(with-eval-after-load 'treemacs
  (unless (require 'nerd-icons nil t)
    (error "nerd-icons isn't installed"))

  (add-hook 'treemacs-mode-hook #'doom-themes-setup-tab-width)
  (add-hook 'treemacs-mode-hook #'doom-themes-setup-line-spacing)
  (add-hook 'treemacs-mode-hook #'doom-themes-define-treemacs-fringe-indicator-bitmap)

  ;; Fix #293: tabs messing up formatting in `treemacs-icons-dired-mode'
  (add-hook 'treemacs-icons-dired-mode-hook #'doom-themes-fix-treemacs-icons-dired-mode)

  ;; The modeline isn't useful in treemacs
  (add-hook 'treemacs-mode-hook #'doom-themes-hide-modeline)

  ;; Disable fringes (and reset them everytime treemacs is selected because it
  ;; may change due to outside factors)
  (add-hook 'treemacs-mode-hook #'doom-themes-hide-fringes-maybe)
  (advice-add #'treemacs-select-window :after #'doom-themes-hide-fringes-maybe)

  ;; variable-pitch labels for files/folders
  (doom-themes-enable-treemacs-variable-pitch-labels)
  (advice-add #'load-theme :after #'doom-themes-enable-treemacs-variable-pitch-labels)

  ;; minimalistic atom-inspired icon theme
  (let ((face-spec 'doom-themes-treemacs-file-face))
    (treemacs-create-theme "my-treemacs-icon-theme"
      :config
      (progn
        (treemacs-create-icon :icon (format " %s%s" (nerd-icons-faicon "nf-fa-folder_open"   :face 'treemacs-nerd-icons-root-face) treemacs-nerd-icons-tab)
                              :extensions (root-open)
                              :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s%s" (nerd-icons-faicon "nf-fa-folder"   :face 'treemacs-nerd-icons-root-face) treemacs-nerd-icons-tab)
                              :extensions (root-closed)
                              :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-folder_open"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                              :extensions (dir-open)
                              :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-folder"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                              :extensions (dir-closed)
                              :fallback 'same-as-icon)
        (dolist (item nerd-icons-extension-icon-alist)
          (let* ((extension (car item))
                 (func (cadr item))
                 (args (append (list (cadr (cdr item))) '(:v-adjust -0.04 :height 0.92) (cdr (cddr item))))
                 (icon (apply func args)))
            (let* ((icon-pair (cons (format "%s%s" icon treemacs-nerd-icons-tab) (format "%s%s" icon treemacs-nerd-icons-tab)))
                   (gui-icons (treemacs-theme->gui-icons treemacs--current-theme))
                   (tui-icons (treemacs-theme->tui-icons treemacs--current-theme))
                   (gui-icon  (car icon-pair))
                   (tui-icon  (cdr icon-pair)))
              (ht-set! gui-icons extension gui-icon)
              (ht-set! tui-icons extension tui-icon))))
        (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-file_o" :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                              :extensions (fallback)
                              :fallback 'same-as-icon))))

  (treemacs-load-theme doom-themes-treemacs-theme))

;;;###autoload
(defun my/doom-themes-treemacs-config ()
  "Install doom-themes' treemacs configuration.

Includes an Atom-esque icon theme and highlighting based on filetype.")

(provide 'doom-themes-ext-treemacs)
;;; doom-themes-treemacs.el ends here
