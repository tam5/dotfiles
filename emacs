;;; ---------------------
;;; Packages
;;; --------------------

;: Load the package manager
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
;; (package-initialize)

;; Install use-package if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Init evil
(use-package evil
  :ensure t
  :config
  (evil-mode t))

;; Init helm
(use-package helm
  :ensure t)

(global-set-key (kbd "M-x") 'helm-M-x)

(use-package material-theme :ensure t)

;;; ---------------------
;;; Visuals
;;; --------------------

;; Disable the menu bar
(menu-bar-mode -1)

;; Disable the scroll bar
(toggle-scroll-bar -1)

;; Disable the toolbar
(tool-bar-mode -1)

;; Set the font attributes
(add-to-list 'default-frame-alist '(font . "Operator Mono-15"))

;; Set the initial frame size to fill the screen
(add-to-list 'default-frame-alist '(height . 95))
(add-to-list 'default-frame-alist '(width . 362))


;;; ---------------------
;;; Other Stuff
;;; --------------------


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
