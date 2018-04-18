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

(use-package evil-leader
  :ensure t
  :config)
 
(global-evil-leader-mode)

(use-package projectile :ensure t)
(use-package helm-projectile :ensure t)

(global-set-key (kbd "M-x") 'helm-M-x)

(use-package material-theme :ensure t)

;; sidbar
(use-package neotree :ensure t)

;;; ---------------------
;;; Language modes
;;; --------------------
(use-package php-mode :ensure t)

;;; ---------------------
;;; Behavior
;;; --------------------
;; enable recent files
(recentf-mode 1)

;; always reload files from disk when they are changed
(global-auto-revert-mode t)

;; save backup files in a less annoying place
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; ---------------------
;;; Visuals
;;; --------------------

;; enable line numbers
(global-linum-mode t)
;; add some padding between line numbers and text
(setq-default left-fringe-width 20)

;; Disable the menu bar
(menu-bar-mode -1)

;; Disable the scroll bar
(toggle-scroll-bar -1)

;; Disable the toolbar
(tool-bar-mode -1)

;; Set the font attributes
(add-to-list 'default-frame-alist '(font . "Operator Mono-15"))
(setq-default line-spacing 5)

;; Set the initial frame size to fill the screen
(add-to-list 'default-frame-alist '(height . 90))
(add-to-list 'default-frame-alist '(width . 315))

;; Natural title bar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;;; ---------------------
;;; Key Bindings
;;; --------------------

(eval-after-load 'evil-maps
  '(define-key evil-normal-state-map (kbd "C-p") 'helm-projectile-find-file))

(eval-after-load 'evil-maps
  '(define-key evil-normal-state-map (kbd "SPC") 'evil-search-forward))

(evil-leader/set-leader ",")


(evil-leader/set-key
  "ev" (lambda () (interactive) (find-file (file-truename "~/.emacs")))
  "mru" 'recentf-open-files
  "1" 'neotree-toggle
  "2" 'neotree-find
  "x" 'comment-line
  "w" 'save-buffer
  "d" 'evil-delete-buffer
  "q" 'evil-quit)

;;; ---------------------
;;; Other Stuff
;;; --------------------


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(php-mode evil-leader helm-ag helm-projectile projectile helm-ls-git use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
