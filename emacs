;;; ---------------------
;;; Packages
;;; --------------------

;: Load the package manager
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)

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
  :ensure t)
 
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
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#ffeeee" "#f36c60" "#c3e88d" "#ffcb6b" "##7eaaff" "#c792ea" "#89ddff" "#263238"))
 '(custom-safe-themes
   '("6114c5e1a3868881f51883f662d4e80fcdf6ac518839f6848cbb0d874173251d" "410007ffcf7443add948c2238378e73920c3ed087ab7ead982acc79f37aa738a" "08b704cd251b018b46d5a810ce92ebe6a83ea1d970b00ad483678e3de292db10" "26698f24e4debf3e3a3c25e8dc8e5728717c51b07f7828125aadf71daa6513a4" "282111376267b07b0b3d0cecb72d62407ec0cc9def168fc980f83c60c752dcac" "97042bdc26170810b1b7a23b5c83cfa50938a6861a4b676677b71948a70107f9" "cbd06a341e21215f9723191271f77203b13904923fb87d47db4876de8617fb50" "8c57626f1993d51c4162aaec53173c5a16d5250c6b2c53f31eb20bb9b1660f9e" "c60fb6a33e546697031900287685b9616b6e7b23323013f4ffa17a8b74f19656" "3f3377058788131a82f546b6859edd441a7e481404cc6a41961a82384b816bb3" "62aefd034c2d6c1619f724c4259075185772364adf2e0be1e853f6f58f4a536e" "cf2df1b2ed60eabc8d64d017f60a8502c30d576b5de2dcec15489e1402b340a7" "5c784a12823dc9da41c7c523dcc2d420579e3d2780bce3538fc7d15f8f3f3ccc" "ab70e2f9837ba2ef3a9ca1337860e433caa75154bbd939c8f2f51229f95bec3b" "7a70d9fb03ce73d9e12f182f001bd86790e2ffeee2d169b83828e97f57330ab4" "dd35d9110271d3e5b19c84846ece05c67d2c0c2489474bc1fdbb7538fbcfb6b3" "0a3ddd053d5c13215218b891d65445163e00605653a2c446c69e0df7b9647c30" "23b256fc4a4ad477d703932cb46f18d1475863d068f800f24d4b1f40412d9d81" "a00b60cdf15c937841f1bca708510023f2bce17a23766169bffcaee182da8f75" "4c8ac363978b73a5ad796a258bb3dc8439f7cc56d0336ca018c558ca075c00e6" default))
 '(fci-rule-color "#ff00ff")
 '(hl-sexp-background-color "#ff00ff")
 '(package-selected-packages
   '(rainbow-mode php-mode evil-leader helm-ag helm-projectile projectile helm-ls-git use-package))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#f36c60")
     (40 . "#f78a64")
     (60 . "#ffcb6b")
     (80 . "#c3e88d")
     (100 . "#89ddff")
     (120 . "##7eaaff")
     (140 . "#c792ea")
     (160 . "#f36c60")
     (180 . "#f78a64")
     (200 . "#ffcb6b")
     (220 . "#c3e88d")
     (240 . "#89ddff")
     (260 . "##7eaaff")
     (280 . "#c792ea")
     (300 . "#f36c60")
     (320 . "#f78a64")
     (340 . "#ffcb6b")
     (360 . "#c3e88d")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
