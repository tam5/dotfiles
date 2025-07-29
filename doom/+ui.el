;;; $DOOMDIR/+ui.el -*- lexical-binding: t; -*-


(load! "lisp/bitmaps")
(load! "lisp/ui-tweaks")
(load! "lisp/my-treemacs-theme")


;; ideas for making corfu better:
;; - it shows a lot of irrelevant stuff
;; - some bg with bg colors maybe, at least on the tooltop one
;; - also seems to be some bug with backspace when we are using the preselect first

;; margins?
;;
(after! corfu

  ;; NOTE: this relies on our custom patch to emacs
  (add-to-list 'corfu--frame-parameters '(has-shadow . t))

  ;; (setf (alist-get 'child-frame-border-width corfu--frame-parameters) 20)
  ;; (delete-frame corfu--frame)

  ;; make it wider so there's less jumping around
  (setq corfu-min-width 40
        corfu-preselect 'first))

;; corfu-quit-no-match t ;; not sure if this is doing anything

;; (set-face-attribute 'corfu-default nil
;;                     :foreground (face-attribute 'font-lock-comment-face :foreground)
;;                     ;; :background (face-attribute 'solaire-default-face :background)
;;                     :family "Arial"
;;                     :weight 'normal
;;                     :height 240
;;                     )

;; ;; (set-face-attribute 'corfu-default nil :font (font-spec :family "Inter 1.5" :size 14.0 :weight 'medium))

;; (set-face-attribute 'corfu-current nil
;;                     :foreground nil
;;                     :family "Arial"
;;                     :weight 'normal
;;                     :height 240
;;                     ;; :background (face-attribute 'solaire-hl-line-face :background)
;;                     )

;; (set-face-attribute 'corfu-bar nil :background "blue")
;; (set-face-attribute 'corfu-border nil :background nil)

;; (set-face-attribute 'orderless-match-face-0 nil :foreground "#79B8FF" :background nil :weight 'bold :family "Arial"
;;                     :height 240))

;; (setf (alist-get 'ns-appearance corfu--frame-parameters) 'light)
;;





;;;
;;; Frame
;;; -----------------------------------------------------------------------------

(setq default-frame-alist
      '((internal-border-width . 0)))


;;;
;;; Titlebar
;;; -----------------------------------------------------------------------------

(setq ns-use-proxy-icon nil)

(add-hook 'window-size-change-functions #'my/titlebar-remove-title)

(after! solaire-mode
  (add-hook 'doom-init-ui-hook #'my/titlebar-patch-color t)
  (add-hook 'solaire-global-mode-hook #'my/titlebar-patch-color t))


;;;
;;; Sidebar
;;; -----------------------------------------------------------------------------

(after! (treemacs treemacs-nerd-icons)
  (setq treemacs-collapse-dirs 0
        treemacs-git-mode nil)

  (add-hook 'treemacs-mode-hook #'hide-mode-line-mode)

  ;; Disable fringes (and reset them everytime treemacs is selected because it
  ;; may change due to outside factors)
  (setq treemacs-fringe-indicator-mode nil)
  (add-hook 'treemacs-mode-hook #'my/hide-fringes)
  (advice-add #'treemacs-select-window :after #'my/hide-fringes)

  (treemacs-load-theme "my/treemacs-theme")
  (my/treemacs-theme-highlight-current-mode t))


;;;
;;; Modeline WIP
;;; -----------------------------------------------------------------------------

(after! doom-modeline
  (doom-modeline-def-modeline 'aritest-line
    '(eldoc bar workspace-name window-number matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
    '(compilation objed-state misc-info persp-name grip irc mu4e gnus github debug repl lsp process check vcs))

  (setq doom-modeline-percent-position nil)

  (add-hook 'doom-modeline-mode-hook
            (lambda ()
              (doom-modeline-set-modeline 'aritest-line 'default))))

;;;
;;; Line numbers
;;; -----------------------------------------------------------------------------
(setq display-line-numbers-type t)


;;;
;;; Indent guides
;;; -----------------------------------------------------------------------------

(after! highlight-indent-guides
  (setq highlight-indent-guides-method 'bitmap
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-auto-character-face-perc 10
        highlight-indent-guides-auto-top-character-face-perc 20
        highlight-indent-guides-bitmap-function 'my/highlight-indent-guides--bitmap-dots)
  (highlight-indent-guides-mode -1))
;; (remove-hook! (prog-mode text-mode conf-mode) #'highlight-indent-guides-mode)


;;;
;;; IntelliSense
;;; -----------------------------------------------------------------------------
(after! lsp-ui
  (setq lsp-ui-sideline-enable nil))
