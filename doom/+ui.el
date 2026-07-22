;;; $DOOMDIR/+ui.el -*- lexical-binding: t; -*-


(load! "lisp/bitmaps")
(load! "lisp/ui-tweaks")
(load! "lisp/command-palette")

;; Cocoa can allocate a native horizontal scroller when a child frame changes
;; the active minibuffer geometry, even though no editor window requested one.
;; Disable it at the frame default as well as for the initial frame, so the
;; palette never leaves a resize-looking strip below the mode line.
(add-to-list 'default-frame-alist '(horizontal-scroll-bars . nil))
(horizontal-scroll-bar-mode -1)
(dolist (frame (frame-list))
  (set-frame-parameter frame 'horizontal-scroll-bars nil))


;; ideas for making corfu better:
;; - it shows a lot of irrelevant stuff
;; - some bg with bg colors maybe, at least on the tooltop one
;; - also seems to be some bug with backspace when we are using the preselect first

;; margins?
;;
;; (after! corfu

;;   ;; NOTE: this relies on our custom patch to emacs
;;   (add-to-list 'corfu--frame-parameters '(has-shadow . t))

;;   ;; (setf (alist-get 'child-frame-border-width corfu--frame-parameters) 20)
;;   ;; (delete-frame corfu--frame)

;;   ;; make it wider so there's less jumping around
;;   (setq corfu-min-width 40
;;         corfu-preselect 'first))

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

(add-to-list 'default-frame-alist '(internal-border-width . 0))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))


;;;
;;; Titlebar
;;; -----------------------------------------------------------------------------

(setq ns-use-proxy-icon nil)

(add-hook 'after-make-frame-functions #'my/apply-native-frame-polish)
(add-hook 'window-size-change-functions #'my/apply-native-frame-polish)
(add-hook 'doom-load-theme-hook #'my/apply-native-frame-polish-to-all)
(add-hook 'doom-init-ui-hook #'my/apply-native-frame-polish-to-all t)


;;;
;;; Sidebar
;;; -----------------------------------------------------------------------------

(defun my/treemacs-hide-mode-line ()
  "Hide the mode line in the current Treemacs buffer."
  (setq-local mode-line-format nil))

(defun my/treemacs-add-top-padding ()
  "Add a compact, theme-native spacer above the first Treemacs node."
  ;; Replace this one buffer-local remap instead of stacking relative remaps
  ;; every time Treemacs is reopened or the config is evaluated.
  (setf (alist-get 'header-line face-remapping-alist)
        '((:inherit solaire-default-face :height 0.45
           :box nil :overline nil :underline nil)
          header-line))
  (setq-local header-line-format " "))

(defun my/treemacs-refresh-fringe-indicator ()
  "Move the Treemacs selection marker flush to the outer frame edge."
  (when (overlayp treemacs--fringe-indicator-overlay)
    (overlay-put
     treemacs--fringe-indicator-overlay 'before-string
     (propertize
      " " 'display
      '(left-fringe my/treemacs-fringe-indicator-edge
                    treemacs-fringe-indicator-face)))))

(after! (treemacs treemacs-nerd-icons)
  ;; Keep Treemacs lazy.  Loading this at the top level would initialize
  ;; Treemacs before Doom applies its theme, which then resets our font faces.
  (load! "lisp/my-treemacs-theme")

  (setq treemacs-collapse-dirs 0
        treemacs-git-mode nil
        treemacs--fringe-indicator-bitmap
        'my/treemacs-fringe-indicator-edge
        treemacs--fringe-overlay-before-string
        (propertize
         " " 'display
         '(left-fringe my/treemacs-fringe-indicator-edge
                       treemacs-fringe-indicator-face)))

  ;; `hide-mode-line-mode' used to come from a Doom-managed package, but Doom
  ;; 2.2 no longer installs it.  Treemacs only needs a buffer-local nil modeline,
  ;; so avoid making its startup depend on an optional package.
  (add-hook 'treemacs-mode-hook #'my/treemacs-hide-mode-line)
  (add-hook 'treemacs-mode-hook #'my/treemacs-add-top-padding)
  (add-hook 'treemacs-mode-hook #'my/treemacs-refresh-fringe-indicator)

  ;; Disable fringes (and reset them everytime treemacs is selected because it
  ;; may change due to outside factors)
  ;; (setq treemacs-fringe-indicator-mode nil)
  ;; (add-hook 'treemacs-mode-hook #'my/hide-fringes)
  ;; (advice-add #'treemacs-select-window :after #'my/hide-fringes)

  (treemacs-load-theme "my/treemacs-theme")
  (my/treemacs-theme-highlight-current-mode t)

  ;; `after!' also runs immediately when this file is evaluated in a live
  ;; session, so refresh buffers whose overlay predates the new bitmap.
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'treemacs-mode)
        (my/treemacs-add-top-padding)
        (my/treemacs-refresh-fringe-indicator)))))


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

(add-hook 'window-state-change-functions #'my/update-editor-frame-layout)
(add-hook 'display-line-numbers-mode-hook
          #'my/update-current-buffer-editor-windows)

(after! diff-hl
  (dolist (function '(diff-hl--update-overlays diff-hl-remove-overlays))
    (unless (advice-member-p #'my/update-current-buffer-editor-windows
                             function)
      (advice-add function :after
                  #'my/update-current-buffer-editor-windows))))

(after! flycheck
  (add-hook 'flycheck-mode-hook #'my/update-current-buffer-editor-windows)
  (add-hook 'flycheck-after-syntax-check-hook
            #'my/update-current-buffer-editor-windows))

(add-hook 'doom-init-ui-hook #'my/update-editor-frame-layout t)


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
