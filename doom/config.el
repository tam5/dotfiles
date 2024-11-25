;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


(load! "+ui")
(load! "+lang")
(load! "+keybinds")



;; (setq +popup-defaults
;;       (list :side   'right
;;             ;; :height 0.16
;;             :width  40
;;             :quit   t
;;             :select #'ignore
;;             :ttl    5))

;;;
;;; Core settings
;;; -----------------------------------------------------------------------------

(setq doom-theme 'doom-github-classic-dark

      doom-font (font-spec :family "MesloLGM Nerd Font" :size 14 :weight 'normal)
      my/sidebar-font (font-spec :family "Inter 1.5" :size 12.0 :weight 'medium)

      ;; doom-variable-pitch-font (font-spec :family "Monaco" :size 13)

      fancy-splash-image (file-name-concat doom-user-dir "rocket.png")

      user-full-name (getenv "MY_FULL_NAME")
      user-mail-address (getenv "MY_EMAIL_ADDRESS"))


;;;
;;; Modules
;;; -----------------------------------------------------------------------------

;;; :lang org
(setq org-directory "~/org/")

;;; :emacs dired
(setq dired-kill-when-opening-new-dired-buffer t)

;;; :ui workspaces
(after! (persp-mode recentf)
  ;; remove the persp autosave file from recent files list
  (add-to-list 'recentf-exclude
               (concat "^" (regexp-quote (file-truename persp-save-dir)))))

;;; :completion company
;; (after! company
;;   ;; i have no patience, i want company to show up immediately
;;   (setq company-idle-delay 0
;;         company-minimum-prefix-length 0
;;         ;; disabling this for now in markdown since i don't need autocomplete for english
;;         company-global-modes '(not markdown-mode)))

;;; :completion corfu
(after! corfu
  (setq corfu-auto-delay 0
        corfu-auto-prefix 0
        +corfu-want-tab-prefer-expand-snippets t))

;;; :completion vertico
(after! orderless
  (defun +vertico-orderless-dispatch-a (orig-fun &rest args)
    "Use flex matching by default. There's probably a more straightforward
way you're supposed to do this, but wtvr."
    (or (apply orig-fun args)
        (cons 'orderless-flex (car args))))
  (advice-add '+vertico-orderless-dispatch :around #'+vertico-orderless-dispatch-a))

;;; :tools lsp
(after! lsp-mode
  (setq ;; If an LSP server isn't present when I start a prog-mode buffer, you
   ;; don't need to tell me. I know. On some machines I don't care to have
   ;; a whole development environment for some ecosystems.
   lsp-enable-suggest-server-download nil))


;;;
;;; Misc
;;; -----------------------------------------------------------------------------

(setq confirm-kill-emacs nil)

;;; copilot
(use-package! copilot
  :defer t
  :hook (prog-mode . copilot-mode)
  :config (setq copilot-indent-offset-warning-disable t)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))
