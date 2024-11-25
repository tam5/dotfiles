;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


(load! "+ui")
(load! "+lang")
(load! "+keybinds")

(setq user-full-name (getenv "MY_FULL_NAME")
      user-mail-address (getenv "MY_EMAIL_ADDRESS")
      lsp-intelephense-licence-key (getenv "LICENSE_KEY_INTELEPHENSE")

      fancy-splash-image (file-name-concat doom-user-dir "rocket.png")

      confirm-kill-emacs nil)


;;; -----------------------------------------------------------------------------
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
(after! company
  ;; i have no patience, i want company to show up immediately
  (setq company-idle-delay 0
        company-minimum-prefix-length 0
        ;; disabling this for now in markdown since i don't need autocomplete for english
        company-global-modes '(not markdown-mode)))

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


;;; -----------------------------------------------------------------------------
;;; Misc
;;; -----------------------------------------------------------------------------

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



;; (defun aritest (&rest _)
;;   (message "this was called ---------------"))

;; (remove-hook 'treemacs-after-visit-functions #'aritest)
