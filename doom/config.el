;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name (getenv "MY_FULL_NAME")
      user-mail-address (getenv "MY_EMAIL_ADDRESS")
      lsp-intelephense-licence-key (getenv "LICENSE_KEY_INTELEPHENSE"))

(load! "+theme")
(load! "+keybinds")
(load! "lisp/codebase-helpers")

(setq org-directory "~/org/")

(setq confirm-kill-emacs nil
      dired-kill-when-opening-new-dired-buffer t)

(after! company
  (setq company-idle-delay 0
        company-minimum-prefix-length 0
        company-global-modes '(not markdown-mode)))

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(after! orderless
  (defun +vertico-orderless-dispatch-a (orig-fun &rest args)
    "Use flex matching by default. There's probably a more straightforward
way you're supposed to do this, but wtvr."
    (or (apply orig-fun args)
        (cons 'orderless-flex (car args))))

  (advice-add '+vertico-orderless-dispatch :around #'+vertico-orderless-dispatch-a))

(after! lsp-ui
  (setq lsp-ui-sideline-enable nil))

(after! lsp-mode
  (setq ;; If an LSP server isn't present when I start a prog-mode buffer, you
   ;; don't need to tell me. I know. On some machines I don't care to have
   ;; a whole development environment for some ecosystems.
   lsp-enable-suggest-server-download nil))

(use-package! lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

(use-package! lsp-tailwindcss
  :after lsp-mode
  :init (setq lsp-tailwindcss-add-on-mode t))
