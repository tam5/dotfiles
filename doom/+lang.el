;;; $DOOMDIR/+lang.el -*- lexical-binding: t; -*-


;;;
;;; PHP
;;; -----------------------------------------------------------------------------

(setq lsp-intelephense-licence-key (getenv "LICENSE_KEY_INTELEPHENSE"))


;;;
;;; Python
;;; -----------------------------------------------------------------------------
(use-package! lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))


;;;
;;; Tailwind
;;; -----------------------------------------------------------------------------
(use-package! lsp-tailwindcss
  :after lsp-mode
  :init (setq lsp-tailwindcss-add-on-mode t))

;;;
;;; Restclient
;;; -----------------------------------------------------------------------------
(defun make-basic-auth-token (username password)
  "Return a base64-encoded string of the username and password."
  (base64-encode-string (concat username ":" password)))
