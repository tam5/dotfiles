;;; $DOOMDIR/+lang.el -*- lexical-binding: t; -*-


;;; -----------------------------------------------------------------------------
;;; Python
;;; -----------------------------------------------------------------------------
(use-package! lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))


;;; -----------------------------------------------------------------------------
;;; Tailwind
;;; -----------------------------------------------------------------------------
(use-package! lsp-tailwindcss
  :after lsp-mode
  :init (setq lsp-tailwindcss-add-on-mode t))
