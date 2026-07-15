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
;;; Markdown
;;; -----------------------------------------------------------------------------
(after! markdown-mode
  (setq markdown-indent-on-enter 'indent-and-new-item)

  (setq markdown-hide-markup t           ; hide ** _ etc. until cursor is on them
        markdown-header-scaling t        ; scale # ## ### sizes
        markdown-fontify-code-blocks-natively t)

  (add-hook 'markdown-mode-hook #'mixed-pitch-mode)
  (add-hook 'markdown-mode-hook #'visual-line-mode)

  (defun my/markdown-list-continuation-context-p ()
    (and (derived-mode-p 'markdown-mode)
         (eq markdown-indent-on-enter 'indent-and-new-item)
         (not (markdown-code-block-at-point-p))
         (markdown-cur-list-item-bounds)))

  (defun my/markdown-continue-list-with-evil-open-a (fn count)
    (if (my/markdown-list-continuation-context-p)
        (let ((evil-auto-indent nil))
          (funcall fn count)
          (markdown-insert-list-item 1))
      (funcall fn count)))

  (after! evil
    (advice-add #'evil-open-above :around #'my/markdown-continue-list-with-evil-open-a)
    (advice-add #'evil-open-below :around #'my/markdown-continue-list-with-evil-open-a)))

;;;
;;; Restclient
;;; -----------------------------------------------------------------------------
(defun make-basic-auth-token (username password)
  "Return a base64-encoded string of the username and password."
  (base64-encode-string (concat username ":" password)))
