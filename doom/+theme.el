;;; +theme.el -*- lexical-binding: t; -*-

(load! "lisp/bitmaps")
(load! "lisp/frames")

(setq doom-theme 'doom-github-classic-dark
      doom-font (font-spec :family "MesloLGM Nerd Font" :size 15 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "Monaco" :size 13)

      frame-title-format nil

      display-line-numbers-type t)

(after! treemacs
  (setq treemacs-collapse-dirs 0))

(after! lsp-treemacs
  (load! "lisp/doom-themes-ext-treemacs")
  (my/doom-themes-treemacs-config))

(after! (:and solaire-mode treemacs)
  (push '(treemacs-window-background-face . solaire-default-face) solaire-mode-remap-alist)
  (push '(treemacs-hl-line-face . solaire-hl-line-face) solaire-mode-remap-alist))

(after! highlight-indent-guides
  (setq highlight-indent-guides-method 'bitmap
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-auto-character-face-perc 10
        highlight-indent-guides-auto-top-character-face-perc 20
        highlight-indent-guides-bitmap-function 'my/highlight-indent-guides--bitmap-dots)

  ;; Disable for performance and cleanliness, togglable via 'SPC-t-i'
  (highlight-indent-guides-mode -1)
  (remove-hook! (prog-mode text-mode conf-mode) #'highlight-indent-guides-mode))

(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
