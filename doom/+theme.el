;;; +theme.el -*- lexical-binding: t; -*-

(setq doom-theme 'doom-github-classic-dark
      doom-themes-treemacs-theme "doom-colors"

      doom-font (font-spec :family "MesloLGM Nerd Font" :size 15 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "Monaco" :size 13)

      frame-title-format nil

      display-line-numbers-type t)

(after! treemacs
  (setq treemacs-collapse-dirs 0))

(load! "lisp/bitmaps")

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

(custom-set-faces!
  ;; `((doom-modeline-project-root-dir doom-modeline-project-dir doom-modeline-buffer-path doom-modeline-buffer-file doom-modeline-buffer-modified doom-modeline-info)
  ;;   :foreground ,(doom-color 'fg-alt) :weight normal)
  ;;  '((mode-line mode-line-inactive)
  ;;    :family +ui/modeline-font-family :height 125 :weight normal)

  ;; `(treemacs-file-face :foreground ,(doom-color 'fg-alt))
  ;; `(treemacs-directory-face :foreground ,(doom-color 'fg-alt))
  ;; `(treemacs-root-face :foreground ,(doom-color 'fg-alt))

  '(all-the-icons-red :foreground "#B42839")
  '(all-the-icons-lred :foreground "#E54542")
  '(all-the-icons-dred :foreground "#CB3837")
  '(all-the-icons-red-alt :foreground "#671855")
  '(all-the-icons-green :foreground "#ffffff")
  '(all-the-icons-lgreen :foreground "#43D16B")
  '(all-the-icons-dgreen :foreground "#ffffff")
  '(all-the-icons-yellow :foreground "#F0DC3E")
  '(all-the-icons-lyellow :foreground "#EED982")
  '(all-the-icons-dyellow :foreground "#FAA91A")
  '(all-the-icons-blue :foreground "#356EA0")
  '(all-the-icons-blue-alt :foreground "#283249")
  '(all-the-icons-lblue :foreground "#29A8DE")
  '(all-the-icons-dblue :foreground "#1F77CE")
  '(all-the-icons-maroon :foreground "#ffffff")
  '(all-the-icons-lmaroon :foreground "#ffffff")
  '(all-the-icons-dmaroon :foreground "#6C5FA9")
  '(all-the-icons-purple :foreground "#4A37BA")
  '(all-the-icons-purple-alt :foreground "#7F50B1")
  '(all-the-icons-lpurple :foreground "#8C6184")
  '(all-the-icons-dpurple :foreground "#542D8D")
  '(all-the-icons-orange :foreground "#EF7A39")
  '(all-the-icons-lorange :foreground "#F0582B")
  '(all-the-icons-dorange :foreground "#F0582B")
  '(all-the-icons-cyan :foreground "#00E4FF")
  '(all-the-icons-cyan-alt :foreground "#35BAA0")
  '(all-the-icons-lcyan :foreground "#829EC2")
  '(all-the-icons-dcyan :foreground "#ffffff")
  '(all-the-icons-pink :foreground "#FF4893")
  '(all-the-icons-lpink :foreground "#BCA784")
  '(all-the-icons-dpink :foreground "#ffffff")
  '(all-the-icons-silver :foreground "#40535B")
  '(all-the-icons-lsilver :foreground "#ffffff")
  '(all-the-icons-dsilver :foreground "#40535B"))
