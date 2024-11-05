;;; +theme.el -*- lexical-binding: t; -*-

(load! "lisp/bitmaps")
(load! "lisp/frames")


(setq my/default-background-color (face-attribute 'default :background))


;; NOTES
;; theme `bg` #282c34 -- (lighter, editor color)
;; theme `bg-alt` #21242b -- (darker, treemacs color)
;; default background -- #282c34
;; solaire-default-face background -- #21242b

;; solaire-mode in a buffer applies the remaps,
;; so that means that when solaire mode is on, the buffer is not a real buffer, and instead we get
;; a darker color!
;;
;;
;; doing face-attribute does get the value without the relative remap, at least it seems

;; (face-attribute 'default :background)
;; (face-attribute 'solaire-default-face :background nil t)

;; (set-face-attribute 'default nil :background "#282c34")

;; (face-remap-add-relative 'default :background "black")

;; (set-face-attribute 'default nil :background "#282c34")

(defface editor-buffer-face
  `((t (:background "#282c34")))
  "Face for the active buffer background."
  :group 'custom-faces)


;; if the buffer is NOT real, then soliare mode gets turned on - which

;; so what solaire mode is doing, is it is saying hey check the buffer and if it is _not_ a real buffer,
;; then do face remapping

;; Set the global background color for the default face
;; (set-face-background 'default "red") ;; Set this to your preferred color

;; Define a different background color for "editor" buffers
;; (setq my/editor-buffer-bg "#282c34") ;; Set this to the desired "editor" buffer color

;; Function to remap default face in editor buffers
;; (defun my/remap-default-face ()
;;   "Locally remap `default` face background color in editor buffers."
;;   (face-remap-add-relative 'default :background my/editor-buffer-bg))

;; Add to solaire-mode hooks for enabling/disabling remapping
;; (add-hook 'solaire-mode-hook
;;           (lambda ()
;;             (if solaire-mode
;;                 (my/remap-default-face)
;;               (face-remap-reset-base 'default))))



;; PLAN
;; so whatever color i set to the actual default background, that will be my tab bar
;; but then i want to remap basically everything else to use relative remap and use the desired value from my theme
;;

;; so when the theme is loaded we should
;; check the original default background color of the theme - this will be our desired editor color, so we will store this value in a variable (face)
;; then we are going to need to set the actual default face background color to the solaire mode color (or what we want)
;; but then we're going to need to apply our remap to set the desired color back on top of the default face

;; ------------------------------------------
;; this whole section works to hack a way to set the title bar and frame color to something of our choosing
;; ------------------------------------------
;; (defvar my/default-background-color (face-attribute 'default :background)
;;   "Stores the background color of the default face from the current theme.")

;; (defun my/update-default-background-color (&rest _args)
;;   "Update `my/default-background-color` with the background color of the `default` face."
;;   (setq my/default-background-color (face-attribute 'default :background)))

;; ;; Add to the hook to update color whenever a new theme is enabled
;; (advice-add 'enable-theme :after 'my/update-default-background-color)

;; (defvar-local editor-buffer-background-remap-cookie nil "Remap cookie for active buffer background.")

;; (after! solaire-mode
;;   (defun my/remap-editor-bg (&rest _)
;;     "Update active and inactive buffer faces across all windows."
;;     (with-current-buffer (window-buffer (selected-window))
;;       (when (funcall solaire-mode-real-buffer-fn)
;;         (unless editor-buffer-background-remap-cookie
;;           (set-face-attribute 'default nil :background "red") ;;;;; ----------- this will be the color of the title bar
;;           (setq-local editor-buffer-background-remap-cookie
;;                       (face-remap-add-relative 'default :background my/default-background-color))))))


;;   (defun my/thing-rest (&rest _)
;;     (face-remap-remove-relative editor-buffer-background-remap-cookie)
;;     (setq-local editor-buffer-background-remap-cookie nil)
;;     (my/remap-editor-bg))

;;   (add-hook 'doom-load-theme-hook #'my/thing-rest)
;;   (add-hook 'window-selection-change-functions #'my/remap-editor-bg)
;;   (add-hook 'buffer-list-update-hook #'my/remap-editor-bg))
;; i think we also need to add a hook to remap it when the theme is loaded
;; ------------------------------------------


;; (set-face-attribute 'default nil :background "#282c34")

;; (face-attribute 'default :font)

(setq doom-font (font-spec :family "MesloLGM Nerd Font" :size 14 :weight 'normal))

(setq

 ;; doom-theme 'doom-github-classic-dark
 ;; doom-font (font-spec :family "MesloLGM Nerd Font" :size 15 :weight 'normal)
 ;; doom-variable-pitch-font (font-spec :family "Monaco" :size 13)

 doom-modeline-modal nil

 frame-title-format nil
 ns-use-proxy-icon nil

 display-line-numbers-type t)

(after! treemacs
  (setq treemacs-collapse-dirs 0
        treemacs-git-mode nil))

(add-hook! treemacs-mode 'hide-mode-line-mode)

;; (after! lsp-treemacs
;;   (load! "lisp/doom-themes-ext-treemacs")
;;   (my/doom-themes-treemacs-config))

;; (after! (:and solaire-mode treemacs)
;;   (push '(treemacs-window-background-face . solaire-default-face) solaire-mode-remap-alist)
;;   (push '(treemacs-hl-line-face . solaire-hl-line-face) solaire-mode-remap-alist))

;; (after! highlight-indent-guides
;;   (setq highlight-indent-guides-method 'bitmap
;;         highlight-indent-guides-responsive 'top
;;         highlight-indent-guides-auto-character-face-perc 10
;;         highlight-indent-guides-auto-top-character-face-perc 20
;;         highlight-indent-guides-bitmap-function 'my/highlight-indent-guides--bitmap-dots)
;;   (highlight-indent-guides-mode -1)
;;   (remove-hook! (prog-mode text-mode conf-mode) #'highlight-indent-guides-mode))

;; (set-face-attribute 'font-lock-comment-face nil :slant 'italic)





;; the below stuff works in theory, it will brighten the color of the text on the current line
;; we have to just hook in to when treemacs is calling the hl-line apply and then also
;; apply this i think

;; (defun my-brighten-color-for-face (face)
;;   "Return a brightened version of the foreground color for FACE."
;;   (when (facep face)
;;     (let ((color (face-foreground face nil 'default)))
;;       (when color
;;         (color-lighten-name color 20)))))  ; Adjust the 20% to your liking

;; (defvar my-line-brightened-overlays nil
;;   "List of overlays for brightening the foreground color of text on the current line.")

;; (defun my-apply-brightened-faces-on-line ()
;;   "Apply brightened foreground colors to faces on the current line individually."
;;   (interactive)
;;   (let ((line-start (line-beginning-position))
;;         (line-end (line-end-position)))
;;     ;; First, clear existing overlays.
;;     (mapc 'delete-overlay my-line-brightened-overlays)
;;     (setq my-line-brightened-overlays nil)  ; Reset the list after cleanup.

;;     (save-excursion
;;       (goto-char line-start)
;;       (while (< (point) line-end)
;;         (let* ((pos (point))
;;                (face (get-text-property pos 'face))
;;                (next-change (or (next-single-property-change pos 'face nil line-end) line-end))
;;                (brightened-color (my-brighten-color-for-face face)))
;;           (when brightened-color
;;             (let ((ov (make-overlay pos next-change)))
;;               (overlay-put ov 'face `(:foreground ,brightened-color))
;;               ;; Add this overlay to our list for future management.
;;               (push ov my-line-brightened-overlays)))
;;           (goto-char next-change))))))
