;;; lisp/title-bar.el -*- lexical-binding: t; -*-

(defvar my/default-background-color nil
  "The background color of the `default` face.")

(defvar-local my/buffer-default-face-remap-cookie nil
  "Remap cookie for active buffer background.")

(defun my/editor-bg-apply-remap ()
  "hacks."
  (interactive)
  (with-current-buffer (window-buffer (selected-window))
    (setq-local my/buffer-default-face-remap-cookie
                (face-remap-add-relative 'default 'my-editor-face))))

(defun my/do-setup-things ()
  ""
  (set-face-attribute 'my-editor-face nil (face-attribute 'default :background))
  (set-face-attribute 'default nil :background "red")
  (my/remap-current-buffer))

;; the problem now is that we would need to re-run the remap relative for every buffer
;;
;; so after the theme is loaded we should:
;; check the original default background color of the theme - this will be our desired editor color, so we will store this value in a variable (face)
;; then we are going to need to set the actual default face background color to the solaire mode color (or what we want)
;; but then we're going to need to apply our remap to set the desired color back on top of the default face

;; ------------------------------------------
;; this whole section works to hack a way to set the title bar and frame color to something of our choosing
;; ------------------------------------------
;; or spec

;; (defun my/update-default-background-color (&rest _args)
;;   "Update `my/default-background-color` with the background color of the `default` face."
;;   (setq my/default-background-color (face-attribute 'default :background)))

;; Add to the hook to update color whenever a new theme is enabled
;; (advice-add 'enable-theme :after 'my/update-default-background-color)

(after! solaire-mode
  (defun my/remap-editor-bg (&rest _)
    "Update active and inactive buffer faces across all windows."
    (with-current-buffer (window-buffer (selected-window))
      (when (funcall solaire-mode-real-buffer-fn)
        (unless editor-buffer-background-remap-cookie
          (set-face-attribute 'default nil :background "red") ;;;;; ----------- this will be the color of the title bar
          (setq-local editor-buffer-background-remap-cookie
                      (face-remap-add-relative 'default :background my/default-background-color))))))


  (defun my/thing-rest (&rest _)
    (face-remap-remove-relative editor-buffer-background-remap-cookie)
    (setq-local editor-buffer-background-remap-cookie nil)
    (my/remap-editor-bg))

  (add-hook 'doom-load-theme-hook #'my/thing-rest)
  (add-hook 'window-selection-change-functions #'my/remap-editor-bg)
  (add-hook 'buffer-list-update-hook #'my/remap-editor-bg))
;; i think we also need to add a hook to remap it when the theme is loaded
;; ------------------------------------------
