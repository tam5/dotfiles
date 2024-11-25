;;; lisp/ui-tweaks.el -*- lexical-binding: t; -*-
;;;
;;;


(defconst my/min-alpha-coefficient 0.0000000000000001
  "The minimum possible alpha coefficient.")

(defvar my/sidebar-font nil
  "The font to use for the sidebar.

I separate this from the doom-fonts just for clarity, as I'll be
slightly tweaking which fonts are used where.")

(defun my/titlebar-patch-color ()
  "Set the titlebar color to match the solaire default face.

This is a tremendous hack that's relying on our custom patch against the
C source that prevents `set-background-color` from also setting the face.

It seems to do what we want at least for now."
  (let* ((bg (face-attribute 'solaire-default-face :background))
         ;; For whatever reason if we use the exact same color as the solaire default face we get weird artifacts,
         ;; so we use a slightly different color. If we figure out a more proper way to set the titlebar color
         ;; in the first place then this can probably go away.
         (bg-modified (doom-darken bg my/min-alpha-coefficient)))
    (set-background-color bg-modified)))

(defun my/titlebar-remove-title (frame)
  "Set the titlebar title to an empty string.

For whatever reason the only way we got this to work was to first set the value
to something and then to reset it back to empty."
  (with-selected-frame frame
    (set-frame-name "_hack_")
    (set-frame-name "")))

(defun my/hide-all-child-frames ()
  "Hide all child frames of the current frame."
  (interactive)
  (let ((parent-frame (selected-frame)))
    (dolist (frame (frame-list))
      (when (eq (frame-parent frame) parent-frame)
        (make-frame-invisible frame t)))))

(defun my/hide-fringes (&rest _)
  "Helper function to hide fringes in the current window.

Mostly for disabling fringes in the treemacs sidebar."
  (when (display-graphic-p)
    (set-window-fringes nil 0 0)))

;;;###autoload
(defun my/toggle-frame-decoration ()
  "Toggle the decoration of the current frame."
  (interactive)
  (let ((current-setting (frame-parameter nil 'undecorated)))
    (set-frame-parameter nil 'undecorated (not current-setting)))
  (my/hide-all-child-frames))
