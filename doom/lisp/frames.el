;;; lisp/frames.el -*- lexical-binding: t; -*-

(defun my/hide-all-child-frames ()
  "Hide all child frames of the current frame."
  (interactive)
  (let ((parent-frame (selected-frame)))
    (dolist (frame (frame-list))
      (when (eq (frame-parent frame) parent-frame)
        (make-frame-invisible frame t)))))

;;;###autoload
(defun my/toggle-frame-decoration ()
  "Toggle the decoration of the current frame."
  (interactive)
  (let ((current-setting (frame-parameter nil 'undecorated)))
    (set-frame-parameter nil 'undecorated (not current-setting)))
  (my/hide-all-child-frames))
