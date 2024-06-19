;;; lisp/code-actions-react.el -*- lexical-binding: t; -*-

;; (defun my/refactor-react-extract-component-to-file ()
;;   "Extracts the currently selected text into a new component file."
;;   (interactive)
;;   ;; Ensure there's an active region selected.
;;   (if (use-region-p)
;;       (let* ((selected-text (buffer-substring-no-properties (region-beginning) (region-end)))
;;              (component-name (read-string "Component name: "))
;;              (snippet-key "react-component")
;;              (file-destination (read-file-name "Save component to: " default-directory nil nil component-name)))
;;         (kill-region (region-beginning) (region-end))
;;         ;; Create and switch to the new file.
;;         (find-file file-destination)
;;         ;; Insert the template text and the previously selected text.
;;         (yas-expand-snippet (yas-lookup-snippet snippet-key) (point-min) (point-max) '((one . two)))
;;         ;; Save the new file.
;;         (save-buffer)
;;         ;; Switch back to the original buffer.
;;         (switch-to-buffer (other-buffer))
;;         ;; Replace the previously selected text with a reference to the component.
;;         (insert (format "<include component='%s' />" (file-name-nondirectory file-location)))))
;;   ;; If no region is selected, display a message to the user.
;;   (message "No text selected!"))



;; (defun refactor-extract-component ()
;;   "Extracts the currently selected text into a new component file, using a template."
;;   (interactive)
;;   ;; Ensure there's an active region selected.
;;   (if (use-region-p)
;;       (let* ((selected-text (buffer-substring-no-properties (region-beginning) (region-end)))
;;              (component-name (read-string "Component name: "))
;;              (initial-directory default-directory)
;;              (template-path (expand-file-name "snippets/component-template.txt" user-emacs-directory) )
;;              (move-cursor-to-end (lambda ()
;;                                    (goto-char (point-max))
;;                                    (remove-hook 'minibuffer-setup-hook move-cursor-to-end))))
;;         ;; Cut the selected text.
;;         (kill-region (region-beginning) (region-end))
;;         ;; Add the hook to move the cursor.
;;         (add-hook 'minibuffer-setup-hook move-cursor-to-end)
;;         ;; Prompt for file location, defaulting to the current directory.
;;         (let ((file-location (read-file-name "Save component to: " initial-directory nil nil component-name)))
;;           ;; Create and switch to the new file.
;;           (find-file file-location)
;;           ;; Insert template content and the previously selected text.
;;           (when (file-exists-p template-path)
;;             (insert-file-contents template-path))
;;           ;; Save the new file.
;;           (save-buffer)
;;           ;; Switch back to the original buffer.
;;           (switch-to-buffer (other-buffer))
;;           ;; Replace the previously selected text with a reference to the component.
;;           (insert (format "<include component='%s' />" (file-name-nondirectory file-location)))))
;;     ;; If no region is selected, display a message to the user.
;;     (message "No text selected!")))
