;;; lisp/my-treemacs-theme.el -*- lexical-binding: t; -*-

(require 'dash)
(require 'treemacs)
(require 'nerd-icons)

(defface my/treemacs-theme-dir-icon-face
  '((t (:inherit nil)))
  "Face used for the directory and file icons in nerd-icons theme."
  :group 'treemacs-faces)

(defface my/treemacs-theme-file-icon-face
  '((t (:inherit nil)))
  "Face used for the directory and file icons in nerd-icons theme."
  :group 'treemacs-faces)

(defface my/treemacs-theme-current-file-face
  '((t (:inherit treemacs-file-face)))
  "Face used for the directory and file icons in nerd-icons theme."
  :group 'treemacs-faces)

(defvar my/treemacs-theme-padding--left (propertize " " 'display '(space :width 2)))
(defvar my/treemacs-theme-dir-spacer--left (propertize " " 'display '(space :width 0)))
(defvar my/treemacs-theme-dir-spacer--right (propertize " " 'display '(space :width 1.4)))
(defvar my/treemacs-theme-icon-spacer--right (propertize " " 'display '(space :width 1.6)))
(defvar my/treemacs-theme-icon-spacer--left (propertize " " 'display '(space :width 0.2)))

(defun my/treemacs-theme-hide-fringes (&rest _)
  "Remove fringes in treemacs window, mostly since it makes hl-line look bad."
  (when (display-graphic-p)
    (set-window-fringes nil 0 0)))

(treemacs-create-theme "my/treemacs-theme"
  :config
  (progn
    ;; directory icons
    (treemacs-create-icon :icon (format "%s%s%s%s"
                                        my/treemacs-theme-padding--left
                                        my/treemacs-theme-dir-spacer--left
                                        (nerd-icons-faicon "nf-fa-folder_o" :v-adjust -0.06 :height 1.12 :face 'my/treemacs-theme-dir-icon-face)
                                        my/treemacs-theme-dir-spacer--right)
                          :extensions (root-open dir-open)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s%s"
                                        my/treemacs-theme-padding--left
                                        my/treemacs-theme-dir-spacer--left
                                        (nerd-icons-faicon "nf-fa-folder" :v-adjust -0.06 :height 1.12 :face 'my/treemacs-theme-dir-icon-face)
                                        my/treemacs-theme-dir-spacer--right)
                          :extensions (root-closed dir-closed)
                          :fallback 'same-as-icon)
    ;; file icons
    (dolist (item nerd-icons-extension-icon-alist)
      (let* ((extension (car item))
             (func (cadr item))
             (args (append (list (cadr (cdr item))) '(:v-adjust -0.04 :height 0.92) (cdr (cddr item))))
             (icon (apply func args)))
        (let* ((icon-pair (cons (format "%s%s%s%s" my/treemacs-theme-padding--left my/treemacs-theme-icon-spacer--left icon my/treemacs-theme-icon-spacer--right)
                                (format "%s%s%s%s" my/treemacs-theme-padding--left my/treemacs-theme-icon-spacer--left icon my/treemacs-theme-icon-spacer--right)))
               (gui-icons (treemacs-theme->gui-icons treemacs--current-theme))
               (tui-icons (treemacs-theme->tui-icons treemacs--current-theme))
               (gui-icon  (car icon-pair))
               (tui-icon  (cdr icon-pair)))
          (ht-set! gui-icons extension gui-icon)
          (ht-set! tui-icons extension tui-icon))))
    ;; fallback
    (treemacs-create-icon :icon (format "%s%s%s%s"
                                        my/treemacs-theme-padding--left
                                        my/treemacs-theme-icon-spacer--left
                                        (nerd-icons-faicon "nf-fa-file_o" :face 'my/treemacs-theme-file-icon-face)
                                        my/treemacs-theme-icon-spacer--right)
                          :extensions (fallback)
                          :fallback 'same-as-icon)))



;;; WIP - fringe indicator ->
;;; this gets the fringe indicator all the way to the left, but i want it to stay on the selected file
;;; and not follow my cursor
;;; also it does look like the fringes get messed up all the time so we'll need to add that hook to make
;;; sure it keeps getting reset properly
;;; and then i also need to do something different with the cursor
;;; check 'treemacs-show-cursor'
;; (defun doom-themes-define-treemacs-fringe-indicator-bitmap ()
;;   "Defines `treemacs--fringe-indicator-bitmap'"
;;   (if (fboundp 'define-fringe-bitmap)
;;       (define-fringe-bitmap 'treemacs--fringe-indicator-bitmap
;;         (make-vector 26 #b111) nil 3)))

;; (add-hook 'treemacs-mode-hook #'doom-themes-define-treemacs-fringe-indicator-bitmap)

;; (setq treemacs--fringe-indicator-bitmap
;;       (define-fringe-bitmap 'treemacs--fringe-indicator-bitmap-default (make-vector 200 #b11100000)))

;;; WIP



;; (set-frame-parameter nil 'cursor-color "orange")
;; (set-face-attribute 'cursor nil :background "red" :foreground "yellow")

;; (defun my/set-cursor-color-for-treemacs ()
;;   "Set a specific cursor color in treemacs-mode."
;;   (setq-local cursor-type nil))

;; (add-hook 'treemacs-mode-hook #'my/set-cursor-color-for-treemacs)

;; (map!  :g "M-i" #'aritest)
;; (map!  :g "M-u" #'aritest)

;; (defun aritest ()
;;   (interactive)
;;   (setq cursor-type nil))


(defun my/treemacs-theme-reload ()
  ""
  (interactive)
  ;; TODO
  ;;
  ;; deal with hl-line
  ;; this one turns off the hl-line in the treemacs (assuming we have solaire on)
  ;; (set-face-attribute 'solaire-hl-line-face nil :background (face-attribute 'solaire-default-face :background))
  ;; problem though is i do want the hl-line when my cursor is in there, just not when i'm in reg buf

  ;; treemacs-hl-line-face
  ;;
  ;; TEMP - set all the faces to the right font and size,
  ;; there is something wrong with doom forcing treemacs git mode to be on
  ;; so we have to override it
  ;;
  ;; check on the root face and spacing
  ;; and the colors of the icons
  ;; add font to dotfiles
  ;; fringe indicator / hl-lien
  ;;
  ;; we don't need the echo messages of "written"
  (dolist (face '(treemacs-directory-face
                  treemacs-directory-collapsed-face
                  treemacs-file-face
                  treemacs-root-face
                  treemacs-git-conflict-face
                  treemacs-git-added-face
                  treemacs-git-untracked-face
                  treemacs-git-ignored-face
                  treemacs-git-renamed-face
                  treemacs-git-modified-face
                  treemacs-git-unmodified-face))
    (set-face-attribute face nil :font (font-spec :family "Inter 1.5" :size 12.0 :weight 'medium)))
  ;;

  (treemacs-load-theme "my/treemacs-theme"))

(provide 'my/treemacs-theme)
;;; my/treemacs-theme.el ends here




























;; (defun my/is-special-buffer-p (buffer)
;;   "Check if BUFFER is a 'special' buffer, such as the minibuffer or Treemacs."
;;   (let ((buf-name (buffer-name buffer)))
;;     (or (minibufferp buffer)
;;         (string-prefix-p " *Minibuf-" buf-name)
;;         (string-equal buf-name " *Treemacs*")
;;         (derived-mode-p 'special-mode))))

(defvar-local my/treemacs-theme-current-file-overlay nil
  "Overlay used to highlight the current Treemacs node.")

(defun my/treemacs-theme-current-highlight-reset ()
  (when my/treemacs-theme-current-file-overlay
    (delete-overlay my/treemacs-theme-current-file-overlay)
    (setq my/treemacs-theme-current-file-overlay nil)))

(defun my/treemacs-theme-highlight-current-file (file)
  "Highlight the filename of the Treemacs node corresponding to `file`."
  (with-current-buffer (treemacs-get-local-buffer)
    (-when-let* ((btn (treemacs-find-visible-node file))
                 (label-start (treemacs-button-start btn))
                 (label-end (treemacs-button-end btn)))
      (save-excursion
        (goto-char label-start)
        (let ((inhibit-read-only t))
          (setq my/treemacs-theme-current-file-overlay (make-overlay label-start label-end))
          (overlay-put my/treemacs-theme-current-file-overlay 'face 'my/treemacs-theme-current-file-face))))))


(defun aritest ()
  "Test function to highlight a Treemacs node."
  (interactive)
  (my/treemacs-theme-current-highlight-reset)
  (let ((file (buffer-file-name (window-buffer))))
    (when file
      (my/treemacs-theme-highlight-current-file file))))

(map! :g "M-i" #'aritest)
;; TODO - now we just got to get it to apply when we switch files and/or when we change the layout of treemacs like open/close nodes bc something might now be visible that wasn't
;; also we should always have visual line mode on

;; Add the hook to track window selection changes
;; (add-hook 'window-selection-change-functions #'my/on-cursor-move)

(set-face-attribute 'my/treemacs-theme-current-file-face nil :foreground "white")
