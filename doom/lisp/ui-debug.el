;;; lisp/ui-debug.el -*- lexical-binding: t; -*-


;;;;;
;;;;;; utils
;;;;;
;;;;;
;;;;;
;;;;;


(require 'json)
(require 'cl-lib)

;; Function to load and process the color JSON file
(defun load-color-map-from-json (file-path)
  "Load a color map from a JSON file at FILE-PATH and populate `my-color-map` hash table."
  (setq my-color-map (make-hash-table :test 'equal))
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json-false nil)
         (json-data (json-read-file file-path)))
    ;; Recursive function to flatten nested JSON color data
    (cl-labels ((flatten-colors (prefix data)
                  (maphash
                   (lambda (key value)
                     (let ((new-key (if prefix
                                        (format "%s-%s" prefix key)
                                      key)))
                       (if (hash-table-p value)
                           (flatten-colors new-key value)
                         (puthash new-key value my-color-map))))
                   data)))
      ;; Populate `my-color-map` by flattening JSON structure
      (flatten-colors nil json-data))))

;; Function to resolve color names
(defun color (name)
  "Resolve color by NAME (a string like \"red-400\") from `my-color-map`."
  (or (gethash name my-color-map)
      (error "Color %s not found in `my-color-map`" name)))

;; Usage: load colors from JSON file
(load-color-map-from-json
 (expand-file-name "tw-colors.json" doom-user-dir))


(defun display-color-map ()
  "Display all colors in `my-color-map` in a new buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*Color Map*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Defined Colors in `my-color-map`:\n\n")
      (maphash (lambda (key value)
                 (insert (format "%s: %s\n" key value)))
               my-color-map))
    (display-buffer buffer)))


;; (color "red-200")

;;;;;
;;;;;; applying the debuggers
;;;;;
;;;;;
;;;;;
;;;;;


;; (defun custom-ui-visualizer-extended ()
;;   "Apply custom borders and backgrounds to visualize additional Doom Emacs UI elements."

;;   ;; Header line
;;   (set-face-attribute 'header-line nil
;;                       :foreground "#00ffcc"
;;                       :background "#002233"
;;                       :box '(:line-width 3 :color "#00ffcc"))

;;   ;; Treemacs buffer
;;   (with-eval-after-load 'treemacs
;;     (set-face-attribute 'treemacs-root-face nil
;;                         :foreground "#ff8800"
;;                         :background "#1c1c1c")
;;     (set-face-attribute 'treemacs-directory-face nil
;;                         :foreground "#ffaa00")
;;     (set-face-attribute 'treemacs-file-face nil
;;                         :foreground "#00ffaa")
;;     (set-face-attribute 'treemacs-fringe-indicator-face nil
;;                         :background "#444400"))

;;   ;; Window title bar (macOS only, visible in GUI mode)
;;   (when (eq system-type 'darwin)
;;     (add-to-list 'default-frame-alist '(ns-appearance . dark))
;;     (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;;     (set-frame-parameter nil 'title "Doom Emacs Customized")
;;     (add-to-list 'default-frame-alist '(ns-title-background . "#550000")))

;;   ;; Gutters (fringe)
;;   (set-face-attribute 'fringe nil
;;                       :background "#333333"
;;                       :foreground "#ff3355")

;;   ;; Modeline gutter or indicator (often used for modified buffer indicator)
;;   (set-face-attribute 'mode-line nil
;;                       :background "#222222"
;;                       :foreground "#ffffff"
;;                       :box '(:line-width 3 :color "#00ff88"))
;;   (set-face-attribute 'mode-line-inactive nil
;;                       :background "#555555"
;;                       :foreground "#bbbbbb"
;;                       :box '(:line-width 3 :color "#555555"))

;;   ;; Customize specific line indicators like flycheck or git-gutter if available
;;   (with-eval-after-load 'flycheck
;;     (set-face-attribute 'flycheck-fringe-warning nil :foreground "#ffaa00" :background "#222222")
;;     (set-face-attribute 'flycheck-fringe-error nil :foreground "#ff0000" :background "#222222")
;;     (set-face-attribute 'flycheck-fringe-info nil :foreground "#00aaff" :background "#222222"))

;;   (with-eval-after-load 'git-gutter
;;     (set-face-attribute 'git-gutter:modified nil :foreground "#ffaa00" :background "#333333")
;;     (set-face-attribute 'git-gutter:added nil :foreground "#00ff00" :background "#333333")
;;     (set-face-attribute 'git-gutter:deleted nil :foreground "#ff0000" :background "#333333"))
;;   )


(defface active-buffer-face
  `((t (:background ,(color "emerald-950"))))
  "Face for the active buffer background."
  :group 'custom-faces)

(defface inactive-buffer-face
  `((t (:background ,(color "red-950"))))
  "Face for the inactive buffer background."
  :group 'custom-faces)

;; Local variables to hold remap cookies
(defvar-local active-buffer-remap-cookie nil "Remap cookie for active buffer background.")
(defvar-local inactive-buffer-remap-cookie nil "Remap cookie for inactive buffer background.")

(defun should-apply-remap? ()
  "Return non-nil if the buffer should be opted in to background remaps.
This excludes special buffers, Treemacs, and Dired buffers."
  (and (not (string-prefix-p "*" (buffer-name)))   ;; Exclude special buffers
       (not (eq major-mode 'treemacs-mode))        ;; Exclude Treemacs
       (not (eq major-mode 'dired-mode))))         ;; Exclude Dired

(defun apply-active-buffer-face ()
  "Apply the active buffer face to the current buffer."
  (when (and (should-apply-remap?)
             (not active-buffer-remap-cookie))
    (setq-local active-buffer-remap-cookie
                (face-remap-add-relative 'default 'active-buffer-face))))

(defun apply-inactive-buffer-face ()
  "Apply the inactive buffer face to the current buffer."
  (when (and (should-apply-remap?)
             (not inactive-buffer-remap-cookie))
    (setq-local inactive-buffer-remap-cookie
                (face-remap-add-relative 'default 'inactive-buffer-face))))

(defun remove-active-buffer-face ()
  "Remove the active buffer face from the current buffer."
  (when active-buffer-remap-cookie
    (face-remap-remove-relative active-buffer-remap-cookie)
    (setq-local active-buffer-remap-cookie nil)))

(defun remove-inactive-buffer-face ()
  "Remove the inactive buffer face from the current buffer."
  (when inactive-buffer-remap-cookie
    (face-remap-remove-relative inactive-buffer-remap-cookie)
    (setq-local inactive-buffer-remap-cookie nil)))

(defun update-active-buffer-faces (&rest _)
  "Update active and inactive buffer faces across all windows."
  ;; Set inactive face for all buffers
  (dolist (window (window-list))
    (with-current-buffer (window-buffer window)
      (remove-active-buffer-face)
      (apply-inactive-buffer-face)))
  ;; Set active face for the selected window buffer
  (with-current-buffer (window-buffer (selected-window))
    (remove-inactive-buffer-face)
    (apply-active-buffer-face)))

;; Set up hooks to manage active and inactive buffer highlighting
(add-hook 'window-selection-change-functions #'update-active-buffer-faces)
(add-hook 'buffer-list-update-hook #'update-active-buffer-faces)

;; Extra TODOs:
;; - inserting svgs should be possible but doesn't seem to be working right now

;; base ----------------------------------------------------------------------------------------------------------

(set-face-attribute 'default nil :background (color "sky-950")) ;; also is the titlebar color
(set-face-attribute 'solaire-default-face nil :background (color "gray-950")) ;; the darker background color for non buffer stuff (?)

;; NOTES:
;; - we can also modify the currently active buffer if we want, see approach above
;; - there is a little bugginess though with this that can be seen in the gutters or something when switching between the windows

;; base ----------------------------------------------------------------------------------------------------------

;; headerline ----------------------------------------------------------------------------------------------------------
;; NOTES:
;; - It does not seem possible to have multiple lines in the header line.
;; - Padding can be simulated by a combination of spacing and line heights plus raising of text
;; - Differnet fonts, different sizes, can be used

(setq-default header-line-format " ")

(setq header-line-format
      (list
       ;; First simulated line with red background and height 1.0
       (propertize " First "
                   'display '(raise -0.2)  ;; Adjust vertical position
                   'face '(:family "Courier" :background "red" :height .5))

       ;; Spacer to simulate a newline
       (propertize " " 'display '(space :height 0.5))

       ;; Second simulated line with yellow background and height 1.5
       (propertize " Second "
                   'display '(raise 1.2)
                   'face '(:family "Arial" :background "yellow" :height 1.5))

       ;; Spacer to simulate another newline
       (propertize " " 'display '(space :height 0.5))

       ;; Third simulated line with green background and height 1.0
       (propertize " Third "
                   'display '(raise -0.2)
                   'face '(:family "Times New Roman" :background "green" :height 1.0))))

(set-face-attribute 'header-line nil
                    :background (color "green-50")
                    :foreground (color "green-700")
                    :box '(:line-width 3 :color (color "green-800")))
;; headerline ----------------------------------------------------------------------------------------------------------
;; NOTES:
;; - there is also such thing as a "Tab bar", which when enabled sits directly below the title bar, right now its actually behaving weird in relation to the internal border though
;; - looks like the tab bar can also be styled at least to some degree if we want

;; modeline ----------------------------------------------------------------------------------------------------------
(set-face-attribute 'mode-line nil
                    :background (color "red-50")
                    :foreground (color "red-700")
                    :box '(:line-width 3 :color (color "red-800")))

(set-face-attribute 'mode-line-inactive nil
                    :background (color "orange-50")
                    :foreground (color "orange-700")
                    :box '(:line-width 3 :color (color "orange-800")))
;; modeline ----------------------------------------------------------------------------------------------------------







(setq default-frame-alist
      '((internal-border-width . 10)
        (border-width . 10))) ;; requires new frame

(set-face-attribute 'internal-border nil :background (color "purple-800"))
(set-face-attribute 'border nil :background (color "red-800"))
(set-face-attribute 'vertical-border nil :background (color "blue-800"))



(setq window-divider-default-right-width 10) ;; requires new frame
(setq window-divider-default-bottom-width 20) ;; requires new frame

(set-face-attribute 'window-divider nil :foreground "cyan")
(set-face-attribute 'window-divider-first-pixel nil :foreground "red") ;; it is the first pixel of the entire column so its like a vertical line
(set-face-attribute 'window-divider-last-pixel nil :foreground "green") ;; it is the first pixel of the entire column so its like a vertical line

;; NOTES
;; - the "border" is not colorizable, and doesn't really seem like i can do anything with it, is controlled by window manager

;; Set left and right fringe colors
(set-face-attribute 'fringe nil :background "yellow")

;; Adjust fringe width (in pixels)
(setq-default left-fringe-width 12)
(setq left-fringe-width 12)
(setq-default right-fringe-width 20)
(setq right-fringe-width 20)


;; NOTES
;; - fringes can also have bitmaps in them, which themselves can be given faces and such
;; - setting different fringe widths for left and right doesnt seem to be working but it should
;; - left and right can't have their own colors, but in theory an overlay could be created instead to mimick the fringes and then be given custom colors for left and right



(setq-default left-margin-width 20)
(setq-default right-margin-width 10)

(set-window-buffer (selected-window) (current-buffer))  ;; Refresh margins

(setq fringes-outside-margins t)

;; NOTES
;; - margins are further space, either between the text and the fringes or between the fringes and the dividers, depending on fringes-outside-margins flag
;; - the bg color of the margin seems to take the bg color of the buffer including the remaps, UNLESS you are on a line that has an icon in the fringe like the text overlflow thing, in which case the color goes back to default face, but that's only true while the icon is displayed in the right fringe currently, or so it seems at least
