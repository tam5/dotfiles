;;; +keybinds.el -*- lexical-binding: t; -*-


(defconst my/leader-key ","
  "Define the leader/prefix key, aka <Leader> in vim. This is
separate from the doom-leader-key, so we can get even quicker access
to our most frequently used keybinds.")


;; Doom has a great keybinding system, but there's certain habits i've picked up
;; over the years that i just can't shake...
(map!
 :n "-" #'dired-jump

 :n "C-p" #'projectile-find-file
 :g "C-s-p" #'execute-extended-command

 :prefix my/leader-key
 :n "w" #'basic-save-buffer
 :n "q" #'evil-quit
 :n "d" #'kill-current-buffer

 :n "v" #'evil-window-vsplit
 :n "h" #'evil-window-split

 :n "1" #'+treemacs/toggle

 :n "ev" (lambda () (interactive) (find-file (concat doom-user-dir "config.el")))
 :n "ek" (lambda () (interactive) (find-file (concat doom-user-dir "+keybinds.el")))
 :n "eu" (lambda () (interactive) (find-file (concat doom-user-dir "+ui.el"))))

(map!
 ;; multiple cursors
 :v "I" #'evil-mc-make-cursor-in-visual-selection-beg
 :v "A" #'evil-mc-make-cursor-in-visual-selection-end

 ;; find references
 :n "gr" #'lsp-find-references)

(map!
 :nvi "M-`" nil
 :i "M-`" (lambda () (interactive) (insert "`"))
 ;; markdown mode
 :mode evil-markdown-mode
 :nvi "s-b" #'markdown-insert-bold
 :nvi "s-i" #'markdown-insert-italic
 :i "M-`" (lambda () (interactive) (insert "`")))

(map!
 :prefix my/leader-key

 ;; avy
 :n "f" #'evil-avy-goto-word-0

 ;; exec code action
 :n "a" #'lsp-execute-code-action)
