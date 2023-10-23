;;; lisp/bitmaps.el -*- lexical-binding: t; -*-

(defun my/highlight-indent-guides--bitmap-dots (width height crep zrep)
  "Defines a dotted guide line, with 1x1 pixel dots and 1px spaces in between,
with 3 or 4 dots per row. Use WIDTH, HEIGHT, CREP, and ZREP as described
in `highlight-indent-guides-bitmap-function'."
  (message "width %s, height %s" width height)
  (let* ((left (/ (- width 2) 2))
         (right (- width left 2))
         (num-dots (/ height 2))
         (dot-space (/ height (- num-dots 1)))
         (dots-per-row (if (= (% height 2) 0) num-dots (1+ num-dots)))
         (row1 (append (make-list left zrep) (make-list 1 crep) (make-list 1 zrep) (make-list right zrep)))
         (row2 (make-list width zrep))
         (rows (list row2)))
    (dotimes (i num-dots rows)
      (setq rows (cons (if (<= i (* dots-per-row 2)) row1 row2) rows))
      (setq rows (cons (make-list width zrep) rows)))
    (setq rows (cons (if (= (% height 2) 0) row1 row2) rows))))

(define-fringe-bitmap 'my/fringe-bitmap-circle
  (vector #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00011100
          #b00111110
          #b00111110
          #b00111110
          #b00011100
          #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00000000))
