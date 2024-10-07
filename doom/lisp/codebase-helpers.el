;;; lisp/codebase-helpers.el -*- lexical-binding: t; -*-

(require 's)

(defun my/get-component-name-from-file ()
  "Return a formatted component name based on the current file name."
  (let* ((filename (file-name-base (buffer-file-name)))
         (formatted-name (s-upper-camel-case filename)))
    formatted-name))
