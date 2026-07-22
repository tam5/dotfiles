;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))

(package! hotfuzz
  :recipe
  (:host github
   :repo "axelf4/hotfuzz"
   :files ("*.el" "*.so")
   ;; Build the optional native matcher before Straight links the package into
   ;; its build directory.  The stable Homebrew `emacs' shim lives beside the
   ;; matching include directory, so this survives Emacs point upgrades.
   :pre-build
   ((condition-case err
        (let* ((emacs-bin (file-truename (executable-find "emacs")))
               (include-dir
                (expand-file-name "../include"
                                  (file-name-directory emacs-bin)))
               (header (expand-file-name "emacs-module.h" include-dir))
               (output (expand-file-name "hotfuzz-module.so"
                                         default-directory))
               temp-output)
          ;; Never leave an ABI-stale module for a newer Emacs to discover.
          (when (file-exists-p output)
            (delete-file output))
          (if (not (file-readable-p header))
              (display-warning
               'hotfuzz
               (format
                "Skipping optional native matcher; missing %s. Using Elisp fallback."
                header)
               :warning)
            (unwind-protect
                (progn
                  ;; Compile beside the destination so the successful rename
                  ;; is atomic on the same filesystem.
                  (setq temp-output
                        (make-temp-file
                         (expand-file-name ".hotfuzz-module-"
                                           default-directory)
                         nil ".so"))
                  (with-temp-buffer
                    (let ((status
                           (call-process
                            "cc" nil (current-buffer) t
                            "-std=c11" "-O3" "-march=native" "-DNDEBUG"
                            "-fPIC" "-bundle" "-undefined" "dynamic_lookup"
                            "-pthread" "-I" include-dir
                            "-o" temp-output "hotfuzz-module.c")))
                      (if (and (integerp status) (zerop status))
                          (progn
                            (rename-file temp-output output t)
                            (setq temp-output nil))
                        (display-warning
                         'hotfuzz
                         (format
                          "Optional native matcher build failed; using Elisp fallback:\n%s"
                          (buffer-string))
                         :warning)))))
              (when (and temp-output (file-exists-p temp-output))
                (delete-file temp-output)))))
      (error
       ;; Even failures while resolving the compiler/Emacs installation must
       ;; not leave an ABI-stale module for the next startup to discover.
       (let ((output (expand-file-name "hotfuzz-module.so"
                                       default-directory)))
         (when (file-exists-p output)
           (delete-file output)))
       (display-warning
        'hotfuzz
        (format
         "Optional native matcher build failed (%s); using Elisp fallback."
         (error-message-string err))
        :warning))))))

(package! lsp-tailwindcss
  :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))

(package! lsp-pyright)

;; (package! gptel :recipe (:nonrecursive t))
