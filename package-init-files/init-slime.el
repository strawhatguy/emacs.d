(eval-after-load 'slime
  '(progn
     (setq slime-lisp-implementations
           '((sbcl ("/usr/local/bin/sbcl"))
             (ecl ("/usr/local/bin/ecl"))
             (clisp ("/usr/local/bin/clisp"))))
     (slime-setup '(
                    slime-asdf
                    slime-autodoc
                    slime-editing-commands
                    slime-fancy
                    slime-fancy-inspector
                    slime-fontifying-fu
                    slime-fuzzy
                    slime-indentation
                    slime-mdot-fu
                    slime-package-fu
                    slime-references
                    slime-repl
                    slime-sbcl-exts
                    slime-scratch
                    slime-xref-browser
                    ))
     (slime-autodoc-mode)
     (setq slime-complete-symbol*-fancy t)
     (setq slime-complete-symbol-function
           'slime-fuzzy-complete-symbol)

     (defsubst slime-company-active-p ()
       (derived-mode-p 'lisp-mode 'clojure-mode 'slime-repl-mode))
     
     (defun slime-company-backend (command &optional arg &rest ignored)
       (case command
         ('prefix
          (if (slime-company-active-p)
              (slime-symbol-at-point)))
         ('candidates
          (first (slime-simple-completions (substring-no-properties arg))))
         ('meta
          (slime-eval `(swank:operator-arglist ,arg ,(slime-current-package))))
         ('doc-buffer
          (let ((doc (slime-eval `(swank:describe-symbol ,arg))))
            (with-current-buffer (company-doc-buffer)
              (insert doc)
              (goto-char (point-min))
              (current-buffer))))
         ('location
          (save-window-excursion
            (if (ignore-errors (slime-edit-definition arg))
                (cons (current-buffer) (point)))))))
     
     (add-to-list 'company-backends 'slime-company-backend)
     
     (add-hook 'company-completion-finished-hook
               (lambda (_) (if (slime-company-active-p) (slime-echo-arglist))))

     ))

(require 'slime)
