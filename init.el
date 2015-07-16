;;;; Load vanilla-emacs config
(setq debug-on-error t)

;;;; Add to environment variables
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(add-to-list 'exec-path "/usr/local/bin")

(require 'package)

(defun setup-el-get-receipes (&optional elpa)
  (require 'el-get-elpa)
  (when elpa (el-get-elpa-build-local-recipes)))

;;;; Install el-get
(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp))
  (setup-el-get-receipes))

;;;; rebuild recipes should they get deleted
(setup-el-get-receipes (not (file-directory-p el-get-recipe-path-elpa)))

;;;; local recipes
(add-to-list 'el-get-recipe-path (concat user-emacs-directory "recipes"))

;;;; init-<package>.el files
(setq el-get-user-package-directory (concat user-emacs-directory "packages"))

(defun get-packages-from-init-files ()
  (if (file-exists-p el-get-user-package-directory)
      (let*
          ((files (directory-files el-get-user-package-directory nil "^init-.*\.el$"))
           (remove-init-ext (lambda (f)
                              (file-name-sans-extension
                               (mapconcat 'identity (cdr (split-string f "-")) "-")))))
        (mapcar remove-init-ext files))
    (list)))

(defun resync-el-get-packages-from-init-files ()
  (interactive)
  (el-get 'sync (get-packages-from-init-files)))

(defun reload-all-user-initialization-files ()
  (interactive)
  (let ((init-files-dir (concat user-emacs-directory "init")))
    ;; (message "byte-compiling %s" init-files-dir)
    ;; (byte-recompile-directory init-files-dir)
    (mapc (lambda (f)
	    (lexical-let ((f2 (file-name-sans-extension f)))
                (message "loading %s" f)
                (load f)))
            (directory-files init-files-dir t ".*\.el"))))

(resync-el-get-packages-from-init-files)
(reload-all-user-initialization-files)

(setq debug-on-error nil)
