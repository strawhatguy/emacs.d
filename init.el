;;;; Load vanilla-emacs config
(setq debug-on-error t)
(load (concat user-emacs-directory "init-base-emacs-configuration"))

;;;; Install el-get
(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(require 'el-get-elpa)
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))

;;;; local recipes
(add-to-list 'el-get-recipe-path (concat user-emacs-directory "recipes"))

;;;; init-<package>.el files
(setq el-get-user-package-directory (concat user-emacs-directory "package-init-files"))

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

(resync-el-get-packages-from-init-files)
(setq debug-on-error nil)
