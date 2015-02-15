;;;; Load vanilla-emacs config
(load "~/.emacs.d/init-base-emacs-configuration")

;;;; Install el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;;;; local recipes
(add-to-list 'el-get-recipe-path "~/.emacs.d/recipes")

;;;; init-<package>.el files
(setq el-get-user-package-directory "~/.emacs.d/package-init-files")

(defun get-packages-from-init-files ()
  (if (file-exists-p el-get-user-package-directory)
      (let*
          ((files (directory-files el-get-user-package-directory nil "^init-.*\.el"))
           (remove-init-ext (lambda (f)
                              (file-name-sans-extension
                               (mapconcat 'identity (cdr (split-string f "-")) "-")))))
        (mapcar remove-init-ext files))
    (message "There are no packages found in %s") el-get-user-package-directory))

(el-get 'sync (get-packages-from-init-files))
