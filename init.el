;;;; Load vanilla-emacs config
(setq debug-on-error t)

;;;; Add to environment variables
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(add-to-list 'exec-path "/usr/local/bin")

(require 'package)

(defvar mc/el-get-first-load-p nil)

;;;; Install el-get
(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp))
  (setq mc/el-get-first-load-p t))

;;;; rebuild recipes should they get deleted
(require 'el-get-elpa)
(defun mc/setup-el-get-receipes ()
  (interactive)
  (when (or mc/el-get-first-load-p
	    (not (file-directory-p el-get-recipe-path-elpa)))
    (el-get-elpa-build-local-recipes)))

(mc/setup-el-get-receipes)

;;;; local recipes
(add-to-list 'el-get-recipe-path (concat user-emacs-directory "recipes"))

;;;; init-<package>.el files
(setq el-get-user-package-directory (concat user-emacs-directory "packages"))

(defun mc/el-get-package-exists-p (package)
  (let ((exists-p (el-get-recipe-filename package)))
    (unless exists-p (message "Could not find package recipe for %s" package))
    exists-p))

(defun mc/el-get-only-existing-packages (packages)
  (remove-if-not 'mc/el-get-package-exists-p packages))

(defun mc/get-packages-from-init-files ()
  (when (file-exists-p el-get-user-package-directory)
      (let*
          ((files (directory-files el-get-user-package-directory nil "^init-.*\.el$"))
           (remove-init-ext (lambda (f)
                              (file-name-sans-extension
                               (mapconcat 'identity (cdr (split-string f "-")) "-")))))
	(mc/el-get-only-existing-packages (mapcar remove-init-ext files)))))

(defun mc/el-get-resync-packages-from-init-files ()
  (interactive)
  (el-get 'sync (mc/get-packages-from-init-files)))

(defun mc/reload-all-user-initialization-files ()
  (interactive)
  (let ((init-files-dir (concat user-emacs-directory "init")))
    ;; (message "byte-compiling %s" init-files-dir)
    ;; (byte-recompile-directory init-files-dir)
    (mapc (lambda (f)
	    (lexical-let ((f2 (file-name-sans-extension f)))
                (message "loading %s" f)
                (load f)))
            (directory-files init-files-dir t ".*\.el"))))

(mc/el-get-resync-packages-from-init-files)
(mc/reload-all-user-initialization-files)

(setq debug-on-error nil)
