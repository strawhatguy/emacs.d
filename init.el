;; Allow this to function outside of an emacs.d directory
(setq debug-on-error t)

(if (not (boundp 'user-emacs-directory))
    (setq user-emacs-directory "~/.emacs.d/"))

(defun mc/reload-all-user-initialization-files ()
  (interactive)
  (mapcar (lambda (f)
            (message "loading %s" f)
            (load-file f))
          (file-expand-wildcards (concat user-emacs-directory "init/*.el"))))

;; most errors because of package repo list not updated,
;; so try refreshing, and reload once, before failing
(condition-case nil
    (mc/reload-all-user-initialization-files)
  (error (progn (message "First load failed, trying again with refreshing packages")
                (package-refresh-contents)
                (mc/reload-all-user-initialization-files))))

(message "My emacs directory is: %s" user-emacs-directory)

(setq debug-on-error nil)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)
