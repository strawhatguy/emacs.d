(global-set-key (kbd "C-x w") 'elfeed)

(defun mc/elfeed-read-lines (file)
  "Return a list of lines of FILE. Stolen from vc mode"
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun mc/reload-elfeed-configuration ()
  (interactive)
  (setq elfeed-feeds (mc/elfeed-read-lines (concat user-emacs-directory "elfeed-feeds"))))

(mc/reload-elfeed-configuration)
