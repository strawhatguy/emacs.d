(require 'magit)
(global-set-key (kbd "C-c g") 'magit-status)

(setq magit-last-seen-setup-instructions "1.4.0")

;;;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(setq magit-log-arguments '("--decorate"))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(define-key magit-mode-map (kbd "M-<tab>") nil)
(define-key magit-mode-map (kbd "C-<tab>") nil)
(define-key magit-mode-map (kbd "s-<tab>") nil)