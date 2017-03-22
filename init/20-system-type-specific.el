;;;; system specific stuff here

(defun mc/darwin-settings ()
  (interactive)
  (menu-bar-mode 1)
  (setq ns-function-modifier 'hyper)
  )

(defun mc/not-windows-settings ()
  (interactive)
  (setenv "PATH" (concat "/usr/local/bin:"
                         (concat (getenv "HOME") "/.cargo/bin:")
                         (getenv "PATH")))
  (add-to-list 'exec-path "/usr/local/bin")
  (add-to-list 'exec-path (concat (getenv "HOME") "/.cargo/bin"))
  )

(when (eq system-type 'darwin) (mc/darwin-settings))

(when (not (eq system-type 'windows-nt)) (mc/not-windows-settings))

