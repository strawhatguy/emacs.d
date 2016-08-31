;;;; system specific stuff here

(when (eq system-type 'darwin)
  (menu-bar-mode 1)
  (setq ns-function-modifier 'hyper)
  )

(when (not (eq system-type 'windows-nt))
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (add-to-list 'exec-path "/usr/local/bin")
  )
