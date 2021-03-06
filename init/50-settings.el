;;;; custom settings

;;;; Seed the random-number generator
(random t)

;;;; Get right to it
(setq inhibit-startup-screen  t)

;;;; no tabs!
(set-default 'indent-tabs-mode nil)

;;;; 2 character indent everywhere
(setq standard-indent 2)

;;;; Stop making backup files
(setq make-backup-files nil)

;;;; Stop making lock files
(setq create-lockfiles nil)

;;;; org-mode
(setq org-log-done t)

;;;; first-error
(setq compilation-scroll-output 'first-error)

;;;; allow color in compilation buffer
(add-hook 'compilation-filter-hook 'mc/colorize-compilation-buffer)

;;;; buffer handling
(add-hook 'Buffer-menu-mode-hook 'mc/Buffer-mode-sort-key-hook)

;;;; Don't spawn a new frame for the ediff commands, keep it all in one frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;;;; Have ediff buffers show in a side-by-side view
(setq ediff-split-window-function 'split-window-horizontally)

;;;; Set browser
(dolist (executable (list "google-chrome" "chromium-browser" "firefox"))
  (let ((browser-path (executable-find executable)))
    (when browser-path
      (setq browse-url-generic-program browser-path
            browse-url-browser-function 'browse-url-generic)
      (return browser-path))))

;;;; Some rcirc mode configuration
(setq rcirc-default-full-name "Matthew Curry")

;;;; company mode messes up eshell
(add-hook 'eshell-mode-hook (lambda () (company-mode -1)))

;;;; try and make mouse less jerky
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil)            ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                  ;; scroll window under mouse
(setq scroll-step 1)                                ;; keyboard scroll one line at a time

;;;; save the file automatically
(auto-save-visited-mode 1)

;;;; this advice function makes delete-trailing-whitespace less jarring
(defun mc/not-at-end-of-line (func &rest args)
  (unless (or (eolp) (eobp))
    (apply func args)))

(setq delete-trailing-lines t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(advice-add 'delete-trailing-whitespace :around #'mc/not-at-end-of-line)
