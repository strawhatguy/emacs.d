;; -*- lexical-binding: t; -*-
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

;;;; try and make mouse less jerky
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil)            ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                  ;; scroll window under mouse
(setq scroll-step 1)                                ;; keyboard scroll one line at a time

;;;; save the file automatically
(auto-save-mode -1)
(auto-save-visited-mode 1)
(setq auto-save-visited-interval 1)
(setq auto-save-interval 0)
(setq compilation-ask-about-save nil)

;;;; this advice function makes delete-trailing-whitespace less jarring
(defun mc/not-at-end-of-line (func &rest args)
  (unless (or (eolp) (eobp))
    (apply func args)))

(setq delete-trailing-lines t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(advice-add 'delete-trailing-whitespace :around #'mc/not-at-end-of-line)

;;;; postgres sql interactive to handle '-' in db names
(defun mc/sql-interactive-mode-hook ()
  "Custom interactive SQL mode behaviours.  See `sql-interactive-mode-hook'."
  ;; Product-specific behaviours.
  (when (eq sql-product 'postgres)
    ;; Allow symbol chars and hyphens in database names in prompt.
    ;; Default postgres pattern was: "^\\w*=[#>] " (see `sql-product-alist').
    (setq sql-prompt-regexp "^\\(?:\\sw\\|\\s_\\|-\\)*=[#>] ")
    ;; Ditto for continuation prompt: "^\\w*[-(][#>] "
    (setq sql-prompt-cont-regexp "^\\(?:\\sw\\|\\s_\\|-\\)*[-(][#>] ")))

(add-hook 'sql-interactive-mode-hook 'mc/sql-interactive-mode-hook)
