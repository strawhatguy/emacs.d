(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

(and (fboundp 'menu-bar-mode)
     (not (eq system-type 'darwin))
     (menu-bar-mode -1))
(dolist (mode '(tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))
(when (eq system-type 'darwin)
  (setq ns-function-modifier 'hyper))

;;;; Stop making backup files
(setq make-backup-files nil)

;;;; toggle horizontal/vertical splitting
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))
(global-set-key [f9]   'toggle-window-split)

;;;; Make control tab switch buffer windows
(global-set-key [(control tab)] 'other-window)
(defun other-window-backwards (&optional count all-frames)
  (interactive)
  (other-window (if count (- count) -1) all-frames))
(global-set-key [(control shift iso-lefttab)] 'other-window-backwards)

;;;; Make meta g to goto-line
(global-set-key [(meta g)] 'goto-line)

;;;; set F5 key to revert-buffer
(defun reset-buffer () 
  "Resets a file-buffer reflect the file on disk, resetting modes"
  (interactive) (revert-buffer nil t nil))
(global-set-key [f5] 'reset-buffer)

;;;; set f8 to be recompile, shift-f8 to compile, scroll compile buffer
(defun compile-asking-directory (top-level)
  (interactive "DProject toplevel directory: ")
  (let ((default-directory top-level))
    (call-interactively 'compile)))
(global-set-key [f8]   'recompile)
(global-set-key [S-f8] 'compile-asking-directory)
(global-set-key [C-S-f8] 'compile)
(setq compilation-scroll-output t)

;;;; allow color in compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;;; Make C-h C-s go to apropos (basically apropos-symbol)
(global-set-key [(control h) (control s)] 'apropos)

;;;; Advise the shell commands to name the buffer after the command itself
(defadvice async-shell-command (before buffer-named-with-command
                                       (command &optional output-buffer error-buffer)
                                       activate compile)
  (setq output-buffer (or output-buffer (concat "*Async: " command "*")))
  (let ((dir default-directory))
    (switch-to-buffer output-buffer)
    (setq default-directory dir)))

(defadvice shell-command (before buffer-named-with-command
                                 (command &optional output-buffer error-buffer)
                                 activate compile)
  (setq output-buffer (or output-buffer (concat "*Shell: " command "*")))
  (let ((dir default-directory))
    (switch-to-buffer output-buffer)
    (setq default-directory dir)))

;;;; Remap shortcuts to use async-shell-command by default
(global-set-key [(meta !)] 'async-shell-command)
(global-set-key [(control meta !)] 'shell-command)
;;;; dired mode added it's own key, ensure it is gone.
(require 'dired)
(define-key dired-mode-map [(meta !)] nil)

;;;; Buffer menu mode sort function
(defun buffer-list-sort (column)
  (interactive "SColumn to sort by (one of name,size,mode,file,time [default=time]): ")
  (case column
    (name (Buffer-menu-sort 2))
    (size (Buffer-menu-sort 3))
    (mode (Buffer-menu-sort 4))
    (file (Buffer-menu-sort 5))
    (t    (Buffer-menu-sort nil))))
(defun Buffer-mode-sort-key-hook ()
  (define-key Buffer-menu-mode-map (kbd "S") 'buffer-list-sort))
(add-hook 'Buffer-menu-mode-hook 'Buffer-mode-sort-key-hook)

;;;; Allow terminal colorization
(ansi-color-for-comint-mode-on)

;;;; Highlight entire expression within parens
(show-paren-mode t)
(setq show-paren-style 'expression)
(set-face-background 'show-paren-match-face "#1f3f3f")

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

;;;; Add an alias to eshell to fork off processes
(defun eshell/fork (&rest args)
  (lexical-let ((cmd ""))
    (dolist (arg args)
      (setf cmd (concat cmd " " arg)))
    (setf cmd (subseq cmd 1))
    (async-shell-command cmd)))

(global-set-key (kbd "C-x m") 'eshell)
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))
(global-set-key (kbd "C-x C-m") 'shell)

;;;; Some rcirc mode configuration
(setq rcirc-default-full-name "Matthew Curry")

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

(random t) ;; Seed the random-number generator

;;;; html-mode stuff
(define-key html-mode-map (kbd "C-M-f") 'sgml-skip-tag-forward)
(define-key html-mode-map (kbd "C-M-b") 'sgml-skip-tag-backward)
