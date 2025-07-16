;;;; custom functions here

(require 'cl)

(defun mc/advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

;;;; vert <-> horiz split switch
(defun mc/toggle-window-split ()
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

;;;; switching between buffers backwards
(defun mc/other-window-backwards (&optional count all-frames)
  (interactive)
  (other-window (if count (- count) -1) all-frames))

;;;; set F5 key to revert-buffer
(defun mc/reset-buffer ()
  "Resets a file-buffer reflect the file on disk, resetting modes"
  (interactive) (revert-buffer nil t nil))

;;;; Ask for directory of the project to compile
(defun mc/compile-asking-directory (top-level)
  (interactive "DProject toplevel directory: ")
  (let ((default-directory top-level)
        (command (read-string "Command: " (or compile-command "make -k"))))
    (set (make-local-variable 'compile-command) command)
    (compile command)))

;;;; Compile from project root
(defun mc/compile-from-project-root ()
  (interactive)
  (mc/compile-asking-directory (projectile-project-root)))

;;;; color the compilation buffer
(require 'ansi-color)
(defun mc/colorize-compilation-buffer ()
  (read-only-mode)
  (ansi-color-apply-on-region (point-min) (point-max))
  (read-only-mode -1))

;;;; Advise the shell commands to name the buffer after the command itself
(defadvice async-shell-command (before buffer-named-with-command
                                       (command &optional output-buffer error-buffer)
                                       activate compile)
  (setq output-buffer (or output-buffer (concat "*Async: " command "*")))
  (let ((dir default-directory))
    (switch-to-buffer output-buffer)
    (setq default-directory dir)))

;;;; Buffer menu mode sort function
(defun mc/buffer-list-sort (column)
  (interactive "SColumn to sort by (one of name,size,mode,file,time [default=time]): ")
  (case column
    (name (tabulated-list-sort 3))
    (size (tabulated-list-sort 4))
    (mode (tabulated-list-sort 5))
    (file (tabulated-list-sort 6))
    (t    (tabulated-list-sort -1))))
(defun mc/Buffer-mode-sort-key-hook ()
  (define-key Buffer-menu-mode-map (kbd "S") 'mc/buffer-list-sort))

;;;; cycle fonts
(lexical-let ((mc/font-sizes '("12" "14" "16" "20" "24")))
  (defun mc/cycle-font-size ()
    "Cycle between 14, 16, 18, 24 pt fonts"
    (interactive)
    (let* ((size (car mc/font-sizes))
           (font (concat "Monaco-" size)))
      (progn (setq mc/font-sizes (-rotate -1 mc/font-sizes))
             (set-frame-font font)))))

;;;; Indent after pasting (yanking) and killing text
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode
                                                     clojure-mode    scheme-mode
                                                     haskell-mode    ruby-mode
                                                     rspec-mode      python-mode
                                                     c-mode          c++-mode
                                                     objc-mode       latex-mode
                                                     plain-tex-mode  js2-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

(defadvice kill-line (before check-position activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode
                                c-mode c++-mode objc-mode
                                latex-mode plain-tex-mode
                                js2-mode))
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))

(defun mc/clear-text-read-only-whole-buffer ()
  (interactive)
  (let ((inhibit-read-only t))
    (remove-text-properties (point-min) (point-max) '(read-only nil))))

(defun mc/projectile-run-etags ()
  (interactive)
  (let ((old-dir (pwd)))
    (cd (projectile-project-root))
    (shell-command "rm -f TAGS && find . -type f -print0 | xargs -0 etags -a")
    (cd old-dir)))

(defun mc/locally-tabs-mode ()
  (interactive)
  (setq-local indent-tabs-mode t)
  (setq-local c-basic-offset 2)
  (setq-local tab-width 2))

(defun mc/add-js-tab-hooks ()
  (interactive)
  (add-hook 'js2-mode-hook 'mc/locally-tabs-mode)
  (add-hook 'json-mode-hook 'mc/locally-tabs-mode))

(defun mc/rem-js-tab-hooks ()
  (interactive)
  (remove-hook 'js2-mode-hook 'mc/locally-tabs-mode)
  (remove-hook 'json-mode-hook 'mc/locally-tabs-mode))

(defun mc/add-java-tab-hooks ()
  (interactive)
  (add-hook 'java-mode-hook 'mc/locally-tabs-mode)
  (add-hook 'nxml-mode-hook 'mc/locally-tabs-mode))

(defun mc/rem-java-tab-hooks ()
  (interactive)
  (remove-hook 'java-mode-hook 'mc/locally-tabs-mode)
  (remove-hook 'nxml-mode-hook 'mc/locally-tabs-mode))

(defun mc/add-rfc3339-zulu-time-at-point ()
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%S.%NZ" nil "Z")))

(defun mc/unixms2rfc3339 (ms)
  (interactive (list (read-number "millisecs: " (thing-at-point 'number) 'mc/unixms2rfc3339-history)))
  (let ((s (format-time-string "%Y-%m-%dT%H:%M:%S.%NZ" (seconds-to-time (/ ms 1000)) t)))
    (when (called-interactively-p 'any)
      (message s)
      (kill-new s))
    s))

(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

;; from: https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/#h:1e468b2a-9bee-4571-8454-e3f5462d9321
(defun mc/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))
