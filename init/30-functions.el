;;;; custom functions here

;;;; vert <-> horiz split switch
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

;;;; switching between buffers backwards
(defun other-window-backwards (&optional count all-frames)
  (interactive)
  (other-window (if count (- count) -1) all-frames))

;;;; set F5 key to revert-buffer
(defun reset-buffer ()
  "Resets a file-buffer reflect the file on disk, resetting modes"
  (interactive) (revert-buffer nil t nil))

;;;; Ask for directory of the project to compile
(defun compile-asking-directory (top-level)
  (interactive "DProject toplevel directory: ")
  (let ((default-directory top-level))
    (call-interactively 'compile)))

;;;; color the compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))

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

;;;; cycle fonts
(lexical-let ((mc/font-sizes '( "12" "14" "16" "20" "24")))
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