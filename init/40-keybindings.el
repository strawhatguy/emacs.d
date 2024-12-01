;;;; custom key bindings go here

;;;; window splitting
(global-set-key [f9]   'mc/toggle-window-split)

;;;; switching buffers
(global-set-key [(control tab)] 'other-window)
(global-set-key [(control shift iso-lefttab)] 'mc/other-window-backwards)

;;;; force C-tab to be other-window even in org-mode
(require 'org)
(define-key org-mode-map [(control tab)] nil)
(define-key org-mode-map (kbd "C-c SPC") nil)
;;;; other org-mode stuff
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;;;; navigation
(global-set-key [(meta g)] 'goto-line)

;;;; buffer handling
(global-set-key [f5] 'mc/reset-buffer)
(global-set-key [C-f5] 'mc/clear-text-read-only-whole-buffer)

;;;; compiling
(global-set-key [f8]   'recompile)
(global-set-key [S-f8] 'mc/compile-from-project-root)
(global-set-key [C-S-f8] 'mc/compile-asking-directory)

;;;; Make C-h C-s go to apropos (basically apropos-symbol)
(global-set-key [(control h) (control s)] 'apropos)

;;;; Remap shortcuts to use async-shell-command by default
(global-set-key [(meta !)] 'async-shell-command)
(global-set-key [(control meta !)] 'shell-command)

;;;; dired mode added it's own key, ensure it is gone.
(require 'dired)
(define-key dired-mode-map [(meta !)] nil)

;;;; shell shortcuts
(global-set-key (kbd "C-x m") 'eshell)
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))
(global-set-key (kbd "C-x C-m") 'shell)

;;;; cycle fonts
(global-set-key [f2] 'mc/cycle-font-size)

;;;; toggle whitespace mode
(global-set-key (kbd "C-c C-w") 'whitespace-mode)

;;;; fix html-mode defaults
(require 'sgml-mode)
(define-key html-mode-map (kbd "C-M-f") 'sgml-skip-tag-forward)
(define-key html-mode-map (kbd "C-M-b") 'sgml-skip-tag-backward)


(define-key global-map (kbd "C-g") #'mc/keyboard-quit-dwim)
