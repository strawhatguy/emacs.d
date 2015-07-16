(require 'grep)
(define-key grep-mode-map (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode)
(setq wgrep-auto-save-buffer t)
