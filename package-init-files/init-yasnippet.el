(require 'yasnippet)

(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-c y e") 'yas-expand)

(yas-global-mode)

(add-to-list 'yas/root-directory "~/.emacs.d/snippets")
(yas/reload-all)
