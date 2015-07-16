;;;; scheme program, use chicken
(setq scheme-program-name "csi -:c")

;;;; chicken-scheme setup
(require 'chicken-scheme)
(add-hook 'scheme-mode-hook 'setup-chicken-scheme)
(define-key scheme-mode-map (kbd "C-?") 'chicken-show-help)
