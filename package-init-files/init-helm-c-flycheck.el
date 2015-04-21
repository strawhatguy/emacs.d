(eval-after-load 'helm
  '(progn (require 'helm-flycheck)
          (eval-after-load 'flycheck
            '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))))
