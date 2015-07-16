(eval-after-load 'company
  '(eval-after-load 'helm
     '(progn
        (define-key company-mode-map (kbd "C-:") 'helm-company)
        (define-key company-active-map (kbd "C-:") 'helm-company))))
