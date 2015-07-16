(require 'paredit)
(define-key paredit-mode-map (kbd "C-j") nil)
(define-key paredit-mode-map (kbd "C-M-n") 'paredit-forward)
(define-key paredit-mode-map (kbd "C-M-p") 'paredit-backward)
(define-key paredit-mode-map (kbd "C-M-f") 'paredit-forward-up)
(define-key paredit-mode-map (kbd "C-M-b") 'paredit-backward-down)
(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
(add-hook 'ielm-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook           'enable-paredit-mode)
(add-hook 'inferior-scheme-mode-hook  'enable-paredit-mode)

;;;; Advice for ielm-mode
(defadvice ielm-eval-input (after ielm-paredit activate)
  "Begin each IELM prompt with a ParEdit parenthesis pair."
  (paredit-open-round))
