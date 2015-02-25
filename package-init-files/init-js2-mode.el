(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'"   . js2-mode))
(set-default 'js2-basic-offset 2)
(set-default 'js2-mirror-mode  nil)
(set-default 'js2-mode-escape-quotes  nil)
(define-key js2-mode-map (kbd "C-M-p") 
  (lambda () 
    (interactive) 
    (js2-beginning-of-defun)))
(define-key js2-mode-map (kbd "C-M-n") 
  (lambda () 
    (interactive) 
    (js2-end-of-defun)))
(define-key js2-mode-map (kbd "C-c m") 'js2-mark-defun)

(eval-after-load 'js2-mode
  '(progn
     (defun wrap-in-js-function ()
       (interactive)
       (save-excursion
         (unless (use-region-p)
           (push-mark)
           (end-of-line))
         (save-restriction
           (narrow-to-region (region-beginning) (region-end))
           (goto-char (point-min))
           (insert "function (arg) {")
           (goto-char (point-max))
           (insert "}")
           (goto-char (point-min))
           (search-forward "(arg)" nil t))))

     (defun unwrap-js-function ()
       (interactive)
       (save-excursion
         (js2-mark-defun)
         (save-restriction
           (narrow-to-region (region-beginning) (region-end))
           (goto-char (point-min))
           (zap-to-char 1 ?\{)
           (goto-char (point-max))
           (zap-to-char -1 ?\}))))

     (define-key js2-mode-map (kbd "C-c w f") 'wrap-in-js-function)
     (define-key js2-mode-map (kbd "C-c u f") 'unwrap-js-function)))
