(global-set-key (kbd "C-:") 'ac-complete-with-helm)
(define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-helm)
(add-to-list 'ac-dictionary-directories 
             (expand-file-name "~/.ac-dict"))
;;;; ac-common-setup is called by ac-config-default
(defun ac-common-setup ()
  (add-to-list 'ac-sources 'ac-source-yasnippet))
(add-to-list 'ac-modes 'html-mode)
(add-to-list 'ac-modes 'nxml-mode)
(ac-config-default)
(defun enable-auto-complete-mode ()
  (auto-complete-mode 1))
(defun disable-auto-complete-mode ()
  (auto-complete-mode 0))
