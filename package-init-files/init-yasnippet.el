;; (require 'yasnippet)

;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)
;; ;; Set Yasnippet's key binding to shift+tab
;; (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)

;; (defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
;;   (when (featurep 'popup)
;;     (popup-menu*
;;      (mapcar
;;       (lambda (choice)
;;         (popup-make-item
;;          (or (and display-fn (funcall display-fn choice))
;;              choice)
;;          :value choice))
;;       choices)
;;      :prompt prompt
;;      ;; start isearch mode immediately
;;      :isearch t)))

;; (setq yas-prompt-functions
;;       '(yas-popup-isearch-prompt
;;         yas-no-prompt))
(yas-global-mode)


(add-to-list 'yas/root-directory "~/.emacs.d/snippets")
(yas/reload-all)

(eval-after-load 'company
  '(progn
     (defun check-expansion ()
       (save-excursion
         (if (looking-at "\\_>") t
           (backward-char 1)
           (if (looking-at "\\.") t
             (backward-char 1)
             (if (looking-at "->") t nil)))))

     (defun do-yas-expand ()
       (let ((yas/fallback-behavior 'return-nil))
         (yas/expand)))

     (defun tab-indent-or-complete ()
       (interactive)
       (if (minibufferp)
           (minibuffer-complete)
         (if (or (not yas/minor-mode)
                 (null (do-yas-expand)))
             (if (check-expansion)
                 (company-complete-common)
               (indent-for-tab-command)))))

     (global-set-key [tab] 'tab-indent-or-complete)))
