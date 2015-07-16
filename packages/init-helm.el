(require 'helm)
(require 'helm-config)

(global-set-key (kbd "M-x") 'helm-M-x)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-quick-update                     t
      helm-split-window-in-side-p           t
      helm-buffers-fuzzy-matching           t
      helm-recentf-fuzzy-match              t
      helm-locate-fuzzy-match               t
      helm-M-x-fuzzy-match                  t
      helm-semantic-fuzzy-match             t
      helm-apropos-fuzzy-match              t
      helm-imenu-fuzzy-match                t
      helm-lisp-fuzzy-completion            t
      helm-move-to-line-cycle-in-source     t
      helm-scroll-amount                    8
      helm-ff-search-library-in-sexp        t
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

