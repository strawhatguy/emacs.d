;; Allow this to function outside of an emacs.d directory
(setq debug-on-error t)

(if (not (boundp 'user-emacs-directory))
    (setq user-emacs-directory "~/.emacs.d/"))

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             ;; The 't' means to append, so that MELPA comes after the more
             ;; stable ELPA archive.
             '("melpa" . "http://melpa.org/packages/") t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Switch to use-package
(message "==== Switch to use-package ====")

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages
(message "==== use-package packages ====")
(use-package ag
  :ensure t)

(use-package avy
  :ensure t
  :bind (("C-c SPC" . avy-goto-word-1)
         ("M-C-g" . avy-goto-line)
         ("C-c ." . avy-goto-char-2))
  :config
  (setq avy-background t))

(use-package bookmark+
  :ensure t)

(use-package cargo
  :ensure t)

(use-package chicken-scheme
  :ensure t
  :config
  (setq scheme-program-name "csi -:c")
  (add-hook 'scheme-mode-hook 'setup-chicken-scheme))

(use-package company
  :ensure t
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection)
              ("C-p" . company-select-previous)
              ("C-n" . company-select-next)
              ("C-d" . company-show-doc-buffer)
              ("C-v" . company-show-location))
  :config
  (add-hook 'after-init-hook
            (lambda ()
              (progn
                (global-company-mode)
                (setq company-idle-delay 0.1)
                (setq company-minimum-prefix-length 3)
                (setq company-tooltip-margin 1)
                (setq company-tooltip-minimum-width 30)))))


(use-package racer :ensure t
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package deft
  :ensure t
  :bind
  (("<f7>" . deft))
  :config
  (setq deft-directory "~/.emacs.d/.deft")
  (setq deft-extension "org")
  (setq deft-text-mode 'org-mode))

(use-package devdocs
  :bind (("C-c d p" . devdocs-search)))

(use-package edit-server
  :ensure t
  :config (edit-server-start))

(use-package elfeed
  :ensure t
  :bind ("C-x w" . elfeed)
  :config
  (defun mc/elfeed-read-lines (file)
    "Return a list of lines of FILE. Stolen from vc mode"
    (with-temp-buffer
      (insert-file-contents file)
      (split-string (buffer-string) "\n" t)))

  (defun mc/reload-elfeed-configuration ()
    (interactive)
    (setq elfeed-feeds 
          (mc/elfeed-read-lines (concat user-emacs-directory "elfeed-feeds"))))

  (mc/reload-elfeed-configuration))

(use-package expand-region
  :ensure t
  :bind (("s-=" . er/expand-region)
         ("s--" . er/contract-region)))

(use-package flycheck
             :ensure t
             :bind (:map flycheck-mode-map
                         ("C-c ! h" . helm-flycheck))
             :config
             (add-hook 'after-init-hook #'global-flycheck-mode)
             (setq-default flycheck-disabled-checkers '(javascript-jshint json-jsonlist rust-cargo))
             (flycheck-add-mode 'javascript-eslint 'js-mode)
             (flycheck-add-mode 'javascript-eslint 'js2-mode)
             (flycheck-add-mode 'javascript-eslint 'web-mode)
             )

(use-package flycheck-rust :ensure t)

(use-package geiser
  :ensure t
  :config
  (delete '("\\.rkt\\'" . scheme-mode) auto-mode-alist)
  :custom
  (geiser-active-implementations (quote (guile chicken chez mit chibi))))

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode 1))

(use-package google-this
  :ensure t)

(use-package google-translate
  :ensure t)

(use-package haskell-mode
  :ensure t
  :config
  (setq exec-path
        (cons
         (concat (getenv "HOME") "/Library/Haskell/bin")
         exec-path))

  ;; Check for hindent on the system
  (require 'haskell-interactive-mode)
  (require 'haskell-process)

  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

  (custom-set-variables
   '(haskell-process-args-cabal-repl '("--ghc-option=-ferror-spans"))
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t)
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-type 'stack-ghci)
   '(haskell-tags-on-save t)
   '(haskell-process-use-presentation-mode t))

  (setq haskell-process-path-stack
        (concat (getenv "HOME")
                "/Library/Haskell/bin/stack")))


(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-c h" . helm-command-prefix)
         ("C-x C-f" . helm-find-files)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-z"   . helm-select-action))
  :config
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

  (helm-mode 1))

(use-package helm-ag
  :ensure t)

(use-package helm-flycheck
  :ensure t)

(use-package helm-company
  :ensure t)

(use-package helm-dash
  :ensure t)

(use-package helm-projectile
  :ensure t
  :config
  (setq helm-projectile-fuzzy-match t))

(use-package helm-swoop
  :ensure t
  :bind (("M-i" . helm-swoop)))

(use-package jasminejs-mode
  :ensure t
  :config
  (add-hook 'jasminejs-mode-hook
            (lambda ()
              (jasminejs-add-snippets-to-yas-snippet-dirs)))

  (add-hook 'jasminejs-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c j") 'jasminejs-prefix-map)))

  (add-hook 'js2-mode-hook (lambda () (jasminejs-mode))))

(use-package jenkins
  :ensure t)

(use-package js2-mode
  :ensure t
  :mode ("\\.js$" . js2-mode)
  :config
  (add-hook 'js2-mode-hook
            (lambda () (subword-mode)))

  ;; Setup company mode for js2-mode
  (add-hook 'js2-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '((company-dabbrev-code company-yasnippet)))))

  (custom-set-variables
   '(js2-auto-insert-catch-block nil)
   '(js2-basic-offset 2)
   '(js2-bounce-indent-p nil)
   '(js2-mode-indent-ignore-first-tab nil))

  (eval-after-load 'js2-mode
    (progn (flycheck-mode))))

(use-package js2-refactor
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(use-package json-mode
  :ensure t
  :config
  (add-hook 'json-mode-hook
            (lambda ()
              (setq js-indent-level 2))))

(use-package less-css-mode
  :ensure t
  :config
  (setq css-indent-offset 2))

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)
         :map magit-status-mode-map
         ("q" . magit-quit-session))

  :config

  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))

  (setq magit-last-seen-setup-instructions "1.4.0")
  (define-key magit-mode-map (kbd "M-<tab>") nil)
  (define-key magit-mode-map (kbd "C-<tab>") nil)
  (define-key magit-mode-map (kbd "s-<tab>") nil)
  (setq magit-log-arguments '("--decorate")))

(use-package markdown-mode
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-c C->" . mc/mark-more-like-this-extended)))

(use-package neotree
  :ensure t
  :bind ("<f12>" . neotree-toggle)
  :config
  (setq neo-window-width 50))

(use-package nodejs-repl
  :ensure t)

(use-package nsis-mode
  :ensure t
  :config
  (setq auto-mode-alist (append '(("\\.\\([Nn][Ss][Ii]\\)$" .
                                   nsis-mode)) auto-mode-alist))

  (setq auto-mode-alist (append '(("\\.\\([Nn][Ss][Hh]\\)$" .
                                   nsis-mode)) auto-mode-alist))
  )

(use-package paredit
  :ensure t)

(use-package popup
  :ensure t
  :bind (:map popup-menu-keymap
              ("M-n" . popup-next)
              ("M-p" . popup-previous)
              ("<tab>" . popup-next)
              ("<backtab>" . popup-previous)))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action
        (lambda ()
          (if current-prefix-arg
              (magit-status)
            (helm-projectile-find-file)))))

(use-package puppet-mode
  :ensure t)

(use-package racket-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

(use-package restclient
  :ensure t)

(use-package ruby-mode
  :ensure t
  :mode (("\\.rb$" . ruby-mode)
         ("Gemfile" . ruby-mode)
         ("Rakefile" . ruby-mode)
         ("\\.rake$" . ruby-mode)))

(use-package rust-mode :ensure t
  :config
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t))

(use-package slime-company
  :ensure t)

(use-package slime
  :ensure t
  :config
  (setq slime-lisp-implementations
        '((sbcl ("/usr/local/bin/sbcl"))
          (ecl ("/usr/local/bin/ecl"))
          (clisp ("/usr/local/bin/clisp"))))
  (slime-setup '(
                 slime-asdf
                 slime-autodoc
                 slime-company
                 slime-editing-commands
                 slime-fancy
                 slime-fancy-inspector
                 slime-fontifying-fu
                 slime-fuzzy
                 slime-indentation
                 slime-mdot-fu
                 slime-package-fu
                 slime-references
                 slime-repl
                 slime-sbcl-exts
                 slime-scratch
                 slime-xref-browser
                 ))
  (slime-autodoc-mode))

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (sp-use-paredit-bindings))

(use-package switch-window
  :ensure t)

(use-package twittering-mode
  :ensure t
  :config
  (setq twittering-use-master-password t))

(use-package vkill
  :ensure t)

(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" . web-mode)
  :config
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

(use-package wgrep
  :ensure t
  :bind (("C-x C-g". wgrep-change-to-wgrep-mode))
  :config
  (setq wgrep-auto-save-buffer t))

(use-package xcscope
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :demand t
  :bind
  (:map yas-minor-mode-map
        ("<tab>" . nil)
        ("TAB" . nil)
        ("C-c y e" . yas-expand))
  :config
  (yas-global-mode)
  (define-key yas-keymap (kbd "<return>") 'yas-next-field)
  (add-to-list 'yas/root-directory "~/.emacs.d/snippets")
  (yas-reload-all))

(use-package yatemplate
  :ensure t
  :demand t
  :init (auto-insert-mode)
  :config (yatemplate-fill-alist))

(use-package zenburn-theme
  :ensure t)

(defun mc/reload-all-user-initialization-files ()
  (interactive)
  (mapcar (lambda (f)
            (message "loading %s" f)
            (load-file f))
          (file-expand-wildcards (concat user-emacs-directory "init/*.el"))))

(mc/reload-all-user-initialization-files)

(message "My emacs directory is: %s" user-emacs-directory)

(setq debug-on-error nil)
