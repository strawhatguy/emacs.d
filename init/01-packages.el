(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Switch to use-package
(message "==== Switch to use-package ====")

(package-initialize)
(package-refresh-contents)

(unless (package-installed-p 'use-package)
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

(use-package cargo
  :ensure t)

(use-package chicken-scheme
  :ensure t
  :config
  (setq scheme-program-name "csi -:c")
  (add-hook 'scheme-mode-hook 'setup-chicken-scheme))

(use-package cider
  :ensure t
  :config
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion))

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

(defun find-rust-src-racer-hook ()
  (eldoc-mode)
  (let* ((cmd (concatenate 'string  (executable-find "rustc") " --print sysroot"))
         (res (s-trim (shell-command-to-string cmd)))
         (src (concatenate 'string res "/lib/rustlib/src/rust/src")))
    (setq racer-rust-src-path src)))

(use-package racer :ensure t
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'find-rust-src-racer-hook))

(use-package deft
  :ensure t
  :bind
  (("<f7>" . deft))
  :config
  (unless (file-exists-p "~/Notes")
    (mkdir "~/Notes"))
  (setq deft-directory "~/Notes")
  (setq deft-extensions '("org" "md"))
  (setq deft-default-extension "org"))

(use-package devdocs
  :ensure t
  :bind (("C-c d p" . devdocs-search)))

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

(use-package exec-path-from-shell
  :ensure t)

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

(use-package helm-ag :ensure t)
(use-package helm-flycheck :ensure t)
(use-package helm-cider :ensure t)
(use-package helm-company :ensure t)
(use-package helm-dash :ensure t)
(use-package helm-lsp :ensure t)

(use-package helm-projectile
  :ensure t
  :config
  (setq helm-projectile-fuzzy-match t))

(use-package helm-swoop
  :ensure t
  :bind (("M-i" . helm-swoop)))

(use-package indium
  :config
  (add-hook 'js2-mode-hook #'indium-interaction-mode))

(use-package jasminejs-mode
  :ensure t
  :config
  (add-hook 'jasminejs-mode-hook
            'jasminejs-add-snippets-to-yas-snippet-dirs)

  (add-hook 'jasminejs-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c j") 'jasminejs-prefix-map)))

  (add-hook 'js2-mode-hook 'jasminejs-mode))

(use-package jenkins
  :ensure t)

(use-package js2-mode
  :ensure t
  :mode ("\\.js$" . js2-mode)
  :bind (:map js2-mode-map
              ("M-." . nil))
  :config
  (add-hook 'js2-mode-hook
            (lambda () (subword-mode)))

  ;; Setup company mode for js2-mode
  (add-hook 'js2-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '((company-dabbrev-code company-yasnippet)))))

  (eval-after-load 'js2-mode
    (progn (flycheck-mode))))

(use-package js2-refactor
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(use-package xref-js2
  :ensure t
  :config
  (add-hook 'js2-mode-hook (lambda ()
                             (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

(use-package json-mode
  :ensure t
  :config
  (add-hook 'json-mode-hook
            (lambda ()
              (setq js-indent-level 2))))

(use-package lsp-mode :commands lsp :ensure t
  :config
  (setq lsp-prefer-flymake nil)
  (add-hook 'js2-mode-hook 'lsp))

(use-package lsp-ui :ensure t :after lsp
  :config
  (add-hook 'lsp-after-open-hook (lambda () (lsp-ui-flycheck-enable 1))))
(use-package lsp-java :ensure t :after lsp
  :config (add-hook 'java-mode-hook 'lsp))

(use-package dap-mode
  :ensure t :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package company-lsp :ensure t :after company lsp)
(use-package dap-java :after lsp-java)
(use-package lsp-java-treemacs :after treemacs)

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
  (define-key magit-mode-map (kbd "s-<tab>") nil))

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
  (projectile-mode 1)
  (setq projectile-completion-system 'helm)
  (setq projectile-generic-command "fd . -0")
  (setq projectile-switch-project-action
        (lambda ()
          (if current-prefix-arg
              (magit-status)
            (helm-projectile-find-file))))
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package puppet-mode
  :ensure t)

(use-package racket-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

(use-package restclient
  :ensure t)

(use-package ripgrep
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

(use-package s
  :ensure t)

(use-package slime-company
  :ensure t)

(use-package slime
  :ensure t
  :config
  (setq slime-lisp-implementations
        '((sbcl ("/usr/local/bin/sbcl"))
          (abcl ("/usr/local/bin/abcl"))
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

(use-package spaceline
  :ensure t
  :config
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode t)
  (spaceline-info-mode t))

(use-package switch-window
  :ensure t)

(use-package treemacs
  :ensure t
  :defer t
  :bind
  (:map global-map
        ("C-x t s"   . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("<f12>"     . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package twittering-mode
  :ensure t
  :config
  (setq twittering-use-master-password t))

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
