(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

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

(use-package avy
  :ensure t
  :bind (("C-." . avy-goto-word-1))
  :config
  (setq avy-background t))

(use-package cargo
  :ensure t)

(use-package cider
  :ensure t
  :config
  (setq cider-repl-display-help-banner nil)
  ;;;; correct for cider's error buffer
  (setq cider-stacktrace-frames-background-color (cider-scale-background-color))
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion))

(use-package company
  :ensure t
  :diminish company-mode
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection)
              ("C-p" . company-select-previous)
              ("C-n" . company-select-next)
              ("C-d" . company-show-doc-buffer)
              ("C-v" . company-show-location))
  :config
  (setq company-global-modes '(not eshell-mode))
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 3)
  (setq company-tooltip-margin 1)
  (setq company-tooltip-minimum-width 30)
  (global-company-mode))

(use-package counsel :ensure t)
(use-package counsel-projectile :ensure t)
(use-package counsel-osx-app :ensure t)

(defun find-rust-src-racer-hook ()
  "Find the rustc system path."
  (let* ((cmd (concatenate 'string  (executable-find "rustc") " --print sysroot"))
         (res (s-trim (shell-command-to-string cmd)))
         (src (concatenate 'string res "/lib/rustlib/src/rust/src")))
    (setq racer-rust-src-path src)))

(use-package racer :ensure t
  :diminish racer-mode
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'find-rust-src-racer-hook))

(use-package deadgrep
  :ensure t
  :bind (("C-c d g" . deadgrep)
         :map deadgrep-mode-map
         ("RET" . #'deadgrep-visit-result-other-window)
         ("M-RET" . #'deadgrep-visit-result)))

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

(use-package direnv :ensure t
  :config (direnv-mode))

(use-package dockerfile-mode :ensure t)

(use-package edn :ensure t)

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

(use-package eglot :ensure t
  :config
  (setq eglot-autoshutdown t)
  (setq eglot-send-changes-idle-time 1)
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider))
  :bind
  (("C-c e e" . eglot)
   ("C-c e l" . flymake-show-buffer-diagnostics)
   ("C-c e n" . flymake-goto-next-error)
   ("C-c e p" . flymake-goto-prev-error)
   :map eglot-mode-map
   ("C-c e a" . eglot-code-actions)
   ("C-c e f" . eglot-format)
   ("C-c e t" . eglot-reconnect)
   ("C-c e x" . eglot-shutdown)
   ("C-c e r" . eglot-rename)))

(use-package exec-path-from-shell
  :ensure t)

(use-package expand-region
  :ensure t
  :bind (("s-=" . er/expand-region)
         ("s--" . er/contract-region)))

(use-package fd-dired :ensure t)

(use-package geiser
  :ensure t
  :config
  (delete '("\\.rkt\\'" . scheme-mode) auto-mode-alist)
  :custom
  (geiser-active-implementations (quote (guile chicken chez mit chibi))))

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode 1))

(use-package go-mode :ensure t)
(use-package go-dlv :ensure t)
(use-package go-playground :ensure t)

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

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind
  (("C-s" . 'swiper-isearch)
   ("M-i" . 'swiper-thing-at-point)
   ("M-x" . 'counsel-M-x)
   ("C-x C-f" . 'counsel-find-file)
   ("M-y" . 'counsel-yank-pop)
   ("C-h f" . 'counsel-describe-function)
   ("C-h v" . 'counsel-describe-variable)
   ("C-h l" . 'counsel-find-library)
   ("C-h S" . 'counsel-info-lookup-symbol)
   ("C-h u" . 'counsel-unicode-char)
   ("C-h V" . 'counsel-set-variable)
   ("C-x b" . 'ivy-switch-buffer))

  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "")
  (setq ivy-initial-inputs-alist nil))

(use-package jenkins
  :ensure t)

(use-package jest :ensure t)

(use-package js2-mode
  :ensure t
  :hook (js2-mode . subword-mode)
  :bind (:map js2-mode-map
              ("M-." . nil))
  :config
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil))

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

(use-package diminish :ensure t)

(use-package less-css-mode
  :ensure t
  :config
  (setq css-indent-offset 2))

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status))

  :config
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

(use-package neotree :ensure t
  :bind (("<f12>" . neotree-toggle)
         ("M-<f12>" . neotree-dir)))

(use-package nodejs-repl
  :ensure t)

;; (use-package nsis-mode
;;   :ensure t
;;   :config
;;   (setq auto-mode-alist (append '(("\\.\\([Nn][Ss][Ii]\\)$" .
;;                                    nsis-mode)) auto-mode-alist))

;;   (setq auto-mode-alist (append '(("\\.\\([Nn][Ss][Hh]\\)$" .
;;                                    nsis-mode)) auto-mode-alist)))

(use-package nix-mode :ensure t)

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
  :diminish projectile-mode
  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'ivy)
  (setq projectile-generic-command "fd . -0")
  (setq projectile-switch-project-action 'counsel-projectile-find-file)
  :bind (("M-!" . projectile-run-async-shell-command-in-root)
         :map projectile-mode-map
         ("C-c p" . projectile-command-map)))

(use-package puppet-mode
  :ensure t)

(use-package racket-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

(use-package restclient
  :ensure t
  :mode ("\\.restclient$" . restclient-mode))

(use-package ripgrep
  :ensure t)

(use-package rjsx-mode :ensure t
  :mode ("\\.js$" . rjsx-mode))

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

;;;; Emacs server (allow emacsclient to connect to running session)
(use-package server
  :ensure nil
  :defer 1
  :config
  (setq server-client-instructions nil)
  (unless (server-running-p)
    (server-start)))

(use-package s
  :ensure t)

(use-package sly :ensure t
  :config
  ;; mac brew location
  (setq common-lisp-hyperspec-root
        "/usr/local/share/doc/hyperspec/HyperSpec/")
  (setq common-lisp-hyperspec-symbol-table
        (concat common-lisp-hyperspec-root "Data/Map_Sym.txt"))
  (setq common-lisp-hyperspec-issuex-table
        (concat common-lisp-hyperspec-root "Data/Map_IssX.txt")))
(use-package sly-asdf :ensure t)
(use-package sly-quicklisp :ensure t)

(use-package spaceline
  :ensure t
  :config
  (spaceline-spacemacs-theme)
  (spaceline-info-mode t))

(use-package string-inflection :ensure t
  :bind
  (:map global-map
        ("C-c i" . string-inflection-all-cycle)
        ("C-c U" . string-inflection-underscore)
        ("C-c K" . string-inflection-kebab-case)
        ("C-c C" . string-inflection-lower-camelcase)
        ("C-c P" . string-inflection-camelcase)))

(use-package sunshine
  :ensure t
  :config
  (setq sunshine-location "98155,USA")
  (setq sunshine-appid "69d6117651e8b0fca467e95d6b576ba3"))

(use-package switch-window :ensure t)

(use-package typescript-mode :ensure t)

(use-package twittering-mode
  :ensure t
  :config
  (setq twittering-use-master-password t))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (setq undo-tree-incompatible-major-modes '(term-mode eshell-mode))
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package unicode-fonts
  :ensure t
  :config
  (unicode-fonts-setup))

(use-package vterm
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

(use-package xcscope :ensure t)

(use-package xkcd :ensure t)

(use-package yaml-mode :ensure t)

(use-package yasnippet :ensure t)

(use-package vue-mode :ensure t)
(use-package vue-html-mode :ensure t)

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))
