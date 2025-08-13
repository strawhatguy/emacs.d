;;; -*- lexical-binding: t -*-
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
  (setq cider-stacktrace-frames-background-color (cider-scale-background-color)))

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

(use-package direnv :ensure t
  :config (direnv-mode))

(use-package dockerfile-mode :ensure t)

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
  (setq eglot-code-action-indications '(eldoc-hint))
  (setq eglot-code-action-indicator "⚡")
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

(use-package ellama
  :init
  (setopt ellama-keymap-prefix "s-l")
  )

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

(use-package git-link :ensure t)

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

;; Enable Vertico.
(use-package vertico
  :ensure t
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure t
  :init
  (savehist-mode))

;; Example configuration for Consult
(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  :bind
  (:map corfu-map
        ("TAB" . corfu-insert)
        ([tab] . corfu-insert))

  :init

  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)

  ;; Enable optional extension modes:
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  )

;; Add extensions
(use-package cape
  :ensure t
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  )

;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :custom
  (orderless-matching-styles '(orderless-initialism orderless-literal))
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))


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
  (setq projectile-generic-command "fd . -0")
  :bind (("M-!" . projectile-run-async-shell-command-in-root)
         :map projectile-mode-map
         ("C-c p" . projectile-command-map)))

(use-package puppet-mode
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

(use-package rust-mode :ensure t)

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
