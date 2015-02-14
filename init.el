;;;; Load vanilla-emacs config
(load "~/.emacs.d/init-base-emacs-configuration")

;;;; Install el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;;;; local recipes
(add-to-list 'el-get-recipe-path "~/.emacs.d/recipes")

;;;; init-<package>.el files
(setq el-get-user-package-directory "~/.emacs.d/package-init-files")

;;;; install these packages by default
(setq my-packages
      '(
        ;; ac-slime ;;;; slime doesn't build
        ac-helm
        ac-js2
        ace-jump-mode
        auto-complete
        clojure-mode
        coffee-mode
        color-theme-zenburn
        cssh
	deft
        dsvn
        el-get
        elnode
        expand-region
        flycheck
        google-maps
        google-this
        google-translate
        haskell-mode
	helm
        ido-ubiquitous
        js2-mode
        julia-mode
        markdown-mode
        magit
        multiple-cursors
        nodejs-repl
        ;; nxhtml
        oddmuse
        puppet-mode
	paredit
        restclient
        ruby-compilation
        smex
        switch-window
        twittering-mode
        vkill
        wgrep
        xcscope
        yaml-mode
        yasnippet
        ))

(el-get 'sync my-packages)
