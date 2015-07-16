(add-hook 'jasminejs-mode-hook
          (lambda ()
            (jasminejs-add-snippets-to-yas-snippet-dirs (concat user-emacs-directory
                                                                "el-get/jasminejs-mode/snippets/"))
            (local-set-key (kbd "C-c j") 'jasminejs-prefix-map)))

(add-hook 'js2-mode-hook 'jasminejs-mode)
