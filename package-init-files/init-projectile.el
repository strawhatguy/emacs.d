(require 'projectile)
(projectile-global-mode)
(eval-after-load 'helm
  '(progn
     (require 'helm-projectile)
     (setq projectile-completion-system 'helm)
     (helm-projectile-on)))

(eval-after-load 'magit
  '(eval-after-load 'helm
     '(setq projectile-switch-project-action
            (lambda ()
              (if current-prefix-arg
                  (magit-status default-directory)
                (helm-projectile-find-file))))))
