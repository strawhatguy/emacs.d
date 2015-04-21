(require 'projectile)
(projectile-global-mode)
(eval-after-load 'helm
  '(progn
     (require 'helm-projectile)
     (setq projectile-completion-system 'helm)
     (helm-projectile-on)))

(eval-after-load 'magit
  (setq projectile-switch-project-action
        (lambda ()
          (magit-status default-directory))))
