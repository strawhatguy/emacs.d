(require 'projectile)
(projectile-global-mode)
(eval-after-load 'helm
  '(progn
     (require 'helm-projectile)
     (helm-projectile-on)))
