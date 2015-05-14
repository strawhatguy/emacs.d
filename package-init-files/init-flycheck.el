(require 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)

(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

(setq flycheck-temp-prefix ".#flycheck")
