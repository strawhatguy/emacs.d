;;;; theme changes here, although zenburn is pretty nice already

;;;; set font
(ignore-errors (set-frame-font "Monaco-12"))

;;;; Allow terminal colorization
(ansi-color-for-comint-mode-on)

;;;; Highlight entire expression within parens
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

;;;; show the end
(set-default 'indicate-empty-lines t)
