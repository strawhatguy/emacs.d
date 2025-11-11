;; -*- lexical-binding: t; -*-
;;;; Initial editor decorations (or lack thereof)

;;;; GNU really should requre cl by default, but since stallman hates
;;;; the common lisp guys, they invented reasons not to use it.  put
;;;; it first thing here.
(require 'cl)

;;;; Use every pixel of available space
(when (functionp 'menu-bar-mode)
  (menu-bar-mode   -1))
(when (functionp 'tool-bar-mode)
  (tool-bar-mode   -1))
(when (functionp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(setq split-height-threshold 80)
(setq split-width-threshold  240)

(diminish 'eldoc-mode)
(diminish 'auto-revert-mode)
