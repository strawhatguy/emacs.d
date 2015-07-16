;;;; Initial editor decorations (or lack thereof)

;;;; GNU really should requre cl by default, but since stallman hates
;;;; the common lisp guys, they invented reasons not to use it.  put
;;;; it first thing here.
(require 'cl)

;;;; Use every pixel of available space
(menu-bar-mode   -1)
(tool-bar-mode   -1)
(scroll-bar-mode -1)
