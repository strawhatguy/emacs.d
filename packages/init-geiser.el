(setq geiser-implementations-alist
      '(((regexp "\\.scm$") chicken)
        ((regexp "\\.scm$") guile)
        ((regexp "\\.ss$") racket)
        ((regexp "\\.rkt$") racket)
        ((regexp "\\.release-info$") chicken)
        ((regexp "\\.meta$") chicken)
        ((regexp "\\.setup$") chicken)))
