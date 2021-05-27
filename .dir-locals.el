;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((purescript-mode
  (purty-use-npm-bin . t)
  (eval load-file
        (expand-file-name "emacs/purty.el"
                          (locate-dominating-file
                           (buffer-file-name)
                           ".dir-locals.el")))
  (mode . purty-on-save)
  (psc-ide-use-npm-bin . t)))
