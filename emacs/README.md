# Purty Setup/TroubleShooting

1. Make sure you have the packages `reformatter` and `purescript-format` in your
`dotspacemacs-additional-packages` or purty won't run correctly. It should look something like below
```lisp
dotspacemacs-additional-packages '( reformatter purescript-format )
```

2. Ensure that you don't have any other `.dir-locals.el` files further down the
directory structure: i.e: in `web` or `web/purs` or deeper. If you have a `.dir-locals.el`
in any of those deeper paths, the dir-locals in the base wildcat folder will be ignored, causing
purty to not be loaded. Perhaps we can set a stronger purty-loading function later on.

You can check for the existance of other dir-locals files using the command
`find path/to/wildcat -name ".dir-locals*"`
