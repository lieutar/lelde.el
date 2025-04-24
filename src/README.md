# Lelde

{{brief}}

{{(commentary)(s-replace-regexp "\\(`[^']+\\)'" "\\1`" commentary)}}

## VERSION

{{version}}

## Requirements

Lelde requires `make` and `cask` now.


## Start your project with Lelde

``` bash
curl -fsSL https://raw.githubusercontent.com/lieutar/lelde.el/refs/heads/main/init.sh | bash

```

or install:

``` bash
curl -fsSL https://raw.githubusercontent.com/lieutar/lelde.el/refs/heads/main/init.sh > ~/bin/lelde-init

```

## The Lelde file

`your-project/Lelde` is a eld file that describes your project's meta information.
`lelde` makes and updates some files on your project.

``` emacs-lisp
(lelde-project
  :name  your-project
  :index your-project
)
```


## AUTHOR

{{author}}

## LICENSE

{{license}}
