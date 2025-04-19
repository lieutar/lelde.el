# Lelde

{{brief}}

{{description}}

## VERSION

{{version}}

## Requirements

Lelde requires `make` and `cask`.
But this dependency is just temporary.

In the nearest future, lelde will use eask instead of them.


## Start your project with Lelde

``` bash
curl -fsSL https://raw.githubusercontent.com/lieutar/lelde/main/init.sh | bash

```

or install:

``` bash
curl -fsSL https://raw.githubusercontent.com/lieutar/lelde/main/init.sh > ~/bin/lelde-init

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
