# Lelde

Live Emacs Lisp Development Environment

`lelde` provides following solutions.

  - template feature ( by `tinplate` )
  - meta-programing using static macros ( by `stmax` )
  - module bundling ( by `elconc` )
  - test environment building

## VERSION

0.1.0

## Requirements

Lelde requires `make` and `cask` now.


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

lieutar <lieutar@gmail.com>

## LICENSE

This project is licensed under the GNU General Public License v3.
see: https://www.gnu.org/licenses/gpl-3.0.html
