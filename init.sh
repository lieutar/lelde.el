#! /usr/bin/bash

if ! which cask >/dev/null 2>&1; then
    cat <<EOF >&2
Lelde requires cask.
see: https://cask.readthedocs.io/en/latest/guide/installation.html
EOF
    exit 1
fi

if ! which make >/dev/null 2>&1; then
    cat <<EOF >&2
Lelde requires make.
EOF
    exit 1
fi


cat <<EOF >Cask
(source gnu)
(source melpa)
(depends-on "f" "20241003.1131")
(depends-on "s" "20220902.1511")
(depends-on "dash" "20240510.1327")
(depends-on "ppp" "20220211.1529")
;; cl-lib (core)
(depends-on "prinfo" :git "file:///home/lieutar/work/emacs/prinfo.el")
(depends-on "stmax" :git "file:///home/lieutar/work/emacs/stmax.el")
(depends-on "elconc" :git "file:///home/lieutar/work/emacs/elconc.el")
(depends-on "tinplate" :git "file:///home/lieutar/work/emacs/tinplate.el")

(depends-on "lelde" :git "file:///home/lieutar/work/emacs/lelde.el")
EOF

cask install
cask exec emacs --batch -l lelde -f lelde-init-package
