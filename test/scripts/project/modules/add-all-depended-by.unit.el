;; -*- lexical-binding: t -*-
(require 'buttercup)
(require 'lelde/test)
(require 'lelde/project/modules)

(describe "lelde/project/modules::modules-alist--add-all-depended-by"
  (let ((mods '((a :depended-by nil)
                (b :depended-by (a))
                (c :depended-by (b))
                (d :depended-by (c))))
        (sort-cb (lambda (a b) (string< (symbol-name a)(symbol-name b)))))
    (lelde/project/modules::modules-alist--add-all-depended-by mods)
    ;;(describe (ppp-sexp-to-string mods))
    (it "d"
      (should (equal (sort (plist-get (cdr (assq 'd mods)) :all-depended-by)
                           sort-cb)
                     (sort '(a b c)
                           sort-cb))))))

(describe "case 02"
  (let ((mods '(
                (root :depended-by nil)
                (p-i  :depended-by (root))
                (p-u  :depended-by (p-i lelde))
                (_e   :depended-by (S p-u))
                (S    :depended-by (root))
                ))
        (sort-cb (lambda (a b) (string< (symbol-name a)(symbol-name b)))))
    (lelde/project/modules::modules-alist--add-all-depended-by mods)
    (describe (ppp-sexp-to-string mods))
    (it "check result"
      (should (memq 'root (plist-get (cdr (assq 'p-u mods)) :all-depended-by)))
      )
    ))
