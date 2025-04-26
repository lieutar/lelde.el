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
  (let ((mods '((l :depended-by nil)
                (P/m :depended-by (l/e S/M/x))
                (P/U :depended-by (P/init l))
                (S/M/x :depended-by (S/M))
                (S/M :depended-by (S P/U))
                (l/cli :depended-by (l/t S l/e P/U))
                (l/e :depended-by nil) (l/init :depended-by nil)
                (P :depended-by (l/t l/test l/rsc l/e l/cli
                                     l/test/util S/M S/M/x P/U P/m P/init))
                (l/rsc :depended-by (S/M P/U P/init))
                (S :depended-by (l))
                (l/t :depended-by (P/U))))
        (sort-cb (lambda (a b) (string< (symbol-name a)(symbol-name b)))))
    (lelde/project/modules::modules-alist--add-all-depended-by mods)
    (describe (ppp-sexp-to-string mods))
    (it "check result"
      (should (memq 'l (plist-get (cdr (assq 'P/U mods))
                                  :all-depended-by))))
    ))
