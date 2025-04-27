;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'f)
(require 'lelde/project/modules)
(require 'lelde/test)

(lelde/test::test-setup)
(require 'lelde-test)

(describe "lelde/project/modules"
  (let (
        mods
        slot
        )
    (lelde-test/with-repos "project--modules/p0000"
      (setq mods
            (lelde/project/modules::get-modules-alist ".")))
    ;;(describe (ppp-sexp-to-string mods))
    (it "got modules alist"
      (should (eq 6 (length mods)))
      (should (assq 'p0000 mods))
      (should (assq 'p0000/META mods))
      (should (assq 'p0000/core mods))
      (should (assq 'p0000/core/isolated mods))
      (should (assq 'p0000/core/sub-b mods))
      (should (assq 'p0000/core/sub mods)))

    (it "query"
      (let ((result (lelde/project/modules::query-internal-dependencies
                     mods 'p0000)))
        (should (equal (-map 'car result)
                       '(p0000/META
                         p0000/core/sub-b p0000/core/sub p0000/core)))
        ))
    ))
