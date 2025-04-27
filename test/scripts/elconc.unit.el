;; -*- lexical-binding: t -*-
(require 'buttercup)
(require 'lelde/elconc)
(require 'lelde/test)
(require 'slash-tmp)
(require 'f)

(lelde/test::test-setup)
(require 'lelde-test)

(describe "lelde/elconc"
  (let (
        content
        )
    (lelde-test/with-repos "elconc/lelde.el.pss"
      (lelde/elconc::bundle "src/lelde.el" "lelde.el")
      (setq content (f-read "lelde.el"))
      )

    (describe content)
    (it "mod-alist"
      (with-temp-buffer
        (insert content)
        (dolist (f '(  lelde/META
                        lelde/project/depends
                        lelde/project
                        lelde/rsc
                        lelde/cli
                        lelde/tinplate/util
                        lelde/tinplate
                        lelde/project/modules
                        lelde/stmax/emit/export
                        lelde/stmax/emit
                        lelde/project/update
                        lelde/project/init
                        lelde/elconc
                        lelde/stmax ))
          (goto-char (point-min))
          (should (re-search-forward (format "^;;+[[:space:]]*%s" f) nil t))
        )))
    ))
