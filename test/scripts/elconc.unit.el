;; -*- lexical-binding: t -*-
(require 'buttercup)
(require 'lelde/elconc)
(require 'lelde/test)
(require 'slash-tmp)
(require 'f)

(lelde/test::test-setup)

(describe "lelde/elconc"
  (let (resource-was-copied
        git-initialized
        all
        mods
        mods-filtered
        )
    (/tmp/with-temp-dir
      (/tmp/weird-magic-spell)
      (lelde/test::test-call rsc-copy "elconc/lelde.el.pss")
      (setq resource-was-copied (f-dir-p "lelde.el.pss"))
      (when resource-was-copied
        (let ((default-directory (f-expand "lelde.el.pss")))
          (call-process "git" nil nil nil "init")
          (setq git-initialized (f-dir-p ".git"))
          (when git-initialized
            (setq all (lelde/project/modules::modules-alist--collect-from-fs
                       'lelde "src"))
            (setq mods (lelde/project/modules::modules-alist--mearge-duplicated all))
            (lelde/project/modules::modules-alist--collect-dependencies 'lelde mods)
            (lelde/project/modules::modules-alist--add-depended-by      mods)
            (lelde/project/modules::modules-alist--add-all-depended-by  mods)
            (setq mods-filtered (lelde/project/modules::get-modules-alist "."))
            ))))
    (it "basic assertions"
      (should resource-was-copied)
      (should git-initialized))
    (describe (ppp-sexp-to-string (assq 'lelde/project/update mods)))
    (describe (ppp-sexp-to-string (assq 'lelde/project/init mods)))
    (it "mod-alist"
      (should (assq 'lelde/cli mods-filtered))
      (should (assq 'lelde/project/update mods-filtered))
      )
    ))
