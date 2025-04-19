;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'f)
(require 'lelde/project/modules)
(require 'lelde/test)
(require 'slash-tmp)

(lelde/test::setup-test-environment $T)

(describe "lelde/project/modules"
  (let (
        dir-was-generated
        git-initialized
        )
  (/tmp/with-temp-dir
    (/tmp/weird-magic-spell)
    (lelde/test/util::test-rsc-copy $T "project--modules/p0000")
    (setq dir-was-generated (f-dir-p "p0000"))
    (when dir-was-generated
      (let ((default-directory (f-expand "p0000")))
        (call-process-shell-command "git init" nil nil nil)
        (setq git-initialized (f-dir-p ".git")))))
  (it "assertions"
    (should dir-was-generated)
    (should git-initialized))))
