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
        modules-alist
        )
  (/tmp/with-temp-dir
    (/tmp/weird-magic-spell)
    (lelde/test/util::test-rsc-copy $T "project--modules/p0000")
    (setq dir-was-generated (f-dir-p "p0000"))
    (when dir-was-generated
      (let ((default-directory (f-expand "p0000")))
        (call-process-shell-command "git init" nil nil nil)
        (setq git-initialized (f-dir-p ".git"))
        (when git-initialized
          (setq modules-alist
                (lelde/project/modules::get-modules-alist "."))))))

  (it "basic assertions"
    (should dir-was-generated)
    (should git-initialized)
    (should modules-alist))

  (let ((mod-order (-map #'car modules-alist))
        (external-dependency (apply #'append
                                    (--map (plist-get (cdr it) :depends-external)
                                           modules-alist)))
        (ss (lambda (it) (format "%S" (sort it (lambda (a b)
                                                 (string< (symbol-name a)
                                                          (symbol-name b))))))))
    (describe (format "%S" mod-order))
    (it "content of the alist"
      (should (string= (format "%S" mod-order)
                       (format "%S" '(p0000/META
                                      p0000/core/sub-b
                                      p0000/core/sub
                                      p0000/core
                                      p0000))))
      (should (string= (funcall ss external-dependency)
                       (funcall ss '(dash s f))))
      ))
  ))
