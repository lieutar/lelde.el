;; -*- lexical-binding: t -*-
(require 'lelde/test)
(require 'buttercup)
(require 'dash)
(require 'f)

(setq $THIS-FILE (or load-file-name buffer-file-name))
(lelde/test::setup-test-environment $T)
(defvar $TEST-IS-READY nil)
(describe "This test is ready"
  (let ((name      (plist-get $T :name))
        (index     (plist-get $T :index))
        (this-file (plist-get $T :this-file))
        (this-dir  (plist-get $T :this-dir)))
    (it "assertions"
      (setq $TEST-IS-READY
            (and
             (should (string= name  "lelde.el"))
             (should (string= index "lelde"))
             (should (string= this-file $THIS-FILE))
             (should (string= this-dir (file-name-directory $THIS-FILE)))
             )))))
