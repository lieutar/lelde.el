;; -*- lexical-binding: t -*-
(require 'lelde)
(lelde-test-setup)
(describe "test on new project"
  (/tmp/with-temp-dir
    (/tmp/weird-magic-spell)
    (f-mkdir "hello")
    (let ((default-directory (f-expand "hello")))
      (lelde-init-project)
      ;;(should (zerop (call-process "make" nil nil nil "test"))
      ))))
