;; -*- lexical-binding: t -*-
(require 'slash-tmp)

(defmacro lelde-test/with-repos (rsc-path &rest body)
  (declare (indent defun))
  `(progn
     (/tmp/with-temp-dir
       (/tmp/weird-magic-spell)
       (lelde/test::test-call rsc-copy ,rsc-path)
       (let ((default-directory (f-expand (f-filename ,rsc-path))))
         (call-process-shell-command "git init" nil nil nil)
         ,@body))))

(provide 'lelde-test)
