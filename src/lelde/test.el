;; -*- lexical-binding: t -*-

;;!drop-when-bundled
(provide 'lelde/test)
(require 'lelde/META)
(require 'lelde/project)
(require 'lelde/test/util)
;;!end

;;;; lelde/test

(defun lelde/test::find-project-root     (this-test)
  (lelde/project::find-project-root (directory-file-name this-test)))

(defun lelde/test::make-test-spec (test-script)
  "Generate a plist containing contextual information for a test script.
The plist contains the following information:

  :this-file (string) - The TEST-SCRIPT itself.
  :this-dir  (string)  - The directory containing the test script.

"
  (let* ((this-dir     (file-name-directory  test-script))
         (project-root  (lelde/test::find-project-root this-dir)))
    (append (lelde/project::get-project-info project-root)
            (list :this-file     test-script
                  :this-dir      this-dir))))

(defun lelde/test::--setup-test-environment (test-spec)
  (dolist (path (list (plist-get test-spec :test-lib-path)
                      (plist-get test-spec :src-path)))
    (when (and (not (member path load-path))
               (f-dir-p path))
      (setq load-path (cons path load-path)))))

;;!export
(defmacro lelde/test::setup-test-environment (&optional var)
  "Sets up your test environment and returns informations of your tests.
If you want to set up `load-path' for your test, you just call this macro,
and if you want to use the information of your test script and your project
you store return value into some variable.
for example:

;; -*- lexical-binding: t -*-
(require 'lelde)
(lelde-setup-test-environment $T) ;; set to $T test spec

"
  `(let ((test-spec (lelde/test::make-test-spec
                       (or load-file-name buffer-file-name))))
     (lelde/test::--setup-test-environment test-spec)
     ,(if var `(setq ,var test-spec) 'test-spec)))
