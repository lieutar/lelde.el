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

(defun lelde/test::--setup-test-environment (test-spec &rest props)
  (dolist (path (list (plist-get test-spec :test-lib-path)
                      (plist-get test-spec :src-path)))
    (when (and (not (member path load-path))
               (f-dir-p path))
      (push path load-path)))
  (when (if (memq :load-deps props) (plist-get props :load-deps) t)
    (dolist (feature (-map #'car (append (plist-get test-spec :dependency)
                                         (plist-get test-spec :dev-dependency))))
      (require feature))))

(defconst lelde/test::$test-enviroments-alist ())
(defun lelde/test::--registered-test-spec (file)
  (let ((spec (or (cdr (assoc file lelde/test::$test-enviroments-alist)))))
    (unless spec (error "`lelde/test-setup' wasn't called"))
    spec))

;;!export
(defmacro lelde/test::test-spec ()
  `(lelde/test::--registered-test-spec (or load-file-name buffer-file-name)))

;;!export
(defmacro lelde/test::test-call (func &rest args)
  `(let* ((file     (or load-file-name buffer-file-name))
          (spec     (lelde/test::--registered-test-spec file))
          (func-to-call (or (->> (or (plist-get spec :prefix)
                                     ;; default prefix list
                                     '(lelde/test/util::test-))
                                 (--map (intern (concat (symbol-name it)
                                                        (symbol-name ',func))))
                                 (-find #'fboundp))
                            ',func)))
     (funcall func-to-call (lelde/test::test-spec) ,@args)))

;;!export
(defmacro lelde/test::test-setup (&rest additional-props)
  `(let* ((file     (or load-file-name buffer-file-name))
          (new-spec (append (lelde/test::make-test-spec file)
                            ,@additional-props)))
     (lelde/test::--setup-test-environment new-spec ,@additional-props)
     (push  (cons file new-spec) lelde/test::$test-enviroments-alist)))
