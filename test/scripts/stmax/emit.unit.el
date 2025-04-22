;; -*- lexical-binding: t -*-
(require 'buttercup)
(require 'lelde/stmax/emit)
(require 'lelde/test)
(require 'slash-tmp)
(require 'cl-lib)
(require 'f)
(lelde/test::setup-test-environment $T)

(describe "lelde/stmax/emit::emit-export"
  (let (project-dir-was-created
        git-was-initialized
        pinfo
        code
        )
  (/tmp/with-temp-dir
    (/tmp/weird-magic-spell)
    (lelde/test/util::test-rsc-copy $T "stmax--emit/c0000" "./c0000")
    (setq project-dir-was-created (f-dir-p "c0000"))
    (when project-dir-was-created
      (let ((default-directory (f-expand "c0000")))
        (call-process "git" nil nil nil "init")
        (setq git-was-initialized (f-dir-p ".git"))
        (when git-was-initialized
          (setq pinfo (lelde/project::get-project-info "."))
          (setq code (lelde/stmax/emit::emit-export pinfo))))))
  (it "basic assertions"
    (should project-dir-was-created)
    (should git-was-initialized))
  ;;(describe code)
  (describe "code content"
    (let ((code-alist
           (let (result)
             (with-temp-buffer
               (insert code)
               (goto-char (point-min))
               (while (re-search-forward "^;;;###autoload" nil t)
                 (let* ((from      (point))
                        (to        (progn (forward-sexp) (point)))
                        (src       (buffer-substring-no-properties from to))
                        (exp       (read src))
                        (first-arg (cadr exp))
                        (sym       (if (symbolp first-arg)
                                       first-arg (cadr first-arg))))
                   (push  (list sym :src src :exp exp)  result))))
             result)))
      ;;(describe (ppp-sexp-to-string code-alist))
      (it "assertions"
        (should-not (assq 'c0000-internal code-alist))
        (cl-macrolet
            ((assert-ex (sym &rest body)
               `(when (should (assq ,sym code-alist))
                  (let ((exp (plist-get (cdr (assq ,sym code-alist)) :exp)))
                    ,@body))))

          (assert-ex
           'c0000-var-a
           (should
            (equal exp
                   '(defvaralias 'c0000-var-a 'c0000/sub::$var-a))))

          (assert-ex
           'c0000-sub-with-optional
           (should
            (equal exp
                   '(defun c0000-sub-with-optional (a &optional b)
                      (interactive (list 1 2))
                      (c0000/sub::sub-with-optional a b)))))

          (assert-ex
           'c0000-sub-with-args
           (should
            (equal exp
                   '(defun c0000-sub-with-args (a b c)
                      (interactive (list 1 2 3))
                      (c0000/sub::sub-with-args a b c)))))

          (assert-ex
           'c0000-sub-no-args
           (should
            (equal exp
                   '(defun c0000-sub-no-args ()
                      (interactive)
                      (c0000/sub::sub-no-args)))))

          (assert-ex
           'c0000-sub-ex-cmd
           (should
            (equal exp
                   '(defun c0000-sub-ex-cmd (a &optional b &rest c)
                       "document of sub-ex-cmd"
                       (interactive (list 1 2 3 4))
                       (apply #'c0000/sub::sub-ex-cmd a b c)))))

          (assert-ex
           'c0000-sub-ex-a
           (should (equal exp
                          '(defalias 'c0000-sub-ex-a 'c0000/sub::sub-ex-a))))
          ))))
  ))
