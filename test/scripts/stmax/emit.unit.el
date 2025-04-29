;; -*- lexical-binding: t -*-
(require 'lelde/stmax/emit)
(require 'lelde/test)
(lelde/test::test-setup)
(require 'lelde-test)

(describe "lelde/stmax/emit/export::emit-export"
  (let (pinfo
        code)
    (lelde-test/with-repos "stmax--emit/c0000" "./c0000"
      (setq pinfo (lelde/project::get-project-info "."))
      (setq code (lelde/stmax/emit/export::emit-export pinfo)))
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
