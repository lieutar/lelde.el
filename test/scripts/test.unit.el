;; -*- lexical-binding: t -*-
(require 'lelde/test)
(require 'buttercup)
(require 'dash)
(require 'f)
(require 'slash-tmp)

(setq $THIS-FILE (or load-file-name buffer-file-name))
(lelde/test::test-setup)
(describe "This test is ready"
  (let (($T (lelde/test::test-spec)))
    (let ((name      (plist-get $T :name))
          (index     (plist-get $T :index))
          (this-file (plist-get $T :this-file))
          (this-dir  (plist-get $T :this-dir)))
      (it "assertions"
        (and
         (should (string= name  "lelde.el"))
         (should (string= index "lelde"))
         (should (string= this-file $THIS-FILE))
         (should (string= this-dir (file-name-directory $THIS-FILE)))
         )))))

(describe "lelde/test::ert"
  (let (result)
    (/tmp/with-temp-dir
      (/tmp/weird-magic-spell)
      (f-mkdir "sample")
      (let ((default-directory (expand-file-name "sample")))
        (call-process "git" nil nil nil "init")
        (with-temp-buffer
          (let* ((load-file-name (expand-file-name "foo.test.el"))
                 (buffer-file-name load-file-name))
            (let (begin)
              (insert (format "%S\n"
                              '(lelde/test::test-setup)))
              (call-interactively #'eval-last-sexp)
              (insert (format
                       "%S"
                       '(insert (format
                                 "%S"
                                 (macroexpand
                                  '(lelde/test::ert case-0 :args (a) "doc"
                                                    (should t)))))))
              (setq begin (point))
              (call-interactively #'eval-last-sexp)
              (setq result (read (buffer-substring-no-properties begin (point))))
              (setcar (assq 'eval (cdr result)) 'progn)
              (setq begin (point))
              (insert (format "%S" (eval result)))
              (setq result (read (buffer-substring-no-properties begin (point))))
              )))))
    ;;(describe (ppp-sexp-to-string result))
    (it "test the macro-expanded structure"
      (should (eq (nth 0 result) 'ert-deftest))
      (should (eq (nth 1 result) 'sample-test::case-0))
      (should (equal (nth 2 result) '(a)))
      (should (equal (nth 3 result) "doc"))
      (should (equal (nth 4 result) '(should t))))
    ))
