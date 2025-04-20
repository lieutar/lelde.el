;; -*- lexical-binding: t -*-
(require 'buttercup)
(require 'lelde/project/update)
(require 'lelde/project/init)
(require 'slash-tmp)
(require 'f)
(require 's)
(require 'dash)
(require 'ppp)

(defconst lelde-test/project/init::$num-files 17)

(describe "lelde/project/init"
  (let ((name-to-index
         (lambda (name) (s-replace-regexp "\\(?:\\.[^\\.]*\\)*\\'" "" name)))
        (get-content
         (lambda ()
           (->>
            (directory-files-recursively "." ".*")
            (-filter #'f-file-p)
            (--filter (not (string-match "\\.git[/\\\\]" it)))
            (--map (cons it (f-read it)))))))

    (describe "init on empty directory"
      (let (name
            index
            content)
        (/tmp/with-temp-dir
          (/tmp/weird-magic-spell)
          (setq name  (f-filename default-directory))
          (setq index (funcall name-to-index name))
          (lelde/project/init::init-project)
          (setq content (funcall get-content)))
      ;;(describe (ppp-sexp-to-string content))

        (it "assertions"
          (should (= (length content) lelde-test/project/init::$num-files))
          (should (assoc "./.gitignore" content))
          (should (assoc "./Cask" content))
          (should (assoc "./Cask.lodal" content))
          (should (assoc "./Lelde" content))
          (should (assoc "./Makefile" content))
          (should (assoc "./custom.mk" content))
          (should (assoc (format "./recipe/local/%s" index) content))
          (should (assoc (format "./recipe/public/%s" index) content))
          (should (assoc "./scripts/lcask" content))
          (should (assoc "./scripts/test" content))
          (should (assoc "./scripts/truncate" content))
          (should (assoc "./src/README.md" content))
          (should (assoc (format "./src/%s.src.el" index) content))
          (should (assoc (format "./src/%s/META.el" index) content))
          (should (assoc (format "./src/%s/core.el" index) content))
          (should (assoc "./test/scripts/core.unit.el" content))
          (should (assoc "./test/scripts/integration-lint.test.el" content))
          )))

    (describe "init with some files"
      (let (name
            index
            content)
        (/tmp/with-temp-dir
          (/tmp/weird-magic-spell)
          (setq name  (f-filename default-directory))
          (setq index (funcall name-to-index name))
          (with-temp-file "Lelde" (insert "princess"))
          (with-temp-file "Makefile" (insert "hoge"))
          (lelde/project/init::init-project)
          (setq content (funcall get-content)))
        (it "assertions"
          (should (= (length content)(+ 2 lelde-test/project/init::$num-files)))
          (should (assoc "./Makefile.bak" content))
          (should (assoc "./Makefile" content))
          (should (string= (cdr (assoc "./Lelde.bak" content)) "princess"))
          (should (string= (cdr (assoc "./Makefile.bak" content)) "hoge")))
        ))
    ))
