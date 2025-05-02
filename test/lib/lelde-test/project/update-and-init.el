;; -*- lexical-binding: t -*-
(require 'buttercup)
(require 'f)
(require 'dash)
(require 's)
(require 'slash-tmp)
(require 'with-advice)

(defconst lelde-test/project/init::$num-files 17)

(defun lelde-test/project/update-and-init::name-to-index (name)
  (s-replace-regexp "\\(?:\\.[^\\.]*\\)*\\'" "" name))


(defun lelde-test/project/update-and-init::get-content ()
  (->>
   (directory-files-recursively "." ".*")
   (-filter #'f-file-p)
   (--filter (not (string-match "\\.git[/\\\\]" it)))
   (--map (cons it (f-read it)))))


(defmacro lelde-test/project/update-and-init::test-Lelde (content-of-repo)
  `(let ((pinfo (cdr (read (cdr (assoc "./Lelde" ,content-of-repo))))))
     ;;(describe (cdr (assoc "./Lelde" ,content-of-repo)))
     ;;(describe (ppp-sexp-to-string pinfo))
     (it "Lelde content"
       (let ((sources (plist-get pinfo :sources)))
         (should sources)
         (should (--filter (and (listp it)
                                (equal "looper" (car it)))
                           sources))
         )
       (let ((dev-dependency (plist-get pinfo :dev-dependency)))
         (should dev-dependency))
       )
     ))

(defmacro lelde-test/project/update-and-init::test-Cask (content-of-repo)
  `(let ((content
          (read (format "(%s\n)" (cdr (assoc "./Cask" ,content-of-repo))))))
     ;;(describe (ppp-sexp-to-string content))
     (describe "Cask content"
       (let ((development (assq 'development content)))
         (it "content isn't nil"
           (should development))
         (let ((depends-on (->> (cdr development)
                                (--filter (eq 'depends-on (car it)))
                                (-map #'cdr))))
           (it "dev-dependency has lelde"
             (should (assoc "lelde" depends-on))))))
     ))

(defmacro lelde-test/project/update-and-init::init-on-empty-directory (init-func)
  `(describe "init on empty directory"
     (let (name
           index
           content)
       (with-advice (message)
         (/tmp/with-temp-dir
           (/tmp/weird-magic-spell)
           (setq name  (f-filename default-directory))
           (setq index (lelde-test/project/update-and-init::name-to-index name))
           (,init-func)
           (setq content (lelde-test/project/update-and-init::get-content))))

       (it "assertions"
         (should (= (length content) lelde-test/project/init::$num-files))
         (should (assoc "./.gitignore" content))
         (should (assoc "./Cask" content))
         (should (assoc "./Cask.local" content))
         (should (assoc "./Lelde" content))
         (should (assoc "./Makefile" content))
         (should (assoc "./custom.mk" content))
         (should (assoc (format "./recipe/%s" index) content))
         (should (assoc "./scripts/lcask" content))
         (should (assoc "./scripts/test" content))
         (should (assoc "./scripts/truncate" content))
         (should (assoc "./src/README.md" content))
         (should (assoc (format "./src/%s.src.el" index) content))
         (should (assoc (format "./src/%s/META.el" index) content))
         (should (assoc (format "./src/%s/core.el" index) content))
         (should (assoc "./test/scripts/core.unit.el" content))
         (should (assoc "./test/scripts/integration-lint.test.el" content))
         )
       (lelde-test/project/update-and-init::test-Lelde content)
       (lelde-test/project/update-and-init::test-Cask content)
       )))

(defmacro lelde-test/project/update-and-init::init-with-some-files (init-func)
  `(describe "init with some files"
     (let (name
           index
           content)
       (/tmp/with-temp-dir
         (/tmp/weird-magic-spell)
         (setq name  (f-filename default-directory))
         (setq index (lelde-test/project/update-and-init::name-to-index name))
         (with-temp-file "Lelde" (insert "princess"))
         (with-temp-file "Makefile" (insert "hoge"))
         (,init-func)
         (setq content (lelde-test/project/update-and-init::get-content)))
       (it "assertions"
         (should (= (length content)(+ 2 lelde-test/project/init::$num-files)))
         (should (assoc "./Makefile.bak" content))
         (should (assoc "./Makefile" content))
         (should (string= (cdr (assoc "./Lelde.bak" content)) "princess"))
         (should (string= (cdr (assoc "./Makefile.bak" content)) "hoge"))
         )
       (lelde-test/project/update-and-init::test-Lelde content)
       (lelde-test/project/update-and-init::test-Cask content)
       )))

(defmacro lelde-test/project/update-and-init (init-func)
  `(progn
     (lelde-test/project/update-and-init::init-on-empty-directory ,init-func)
     (lelde-test/project/update-and-init::init-with-some-files ,init-func)))


(provide 'lelde-test/project/update-and-init)
