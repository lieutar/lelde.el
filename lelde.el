;;; lelde.el --- Live Emacs Lisp Development Environment -*-lexical-binding: t -*-

;; Copyright (C) 2025 lieutar <lieutar@gmail.com>

;; Author: lieutar <lieutar@gmail.com>
;; Version: 0.1.0
;; Keywords: lisp, Programming, Develop
;; URL: https://github.com/lieutar/lelde.el
;; Package-Requires: ((f)(s)(dash)(ppp)(prinfo)(stmax)(elconc)(tinplate))

;;; License:

;; This project is licensed under the GNU General Public License v3.
;; see: https://www.gnu.org/licenses/gpl-3.0.html

;;; Commentary:
;;
;; `lelde' provides following solutions.
;; 
;;   - template feature ( by `tinplate' )
;;   - meta-programing using static macros ( by `stmax' )
;;   - module bundling ( by `elconc' )
;;   - test environment building
;;
;;  About deteils of this, see: README.md
;;

;;; Code:



;;;; lelde/META

(defconst lelde-VERSION "0.1.0")

(require 'f)
(require 's)
(require 'dash)
(require 'ppp)
(require 'package)
(require 'cl-lib)
(require 'prinfo)
(require 'stmax)
(require 'elconc)
(require 'tinplate)


;;; lelde/project/depends

(defun lelde/project/depends::is-core-module-p (package)
  (let ((path (locate-library (symbol-name package))))
    (and path
         (string-match (format "\\`%s" (regexp-quote lisp-directory)) path)
         t)))

(defsubst lelde/project/depends::detect-repository (package)
  ""
  (if (lelde/project/depends::is-core-module-p package) 'core 'elpa))


(defun lelde/project/depends::parse-version-string--local (location)
   (list :type 'local :location location))

(defun lelde/project/depends::parse-version-string (package verstr)
  ""
  (cond ;; local
        ((s-match "\\`\\(?:\\~\\|/\\)" verstr)
         (lelde/project/depends::parse-version-string--local
          (f-expand verstr)))

        ((s-match "\\`file://" verstr)
         (lelde/project/depends::parse-version-string--local
          (f-expand (s-replace-regexp "\\`file://" "" verstr))))

        ((s-match "\\`\\(?:https?\\|ftp\\)://" verstr)
         (list :type 'remote :location verstr))

        ((s-match "[^[:space:]]" verstr)
         (list :type (lelde/project/depends::detect-repository package)
               :version verstr))
        (t
         (list :type    (lelde/project/depends::detect-repository package)
               :version "0"))))

(defun lelde/project/depends::parse-depends-list (depends-list)
  "Completes lacked infoprmations from Lelde's :depends and
:dev-depends lists from user's emacs environment."
  (let ((r-result nil))
    (while depends-list
      (let ((package (car  depends-list))
            (version (cadr depends-list)))
        (push (cons package
                    (if (stringp version)
                        (progn
                          (setq depends-list (cddr depends-list))
                          (lelde/project/depends::parse-version-string
                           package version))
                      (progn
                        (setq depends-list (cdr depends-list))
                        (list :type
                              (lelde/project/depends::detect-repository package)
                              :version "0"))))
                    r-result)))
    (reverse r-result)))


;;; lelde/project

(defun lelde/project::find-project-root (&optional base)
  (prinfo/git::project-root (or base ".")))

(defsubst lelde/project::--CONFIG-FILE () "Lelde")

(defsubst lelde/project::--default-config (project-root)
  (let* ((name  (f-filename (f-expand project-root)))
         (index (s-replace-regexp "\\(?:\\.[^.]*\\)*\\'" "" name))
          ;; TODO: get information from git configuration
          ;; git config --global user.name
          ;; git config --global user.email
         (author    (format "%s <%s>" user-full-name user-mail-address))
         (copyright (format "%s %s"
                            (decoded-time-year (decode-time (current-time)))
                            author)))
    (list :name                    name
          :index                   index
          :libs                    nil
          :brief                   ""
          :commentary              ""
          :files                   nil
          :files-to-update        (list "Makefile"
                                        "Cask"
                                        (format "recipe/%s" index))
          :template-alist         (list (list (format "src/%s.el" index)
                                              (format "%s.el" index))
                                        (list "src/README.md" "README.md"))
          :copyright               copyright
          :author                  author
          :emacs                   emacs-version
          :project-path            project-root
          :sources                 '(gnu melpa)
          :dependency              nil
          :dev-dependency          nil
          :test-feature            ""
          :test-runner             ""
          :src-dir                 "src"
          :rsc-dir                 "rsc"
          :recipe-dir              "recipe"
          :scripts-dir             "scripts"
          :test-integration-suffix ".test.el"
          :test-unit-suffix        ".unit.el"
          :test-scripts-dir        "test/scripts"
          :test-lib-dir            "test/lib"
          :test-rsc-dir            "test/rsc")))

(defun lelde/project::get-project-config-file (project-root)
  (setq project-root (lelde/project::find-project-root project-root))
  (let ((cfg (f-expand (lelde/project::--CONFIG-FILE) project-root)))
    (when (file-exists-p cfg) cfg)))

(defun lelde/project::read-project-config--arrange-depends (result)
  (dolist (kwd '(:dependency :dev-dependency))
    (plist-put result kwd (lelde/project/depends::parse-depends-list
                           (plist-get result kwd))))
  )

(defun lelde/project::read-project-config (project-root)
  (setq project-root (lelde/project::find-project-root project-root))
  (let* ((result (lelde/project::--default-config project-root))
         (config-file (lelde/project::get-project-config-file project-root))
         (configured  (and config-file (cdr (read (f-read config-file))))))
    (dolist (key (-filter #'keywordp configured))
      (let ((value (plist-get configured key)))
        (plist-put result key value)))
    (lelde/project::read-project-config--arrange-depends result)
    result))

;;
;; memo
;;
;; about :keywords , eask's keywords list has 36 keywords.
;; but these keywords seem a bit old fashioned.
;; these keywords were defined on the variable `finder-known-keywords'
;; on finder.el
;; (require 'finder)
;;

(defun lelde/project::get-project-info (path)
  "Makes project information plist from given PATH.
The information has following properties.

  :name (string)
  :index (string)
  :libs (list<string>) - List of additional libraries. Default is nil.
  :project-path (string) - The root directory of the project (containing .git).
  :brief (string) - Short description of the module.
  :commentary (string) - Long description of the module.
  :files (list<string>) - List of file globs of resource files.
  :files-to-update (list<string>)
  :template-alist (list<list<string string>>)
  :copyright (string) -
  :author (string) -
  :emacs (string) - Emacs version to require .
  :sources (list<symbol|list<string>>) -
  :dependency (list<symbol|string>) -
  :dev-dependency (list<symbol|string>) -

  :src-dir (string) - The directory containing elisp files.
         Defaults to \"src\".
         If provided, it will be expanded relative to the project root.
  :src-path (string) - The realpath of the :src-dir.

  :recipe-dir (string)
  :recipe-path (string)

  :scripts-dir (string)
  :scripts-path (string)

  :test-feature (string) - emacs options to load additional test frameworks.
  :test-runner (string) - emacs options to execute additional test runners.
  :test-integration-suffix (string) - suffix of integration test scripts.
  :test-unit-suffix (string) - suffix of unit test scripts.

  :test-scripts-dir (string, optional) - The directory containing test scripts.
         Defaults to \"test/scripts\".
         If provided, it will be expanded relative to the project root.
  :test-scripts-path (string) - The realpath of the :test-scripts-dir.

  :test-lib-dir (string, optional) - The directory containing libraries
         for tests.
         Defaults to \"test/lib\"
         If provided, it will be expanded relative to the project root.
  :test-lib-path (string) - The realpath of the :test-lib-dir.

  :test-rsc-dir (string, optional) - The directory containing resources
         for tests.
         Defaults to \"test/rsc\".
         If provided, it will be expanded relative to the project root.
  :test-rsc-path (string) - The realpath of the :test-rsc-dir.
"
  (let ((project-root (lelde/project::find-project-root path)))
    (let ((cfg (lelde/project::read-project-config project-root)))
      (dolist (dir2path '(src
                          recipe
                          scripts
                          test-scripts
                          test-lib
                          test-rsc))
        (let ((dir  (intern (format ":%s-dir"  dir2path)))
              (path (intern (format ":%s-path" dir2path))))
          (plist-put cfg path (f-expand (plist-get cfg dir) project-root))))
      cfg)))


;;;; lelde/rsc

(defconst lelde/rsc::$resource-path
  (f-expand "rsc" (lelde/project::find-project-root
                   (or load-file-name buffer-file-name))))

(defun lelde/rsc::get-rsc-file-list (base)
  (let ((base-path (f-expand base lelde/rsc::$resource-path)))
    (--map
     (f-relative it base-path)
     (directory-files-recursively base-path ""))))
;; (lelde/rsc::get-rsc-file-list "template")

(defun lelde/rsc::get-rsc (name)
  (f-read (f-expand name lelde/rsc::$resource-path)))


;;!export
(defvar lelde/stmax/emit::$emit-for-index-functions '())

;;!export
(defun lelde/stmax/emit::emit-index-header ()
  ""
  (lelde/rsc::get-rsc "common/index-header.el"))

;;!export
(defmacro lelde/stmax/emit::emit-for-index ()
  `(let ((pspec
          (lelde/project::get-project-info stmax-current-processing-file)))
     (mapconcat (lambda (it) (funcall it pspec))
                lelde/stmax/emit::$emit-for-index-functions
                "\n")))


;;;; lelde/tinplate

(defsubst lelde/tinplate::--make-env (pinfo)
  (let* ((env-slot '("@ENV"))
         (env       (list env-slot)))
    (dolist (kwd (-filter #'keywordp pinfo))
      (let ((key   (s-replace-regexp "\\`:" "" (symbol-name kwd)))
            (value (plist-get pinfo kwd)))
        (push (cons key value) env)))
    (setcdr env-slot env)
    env))

(defun lelde/tinplate::fill (pinfo src-content dst)
  (let ((update? (f-exists-p dst)))
    (with-temp-file dst
      (insert (tinplate-fill src-content (lelde/tinplate::--make-env pinfo)))
      (message "%s: %s" (if update? "Update" "Create") dst))))

;;!export
(defun lelde/tinplate::tinplate-fill ()
  (lelde/cli::init)
  (let ((src (nth 0 command-line-args-left))
        (dst (nth 1 command-line-args-left))
        (pinfo (lelde/project::get-project-info ".")))
    (lelde/tinplate::fill pinfo (f-read src) dst)))


;;;; lelde/project/update

;;!export
(defun lelde/project/update::update-project-files ()
  (lelde/cli::init)
  (apply #'lelde/project/update::update-files command-line-args-left))

(defun lelde/project/update::update-files (&rest files)
  (dolist (file files)
    (lelde/project/update::update-file file)))

(defsubst lelde/project/update::update-file--make-template-alist (pinfo base)
  (let ((index (plist-get pinfo :index)))
    (let ((templates (lelde/rsc::get-rsc-file-list base)))
      (--map (cons (let ((src it)
                         (match (-map #'cadr (s-match-strings-all
                                              "@\\([^@]*\\)@" it))))
                     (dolist (key match)
                       (let* ((kwd (intern (format ":%s" key)))
                              (val (plist-get pinfo kwd)))
                         (when (stringp val)
                           (setq src (s-replace (format "@%s@" key)
                                                val src)))))
                     (s-replace "@@" "" src))
                   it)
             templates))))

(defun lelde/project/update::update-file (file &optional base)
  (setq base (or base "template"))
  (let* ((pinfo     (lelde/project::get-project-info "."))
         (pp        (plist-get pinfo :project-path))
         (index     (plist-get pinfo :index))
         (t-alist   (lelde/project/update::update-file--make-template-alist
                     pinfo base))
         (template  (let ((slot (assoc file t-alist)))
                      (unless slot
                        (error "Undefined way to update of \"%s\"." file))
                      (lelde/rsc::get-rsc (f-join base (cdr slot))))))
    (let ((dir (f-dirname (f-expand file pp))))
      (unless (f-dir-p dir) (apply #'f-mkdir (f-split dir))))
    (lelde/tinplate::fill pinfo template (f-expand file pp))))

;;(lelde/project/update::update-file "Cask")
;;(lelde/project/update::update-file "Makefile")
;;(lelde/project/update::update-file "init.sh" "bootstrap")

;;(insert "\n" (lelde/project/update::update-file "src/lelde.src.el"))
;;(insert "\n" (ppp-sexp-to-string (lelde/project/update::update-file "Cask")))


;;; lelde/project/init
;;!export
(defun lelde/project/init::init-project ()
  ""
  (unless (prinfo/git::is-valid-dot-git-p ".git")
    (unless (zerop (shell-command "git init" nil nil))
      (error "\"git init\" was failsed"))
    (unless (prinfo/git::is-valid-dot-git-p ".git")
      (error
       "WTH? \"git init\" was success. However, there isn't valid \".git\"")))
  (when (f-exists-p "Cask") (delete-file "Cask"))
  (when (f-exists-p "Lelde")
    (rename-file "Lelde" "Lelde.bak")
    (message "Lelde is already exists. it was renamed as Lelde.bak"))
  (lelde/cli::init)
  (let* ((pinfo (lelde/project::get-project-info "."))
         (index (plist-get pinfo :index))
         (skipped nil)
         (files
          (cons "Lelde"
                (->>
                 (lelde/project/update::update-file--make-template-alist
                  pinfo "template")
                 (-map #'car)
                 (--filter (not (string= "Lelde" it)))))))
    (dolist (file files)
      (when (f-exists-p file)
        (rename-file file (format "%s.bak" file))
        (message "%s is already exists. it was renamed as %s.bak" file file)))
    (apply #'lelde/project/update::update-files files)
    (let ((scripts-dir (plist-get pinfo :scripts-path)))
      (dolist (f (directory-files scripts-dir))
        (let ((file (f-expand f scripts-dir)))
          (when (f-file-p file) (chmod file #o755)))))
    ))


;;;; lelde/elconc

(defun lelde/elconc::bundle (src dst)
  (setq src (expand-file-name src))
  (let ((mods (lelde/project/modules::get-modules-alist ".")))
    (with-temp-file dst
      (insert (apply 'elconc-bundled-source
                     src
                     (->> mods
                          (--map (plist-get (cdr it) :el))
                          (--filter (not (equal it src)))
                          ))))))

;;!export
(defun lelde/elconc::elconc-bundle ()
  (lelde/cli::init)
  (let ((src (nth 0 command-line-args-left))
        (dst (nth 1 command-line-args-left)))
    (lelde/elconc::bundle src dst)))


;;!export
(defun lelde/stmax::stmax-file ()
  (lelde/cli::init)
  (let ((src (nth 0 command-line-args-left)))
    (stmax-file src)))


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
                                                        (symbol-name func))))
                                 (-find #'fboundp))
                            func)))
     (,func-to-call (lelde/test::test-spec) ,@args)))

;;!export
(defmacro lelde/test::test-setup (&rest additional-props)
  `(let* ((file (or load-file-name buffer-file-name))
          ((new-spec (append (lelde/test::make-test-spec file)
                             additional-props))))
     (lelde/test::--setup-test-environment new-spec)
     (push  (cons file new-spec) lelde/test::$test-enviroments-alist)))

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

;;;###autoload
(defvaralias 'lelde-emit-for-index-functions 'lelde/stmax/emit::$emit-for-index-functions)

;;;###autoload
(defalias 'lelde-emit-index-header 'lelde/stmax/emit::emit-index-header)

;;;###autoload
(defalias 'lelde-emit-for-index 'lelde/stmax/emit::emit-for-index)

;;;###autoload
(defalias 'lelde-tinplate-fill 'lelde/tinplate::tinplate-fill)

;;;###autoload
(defalias 'lelde-update-project-files 'lelde/project/update::update-project-files)

;;;###autoload
(defalias 'lelde-init-project 'lelde/project/init::init-project)

;;;###autoload
(defalias 'lelde-elconc-bundle 'lelde/elconc::elconc-bundle)

;;;###autoload
(defalias 'lelde-stmax-file 'lelde/stmax::stmax-file)

;;;###autoload
(defalias 'lelde-test-spec 'lelde/test::test-spec)

;;;###autoload
(defalias 'lelde-test-call 'lelde/test::test-call)

;;;###autoload
(defalias 'lelde-test-setup 'lelde/test::test-setup)

;;;###autoload
(defalias 'lelde-setup-test-environment 'lelde/test::setup-test-environment)


(provide 'lelde)
