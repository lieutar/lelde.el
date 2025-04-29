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
          :license                 "This project is licensed under the GNU General Public License v3.
see: https://www.gnu.org/licenses/gpl-3.0.html"
          :emacs                   emacs-version
          :project-path            project-root
          :sources                 '(gnu
                                     melpa
                                     ("looper"  "https://raw.githubusercontent.com/lieutar/looper-elpa/refs/heads/looper/packages/archive-contents"))
          :dependency              nil
          :dev-dependency          '(lelde)
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
  (f-expand "rsc" (f-dirname (or load-file-name buffer-file-name))))
(defun lelde/rsc::get-rsc-file-list (base)
  (let ((base-path (f-expand base lelde/rsc::$resource-path)))
    (--map
     (f-relative it base-path)
     (directory-files-recursively base-path ""))))

(defun lelde/rsc::get-rsc (name)
  (f-read (f-expand name lelde/rsc::$resource-path)))


;;;; lelde/cli
(defconst lelde/cli::$project-info-cache nil)

(defun lelde/cli::get-project-info (orig-fun project-root)
  (setq project-root (f-expand project-root))
  (let* ((cache  lelde/cli::$project-info-cache)
         (result (cdr (assoc project-root (cdr cache)))))
    (unless result
      (setq result (funcall orig-fun project-root))
      (nconc cache (list (cons project-root result))))
    result))

(defun lelde/cli::init ()
  (unless lelde/cli::$project-info-cache

    (setq lelde/cli::$project-info-cache '(cached))
    (let* ((proot (f-expand (prinfo/git::project-root ".")))
           (local-lelde-config.el (f-expand "local-lelde-config.el" proot)))
      (when (f-exists-p local-lelde-config.el)
        (load local-lelde-config.el)))

    (advice-add #'lelde/project::get-project-info :around
                #'lelde/cli::get-project-info)))


;;;; lelde/tinplate/util

(defun lelde/tinplate/util::Cask-depends-on (dependency indent)
  (s-replace-regexp
   "^" indent
   (mapconcat
    (lambda (it)
      (let* ((pkg   (car it))
             (plist (cdr it))
             (type  (plist-get plist :type)))
        (cond ((eq type 'core)
               (format ";; %s (core)\n" pkg))
              ((eq type 'elpa)
               (ppp-sexp-to-string
                `(depends-on ,(symbol-name pkg) ,(plist-get plist :version))))
              ((eq type 'local)
               (format ";; %s file\n" pkg))
              ((eq type 'lelde)
               (ppp-sexp-to-string
                `(depends-on ,(symbol-name pkg) :git ,(plist-get plist :git))))
              (t (format ";; %s UNKNOWN (%s)\n" pkg type)))
        ))
    dependency)))

(defun lelde/tinplate/util::index-pr (depends)
  (let* ((warnings nil)
         (pr-list (mapconcat
                   (lambda (it)
                     (let* ((pkg     (car it))
                            (plist   (cdr it))
                            (type    (plist-get plist :type))
                            (version (plist-get plist :version)))
                       (cond ((eq type 'core) "")
                             ((eq type 'elpa)
                              (format "%S"
                                      (cons pkg
                                            (when (and (stringp version)
                                                       (not (string= "0"
                                                                      version)))
                                              (list version)))))
                             (t (push pkg warnings)))))
                   depends)))
    (format "(%s)%s" pr-list
            (if warnings (format "
;; WARNING
;; This module depends following packages where aren't on any package archives:
;; %s
" (s-join ", " warnings)) ""))))


(defconst lelde/tinplate/util::$Cask-additional-source
  `())

(defun lelde/tinplate/util::Cask-sources (sources)
  (mapconcat
   (lambda (it)
     (ppp-sexp-to-string
      (if (listp it) (cons 'source it)
        (list 'source it))))
   sources))

;; (lelde/tinplate/util::Cask-sources '(gnu melpa))

(defun lelde/tinplate/util::recipe (@ENV)
  (let ((index   (cdr (assoc "index"   @ENV)))
        (url     (cdr (assoc "url"     @ENV)))
        (repo    (cdr (assoc "repo"    @ENV)))
        (files   (cdr (assoc "files"   @ENV)))
        (errors  nil))
    (let ((recipe (list (intern index))))
      (when (and (stringp url) (null repo)) (setq repo url))
      (if repo
        (cond
         ((string-match "\\`https://\\(github\\|gitlab\\)\\.com/\\(.+\\)" repo)
          (nconc recipe (list :fetcher (intern (match-string 1 repo))
                                      :repo    (match-string 2 repo)))))
        (push (format "
;; WARNING THIS PROJECT DOESN'T HAVE THE DEFINITION OF THE REPOSITORY.
;;
;; At first, add :url or :repo on the `Lelde' file, and run
;;
;;     make %s
;;
;; (:repo is required, if the url isn't github / gitlab repository's url. )
"
                      (f-join (cdr (assoc "recipe-dir" @ENV)) index))
              errors))
      (when files
        (nconc recipe (list :files (cons (format "%s.el" index) files))))
      (if errors (s-join "\n" errors)
        (ppp-sexp-to-string recipe))))
  )

(defun lelde/tinplate/util::make-phoeny-macro (@ENV)
  (let ((files-to-update (cdr (assoc "files-to-update" @ENV))))
    (format
     "
PHONY := help all build package clean clean-all clean-cask update%s\\
	test test-unit test-integration"
     (if (member "Makefile" files-to-update)
         " Makefile-itself" ""))))

(defun lelde/tinplate/util::update-tasks (@ENV)
  (let ((files-to-update (cdr (assoc "files-to-update" @ENV)))
        (template-alist  (cdr (assoc "template-alist" @ENV))))
    (concat
     "
update :=
"
     (mapconcat (lambda (file) (format "
update := $(update) %s
%s: Lelde
\t$(lelde_update) $@
"
                                       file file file))
                (--filter (not (string= "Makefile" it)) files-to-update))
     (mapconcat (lambda (pair)
                  (let ((src (car pair))
                        (dst (cadr pair)))
                    (format "
update := $(update) %s
%s: %s Lelde
\t$(lelde_fill) $< $@
"
                            dst dst src)))
                template-alist)
     (if (member "Makefile" files-to-update)
         "
#>Makefile-itself
#>    Update Makefile itself.
#>
Makefile-itself:
\t$(lelde_update) Makefile

update: $(update) Makefile-itself
"
     "
update: $(update)
"))))


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


;;;; lelde/project/modules

(defsubst lelde/project/modules::modules-alist--collect-from-fs (project base)
  (let ((files nil))
    (dolist (fmt '("%s.el" "%s.src.el"))
      (let ((file (f-expand (format fmt project) base)))
        (when (f-exists-p file)(setq files (cons file files)))))
    (dolist (file (directory-files-recursively
                     (f-expand (symbol-name project) base)
                     "\\.el\\'"))
      (when (not (s-match "\\`\\.?#" (f-base file)))
        (setq files (cons file files))))
    (--map
     (list
      (intern (s-replace-regexp "\\(?:\\.src\\)?\\.el\\'" ""
                                (f-relative it base)))
      (if (s-match "\\.src\\.el\\'" it) :src :el) it)
     files)))

(defsubst lelde/project/modules::modules-alist--mearge-duplicated (all)
  (let ((result nil))
    (dolist (slot all)
      (let* ((feature (car slot))
             (previous-slot (assq feature result)))
        (if previous-slot
            (nconc previous-slot (cdr slot))
          (setq result (cons slot result)))))
    result))

(defsubst lelde/project/modules::modules-alist--collect-dependencies (project
                                                                      result)
  (dolist (slot result)
    (let* ((plist   (cdr slot))
           (depends (lelde/project/modules::get-dependencies-from-file
                     (or (plist-get (cdr slot) :src)
                         (plist-get (cdr slot) :el))))
           (internalp (lambda (sym)
                        (s-match (format "\\`%s\\(?:/.*\\)?\\'"
                                         project)
                                 (symbol-name sym))))
           internal external)
      (dolist (mod depends)
        (if (funcall internalp mod)
            (setq internal (cons mod internal))
          (setq external (cons mod external))))
      (plist-put plist :depends-internal internal)
      (plist-put plist :depends-external external))))

(defun lelde/project/modules::get-dependencies-from-file (file)
  "Get a list of dependencies from FILE by searching for `require` forms.

This function has several limitations:
1. It does not handle macros that may expand to `require` forms.
2. It may incorrectly identify `require` forms in argument lists.
3. It does not consider conditional `require` forms
   (e.g., inside `when` or `if`).
4. It may not handle all possible syntaxes of `require`.
5. The argument of require have to be a quoted symbol.

For accurate dependency analysis, manual inspection or more sophisticated
parsing techniques may be necessary."
  (let* ((src (with-temp-buffer
                (insert-file-contents file)
                (buffer-substring-no-properties (point-min) (point-max))))
         (read-src (ignore-errors (read (format "(\n%s\n)" src))))
         (result nil))
    (letrec ((recurse (lambda (tree)
                        (when (and tree (listp tree))
                          (if (eq 'require (car tree))
                              (push (cadr tree) result)
                            (dolist (node tree)
                              (funcall recurse node)))))))
      (funcall recurse read-src))
    (-map 'cadr (--filter (and (listp it) (eq 'quote (car it))) result))))

(defun lelde/project/modules::get-modules-alist (project-root)
  ""
  (let* ((pinfo  (lelde/project::get-project-info project-root))
         (index  (plist-get pinfo :index)))
    (when (stringp index) (setq index (intern index)))
    (let* ((base   (plist-get pinfo :src-path))
           (all    (lelde/project/modules::modules-alist--collect-from-fs index
                                                                          base))
          (result (lelde/project/modules::modules-alist--mearge-duplicated all)))
      (lelde/project/modules::modules-alist--collect-dependencies index result)
      result)))

(defun lelde/project/modules::query-internal-dependencies (mods feature)
  (letrec ((recurse
            (lambda (feature current-path)
              (when (memq feature current-path)
                (error "Cycled dependencies: %s > %s"
                       (s-join " > " (-map 'symbol-name (reverse current-path)))
                       feature))
              (push feature current-path)
              (let* ((slot  (assq feature mods))
                     (plist (cdr slot))
                     (deps  (plist-get plist :depends-internal)))
                (let ((results
                       (->> deps
                            (-map (lambda (dep)
                                    (append (funcall recurse dep current-path)
                                            (list dep))))
                            -flatten)))
                  (pop current-path)
                  results)))))
    (--map (assq it mods) (delete-dups (funcall recurse feature nil)))))


;;;; lelde/stmax/emit/export

;;(s-match "[^[:space:]\n\r]" "\n")
(defun lelde/stmax/emit/export::parse-arg (src)
  (let ((arg-sexp (and (s-match "[^[:space:]\n\r]" src)
                       (read src))))
    (when (and arg-sexp
               (not (and(listp arg-sexp)
                        (eq (car arg-sexp) 'interactive))))
      (error ";;!export macro accepts only (interactive) form."))
    arg-sexp))

(defsubst lelde/stmax/emit/export::--export-as (symbol)
  (intern (s-replace-regexp "\\`\\([^/]+\\).*?::\\$?" "\\1-"
                            (symbol-name symbol))))

(defun lelde/stmax/emit/export::parse-sexp--defvar   (sexp arg)
  (when arg (error "exporting variable can't accept any args"))
  (let* ((name      (cadr sexp))
         (export-as (lelde/stmax/emit/export::--export-as name))
         (export-form `(defvaralias ',export-as ',name)))
    (list :type 'defvar
          :symbol name :export-as export-as :export-form export-form)))

(defun lelde/stmax/emit/export::parse-sexp--defconst (sexp arg)
  (when arg (error "exporting constant can't accept any args"))
  (let ((result (lelde/stmax/emit/export::parse-sexp--defvar sexp arg)))
    (plist-put :type 'defconst)
    result))

(defun lelde/stmax/emit/export::parse-sexp--defmacro (sexp arg)
  (when arg (error "exporting macro can't accept any args"))
  (let ((result (lelde/stmax/emit/export::parse-sexp--defun sexp arg)))
    (plist-put result :type 'defmacro)
    result))

(defsubst lelde/stmax/emit/export::delegating-form (func arg-list)
  (let ((stripped-arg-list (--filter (not (s-match "\\`&" (symbol-name it)))
                                     arg-list)))
    (if (member '&rest arg-list)
        `(apply #',func ,@stripped-arg-list)
      `(,func ,@stripped-arg-list))))

(defun lelde/stmax/emit/export::parse-sexp--defun (sexp arg)
  (let ((name              (cadr sexp))
        (arg-list          (caddr sexp))
        (doc-or-first-sexp (cadddr sexp)))
    (let* ((export-as (lelde/stmax/emit/export::--export-as name)))
      (list
       :type 'defun :symbol name :export-as export-as
       :export-form
       (if arg
           `(defun ,export-as ,arg-list
              ,@(and (stringp doc-or-first-sexp) (list doc-or-first-sexp))
              ,arg
              ,(lelde/stmax/emit/export::delegating-form name arg-list))
       `(defalias ',export-as ',name))))))

(defun lelde/stmax/emit/export::parse-sexp (src arg)
  (unless (s-match "\\`[[:space:]\n\r]*(" "\n  \n(")
    (error "sexp after ;;!export have to be a list without any comments"))
  (let* ((sexp   (read src))
         (type   (car sexp))
         (parser (intern (format "lelde/stmax/emit/export::parse-sexp--%s"
                                 type))))
    (if parser (funcall parser sexp arg)
      (error "Undefined exporting method for: %s" type))))

(defsubst lelde/stmax/emit/export::--parse-file (file)
  (let ((r-result nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "^;;!export\\b" nil t)
        (let ((arg-start (point))
              (arg nil)
              (sexp nil))
          (move-beginning-of-line 2)
          (let* ((start (point))
                 (end (progn (condition-case nil (forward-sexp)(error))
                             (point))))
            (setq arg  (lelde/stmax/emit/export::parse-arg
                        (buffer-substring-no-properties arg-start start)))
            (setq sexp (buffer-substring-no-properties start end)))
          (push  (append (list :src file
                               :arg  arg)
                         (lelde/stmax/emit/export::parse-sexp sexp arg))
                 r-result))))
    (reverse r-result)))

(defun lelde/stmax/emit/export::aggregate-exporting-information (project-spec)
  (let* ((project-root (plist-get project-spec :project-path))
         (src-path     (plist-get project-spec :src-path))
         (files (--map
                 (let ((plist (cdr it)))
                   (or (plist-get plist :el)
                       (plist-get plist :src)))
                 (lelde/project/modules::get-modules-alist project-root)))
         (result (list :result)))
    (dolist (file files)
      (nconc result (lelde/stmax/emit/export::--parse-file file)))
    (cdr result)))

;;!export
(defun lelde/stmax/emit/export::emit-export (project-spec)
  (s-join
   "\n"
   (->> (lelde/stmax/emit/export::aggregate-exporting-information project-spec)
        (--map (list ";;;###autoload"
                     (ppp-sexp-to-string (plist-get it :export-form))))
        -flatten)))

(add-hook 'lelde/stmax/emit::$emit-for-index-functions
          #'lelde/stmax/emit/export::emit-export)


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


;;; lelde/project/init

(defun lelde/project/init::--move-to-backup-if-exists (file)
  (let ((new-name file)
        (counter  0))
    (while (f-exists-p new-name)
      (setq new-name (if (zerop counter)
                         (format "%s.bak" file)
                       (format "%s.%s.bak" file counter)))
      (setq counter (1+ counter)))
    (unless (equal file new-name)
      (rename-file file new-name)
      (message "%s is already exists. it was renamed as %s."
               file new-name))))

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
  (lelde/project/init::--move-to-backup-if-exists "Lelde")
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
      (lelde/project/init::--move-to-backup-if-exists file))
    (apply #'lelde/project/update::update-files files)
    (let ((scripts-dir (plist-get pinfo :scripts-path)))
      (dolist (f (directory-files scripts-dir))
        (let ((file (f-expand f scripts-dir)))
          (when (f-file-p file) (chmod file #o755)))))
    ))


;;;; lelde/elconc

(defun lelde/elconc::bundle (src dst)
  (setq src (expand-file-name src))
  (let* ((pinfo        (lelde/project::get-project-info "."))
         (mods         (lelde/project/modules::get-modules-alist "."))
         (src-path     (plist-get pinfo :src-path))
         (src-feature  (intern (s-replace-regexp "\\(?:\\.src\\)?\\.elc?\\'" ""
                                                 (f-relative src src-path))))
         (depends      (lelde/project/modules::query-internal-dependencies
                        mods src-feature)))
    (with-temp-file dst
      (insert (apply 'elconc-bundled-source
                     src
                     (--map (plist-get (cdr it) :el) depends)
                     )))))

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


;;;; lelde/test/util

;;!export
(defun lelde/test/util::test-rsc-content (test-spec path)
  ""
  (f-read (lelde/test/util::test-rsc-expand test-spec path)))

;;!export
(defun lelde/test/util::test-rsc-copy (test-spec path &rest spec)
  ""
  (let ((from (lelde/test/util::test-rsc-expand test-spec path))
        (to   (f-expand (if (memq :to spec)
                            (plist-get spec :to)
                          (f-filename path)))))
    (if (f-dir-p from)
        (let ((keep-time     (if (memq :keep-time spec)
                                 (plist-get spec :keep-time) t))
              (parents       (if (memq :parents spec)
                                 (plist-get spec :parents) nil))
              (copy-contents (if (memq :copy-contents spec)
                                 (plist-get spec :copy-contents) t)))
          (copy-directory from to keep-time parents copy-contents))
      (with-temp-file to
        (insert-file-contents from)))))

;;!export
(defun lelde/test/util::test-rsc-untar (test-spec tarball &optional options)
  "Expand the TARBALL into `default-directory'.
This function detects compression format from TARBALL's suffix automatically,
But it can be specified by OPTIONS manually."
  (setq options
    (or options
        (cond ((string-match "\\(?:tar\\.?\\|\\.\\)gz\\'"   tarball)"-xzf")
              ((string-match "\\(?:tar\\.?\\|\\.\\)bz2?\\'" tarball)"-xjf")
              ((string-match "\\(?:tar\\.?\\|\\.\\)xz?\\'"  tarball)"-xJf")
              ((string-match "\\(?:tar\\.?\\|\\.\\)z?\\'"   tarball)"-xZf")
              (t "-xf"))))
  (call-process "tar" nil nil nil options
                (lelde/test/util::test-rsc-expand test-spec tarball))
  )

;;!export
(defun lelde/test/util::test-rsc-expand (test-spec path)
  "Return full path to the resource."
  (f-expand path (plist-get test-spec :test-rsc-path)))

;;!export
(defun lelde/test/util::test-byte-compile-no-warnings (&optional test-spec)
  "Test that byte-compiling the package does not produce warnings."
  (setq test-spec (or test-spec (lelde/project::get-project-info ".")))
  (let ((target (f-expand (format "%s.el" (plist-get test-spec :index))
                          (plist-get test-spec :project-path)))
        (byte-compile-error-on-warn t))
    (byte-compile-file target)))


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

;;;###autoload
(defalias 'lelde-init-project 'lelde/project/init::init-project)

;;;###autoload
(defalias 'lelde-update-project-files 'lelde/project/update::update-project-files)

;;;###autoload
(defalias 'lelde-emit-export 'lelde/stmax/emit/export::emit-export)

;;;###autoload
(defvaralias 'lelde-emit-for-index-functions 'lelde/stmax/emit::$emit-for-index-functions)

;;;###autoload
(defalias 'lelde-emit-index-header 'lelde/stmax/emit::emit-index-header)

;;;###autoload
(defalias 'lelde-emit-for-index 'lelde/stmax/emit::emit-for-index)

;;;###autoload
(defalias 'lelde-test-rsc-content 'lelde/test/util::test-rsc-content)

;;;###autoload
(defalias 'lelde-test-rsc-copy 'lelde/test/util::test-rsc-copy)

;;;###autoload
(defalias 'lelde-test-rsc-untar 'lelde/test/util::test-rsc-untar)

;;;###autoload
(defalias 'lelde-test-rsc-expand 'lelde/test/util::test-rsc-expand)

;;;###autoload
(defalias 'lelde-test-byte-compile-no-warnings 'lelde/test/util::test-byte-compile-no-warnings)

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
(defalias 'lelde-tinplate-fill 'lelde/tinplate::tinplate-fill)


(provide 'lelde)
