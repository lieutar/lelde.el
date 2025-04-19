;; -*- lexical-binding: t -*-

;;!drop-when-bundled
(provide 'lelde/project)
(require 'lelde/META)
(require 'lelde/project/depends)
;;!end
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
         (author (format "%s <%s>" user-full-name user-mail-address))
         (copyright (format "%s %s"
                            (decoded-time-year (decode-time (current-time)))
                            author)))
    (list :name            name
          :index           index
          :brief           ""
          :commentary      ""
          :files           nil
          :copyright       copyright
          :author          author
          :emacs           emacs-version
          :project-path    project-root
          :sources        '(gnu melpa)
          :dependency      nil
          :dev-dependency nil
          :test-feature   ""
          :test-runner    ""
          :src-dir        "src"
          :tests-dir      "tests"
          :test-lib-dir   "tests/lib"
          :test-rsc-dir   "tests/rsc")))

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
  :project-path (string) - The root directory of the project (containing .git).
  :brief (string) - Short description of the module.
  :commentary (string) - Long description of the module.
  :files (list<string>) - List of file globs of resource files.
  :copyright (string) -
  :author (string) -
  :emacs (string) - Emacs version to require .
  :sources (list<symbol|list<string>>) -
  :dependency (list<symbol|string>) -
  :dev-dependency (list<symbol|string>) -
  :test-feature (string) -
  :test-runner (string) -

  :src-dir (string, optional) - The directory containing elisp files.
         Defaults to \"\".
         If provided, it will be expanded relative to the project root.
  :src-path (string) - The realpath of the :src-dir.

  :tests-dir (string, optional) - The directory containing test scripts.
         Defaults to \"tests\".
         If provided, it will be expanded relative to the project root.
  :tests-path (string) - The realpath of the :tests-dir.

  :test-lib-dir (string, optional) - The directory containing libraries for tests.
         Defaults to (f-join :tests-dir \"lib\").
         If provided, it will be expanded relative to the project root.
  :tests-lib-dir (string) - An alias for :test-lib-dir.
         Use this option to specify the test library directory at
         the project level.
  :test-lib-path (string) - The realpath of the :test-lib-dir.
  :tests-lib-path (string) - An alias for :test-lib-path.

  :test-rsc-dir (string, optional) - The directory containing resources for tests.
    Defaults to (f-join :tests-dir \"rsc\").
    If provided, it will be expanded relative to the project root.
  :tests-rsc-dir (string) - An alias for :test-rsc-dir.
    Use this option to specify the test resource directory at the project level.
  :test-rsc-path (string) - The realpath of the :test-rsc-dir.
  :tests-rsc-path (string) - An alias for :test-rsc-path.
"
  (let ((project-root (lelde/project::find-project-root path)))
    (let ((cfg (lelde/project::read-project-config project-root)))
      (plist-put cfg :src-path
                 (f-expand (plist-get cfg :src-dir) project-root))
      (plist-put cfg :tests-path
                 (f-expand (plist-get cfg :tests-dir) project-root))
      (plist-put cfg :test-lib-path
                 (f-expand (plist-get cfg :test-lib-dir) project-root))
      (plist-put cfg :test-rsc-path
                 (f-expand (plist-get cfg :test-rsc-dir) project-root))
      cfg)))
