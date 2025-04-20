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
      (plist-put cfg :src-path
                 (f-expand (plist-get cfg :src-dir) project-root))
      (plist-put cfg :scripts-path
                 (f-expand (plist-get cfg :scripts-dir) project-root))
      (plist-put cfg :test-scripts-path
                 (f-expand (plist-get cfg :test-scripts-dir) project-root))
      (plist-put cfg :test-lib-path
                 (f-expand (plist-get cfg :test-lib-dir) project-root))
      (plist-put cfg :test-rsc-path
                 (f-expand (plist-get cfg :test-rsc-dir) project-root))
      cfg)))
