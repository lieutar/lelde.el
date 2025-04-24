;; -*- lexical-binding: t -*-
;;!drop-when-bundled
(provide 'lelde/tinplate)
(require 'lelde/META)
(require 'lelde/cli)
(require 'lelde/project)
(require 'lelde/tinplate/util)
;;!end

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
