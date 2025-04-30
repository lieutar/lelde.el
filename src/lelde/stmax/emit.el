;; -*- lexical-binding: t -*-
;;!drop-when-bundled
(provide 'lelde/stmax/emit)
(require 'lelde/META)
(require 'lelde/project)
(require 'lelde/stmax/emit/export)
(require 'lelde/rsc)
;;!end

;;!export
(defvar lelde/stmax/emit::$emit-for-index-functions '())

;;!export
(defun lelde/stmax/emit::emit-index-header ()
  ""
  (lelde/rsc::get-rsc "common/index-header.el@@"))

;;!export
(defmacro lelde/stmax/emit::emit-for-index ()
  `(let ((pspec
          (lelde/project::get-project-info stmax-current-processing-file)))
     (mapconcat (lambda (it) (funcall it pspec))
                lelde/stmax/emit::$emit-for-index-functions
                "\n")))
