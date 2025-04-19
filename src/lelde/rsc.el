;; -*- lexical-binding: t -*-
;;!drop-when-bundled
(provide 'lelde/rsc)
(require 'lelde/META)
(require 'lelde/project)
;;!end
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
