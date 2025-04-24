;; -*- lexical-binding: t -*-

;;!drop-when-bundled
(require 'lelde/META)
;;!end

;;;; lelde/init
;;!export
(defvar lelde/init::$init-hook nil)

;;!export (interactive)
(defun lelde/init::init ()
  (run-hooks lelde/init::$init-hook))
