;; -*- lexical-binding: t -*-

;;!drop-when-bundled
(require 'lelde/META)
;;!end

(defvar lelde/init::$init-hook nil)

;;!export (interactive)
(defun lelde/init::init ()
  (run-hooks lelde/init::$init-hook))
