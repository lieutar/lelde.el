;; -*- lexical-binding: t -*-
;;!drop-when-bundled
(provide lelde/fill)
(require 'lelde/META)
(require 'lelde/cli)
;;!end

;;!export
(defun lelde/fill::fill ()
  (lelde/cli::init)
  )
