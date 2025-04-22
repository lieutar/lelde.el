;; -*- lexical-binding: t -*-
;;!drop-when-bundled
(provide lelde/fill)
(require 'lelde/META)
(require 'lelde/cli)
;;!end

;;;; lelde/tinplate
;;!export
(defun lelde/tinplate::fill ()
  (lelde/cli::init)
  )
