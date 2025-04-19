;; -*- lexical-binding: t -*-
;;!drop-when-bundled
(provide 'lelde/bundle)
(require 'lelde/META)
(require 'lelde/project)
(require 'lelde/cli)
;;!end

;;;; lelde/bundle
;;!export
(defun lelde/bundle::bundle ()
  (lelde/cli::init)
  )
