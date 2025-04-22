;; -*- lexical-binding: t -*-
;;!drop-when-bundled
(provide 'lelde/bundle)
(require 'lelde/META)
(require 'lelde/project)
(require 'lelde/cli)
;;!end

;;;; lelde/elconc

;;!export
(defun lelde/elconc::bundle ()
  (lelde/cli::init)
  )
