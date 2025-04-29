;; -*- lexical-binding: t -*-

;;!drop-when-bundled
(provide '{{index}}/core)
(require '{{index}}/META)
;;!end

;;!export (interactive)
(defun {{index}}/core::hello ()
  "Show the message \"Hello {{index}} !!\"."
  (message "Hello {{index}} !!"))
