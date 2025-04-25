;; -*- lexical-binding: t -*-
;;!drop-when-bundled
(provide 'lelde/stmax)
(require 'lelde/META)
(require 'lelde/cli)
(require 'lelde/stmax/emit)     ;; utils (with stmax)
;;!end

;;!export
(defun lelde/stmax::stmax-file ()
  (lelde/cli::init)
  (let ((src (nth 0 command-line-args-left)))
    (stmax-file src)))
