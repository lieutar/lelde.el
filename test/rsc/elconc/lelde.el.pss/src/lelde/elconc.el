;; -*- lexical-binding: t -*-
;;!drop-when-bundled
(provide 'lelde/elconc)
(require 'lelde/META)
(require 'lelde/project)
(require 'lelde/project/modules)
(require 'lelde/cli)
;;!end

;;;; lelde/elconc

(defun lelde/elconc::bundle (src dst)
  (setq src (expand-file-name src))
  (let ((mods (lelde/project/modules::get-modules-alist ".")))
    (with-temp-file dst
      (insert (apply 'elconc-bundled-source
                     src
                     (->> mods
                          (--map (plist-get (cdr it) :el))
                          (--filter (not (equal it src)))
                          ))))))

;;!export
(defun lelde/elconc::elconc-bundle ()
  (lelde/cli::init)
  (let ((src (nth 0 command-line-args-left))
        (dst (nth 1 command-line-args-left)))
    (lelde/elconc::bundle src dst)))
