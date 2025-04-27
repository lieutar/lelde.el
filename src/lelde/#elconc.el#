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
  (let* ((pinfo        (lelde/project::get-project-info "."))
         (mods         (lelde/project/modules::get-modules-alist "."))
         (src-path     (plist-get pinfo :src-path))
         (src-feature  (intern (s-replace-regexp "\\(?:\\.src\\)?\\.elc?\\'" ""
                                                 (f-relative src src-path))))
         (depends      (lelde/project/modules::query-internal-dependencies
                        mods src-feature)))
    (with-temp-file dst
      (insert (apply 'elconc-bundled-source
                     src
                     (--map (plist-get (cdr it) :el) depends)
                     )))))

;;!export
(defun lelde/elconc::elconc-bundle ()
  (lelde/cli::init)
  (let ((src (nth 0 command-line-args-left))
        (dst (nth 1 command-line-args-left)))
    (lelde/elconc::bundle src dst)))
