;; -*- lexical-binding: t -*-
;;!drop-when-bundled
(provide 'lelde/cli)
(require 'lelde/META)
(require 'lelde/project)
;;!end
;;;; lelde/cli
(defconst lelde/cli::$project-info-cache nil)

(defun lelde/cli::get-project-info (orig-fun project-root)
  (setq project-root (f-expand project-root))
  (let* ((cache  lelde/cli::$project-info-cache)
         (result (cdr (assoc project-root (cdr cache)))))
    (unless result
      (setq result (funcall orig-fun project-root))
      (nconc cache (list (cons project-root result))))
    result))

(defun lelde/cli::init ()
  (unless lelde/cli::$project-info-cache

    (setq lelde/cli::$project-info-cache '(cached))
    (let* ((proot (f-expand (prinfo/git::project-root ".")))
           (local-lelde-config.el (f-expand "local-lelde-config.el" proot)))
      (when (f-exists-p local-lelde-config.el)
        (load local-lelde-config.el)))

    (advice-add #'lelde/project::get-project-info :around
                #'lelde/cli::get-project-info)))
