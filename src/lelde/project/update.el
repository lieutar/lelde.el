;; -*- lexical-binding: t -*-
;;!drop-when-bundled
(provide 'lelde/project/update)
(require 'lelde/META)
(require 'lelde/cli)
(require 'lelde/rsc)
(require 'lelde/project)
(require 'lelde/tinplate)
(require 'lelde/stmax/emit)
;;!end

;;;; lelde/project/update

;;!export
(defun lelde/project/update::update-project-files ()
  (lelde/cli::init)
  (apply #'lelde/project/update::update-files command-line-args-left))

(defun lelde/project/update::update-files (&rest files)
  (dolist (file files)
    (lelde/project/update::update-file file)))

(defsubst lelde/project/update::update-file--make-template-alist (pinfo base)
  (let ((index (plist-get pinfo :index)))
    (let ((templates (lelde/rsc::get-rsc-file-list base)))
      (--map (cons (let ((src it)
                         (match (-map #'cadr (s-match-strings-all
                                              "@\\([^@]*\\)@" it))))
                     (dolist (key match)
                       (let* ((kwd (intern (format ":%s" key)))
                              (val (plist-get pinfo kwd)))
                         (when (stringp val)
                           (setq src (s-replace (format "@%s@" key)
                                                val src)))))
                     (s-replace "@@" "" src))
                   it)
             templates))))

(defun lelde/project/update::update-file (file &optional base)
  (setq base (or base "template"))
  (let* ((pinfo     (lelde/project::get-project-info "."))
         (pp        (plist-get pinfo :project-path))
         (index     (plist-get pinfo :index))
         (t-alist   (lelde/project/update::update-file--make-template-alist
                     pinfo base))
         (template  (let ((slot (assoc file t-alist)))
                      (unless slot
                        (error "Undefined way to update of \"%s\"." file))
                      (lelde/rsc::get-rsc (f-join base (cdr slot))))))
    (let ((dir (f-dirname (f-expand file pp))))
      (unless (f-dir-p dir) (apply #'f-mkdir (f-split dir))))
    (lelde/tinplate::fill pinfo template (f-expand file pp))))

;;(lelde/project/update::update-file "Cask")
;;(lelde/project/update::update-file "Makefile")
;;(lelde/project/update::update-file "init.sh" "bootstrap")

;;(insert "\n" (lelde/project/update::update-file "src/lelde.src.el"))
;;(insert "\n" (ppp-sexp-to-string (lelde/project/update::update-file "Cask")))
