;; -*- lexical-binding: t -*-
;;!drop-when-bundled
(provide 'lelde/stmax/emit/export)
(require 'lelde/META)
(require 'lelde/project)
(require 'lelde/project/modules)
;;!end

;;;; lelde/stmax/emit/export

;;(s-match "[^[:space:]\n\r]" "\n")
(defun lelde/stmax/emit/export::parse-arg (src)
  (let ((arg-sexp (and (s-match "[^[:space:]\n\r]" src)
                       (read src))))
    (when (and arg-sexp
               (not (and(listp arg-sexp)
                        (eq (car arg-sexp) 'interactive))))
      (error ";;!export macro accepts only (interactive) form."))
    arg-sexp))

(defsubst lelde/stmax/emit/export::--export-as (symbol)
  (intern (s-replace-regexp "\\`\\([^/]+\\).*?::\\$?" "\\1-"
                            (symbol-name symbol))))

(defun lelde/stmax/emit/export::parse-sexp--defvar   (sexp arg)
  (when arg (error "exporting variable can't accept any args"))
  (let* ((name      (cadr sexp))
         (export-as (lelde/stmax/emit/export::--export-as name))
         (export-form `(defvaralias ',export-as ',name)))
    (list :type 'defvar
          :symbol name :export-as export-as :export-form export-form)))

(defun lelde/stmax/emit/export::parse-sexp--defconst (sexp arg)
  (when arg (error "exporting constant can't accept any args"))
  (let ((result (lelde/stmax/emit/export::parse-sexp--defvar sexp arg)))
    (plist-put :type 'defconst)
    result))

(defun lelde/stmax/emit/export::parse-sexp--defmacro (sexp arg)
  (when arg (error "exporting macro can't accept any args"))
  (let ((result (lelde/stmax/emit/export::parse-sexp--defun sexp arg)))
    (plist-put result :type 'defmacro)
    result))

(defsubst lelde/stmax/emit/export::delegating-form (func arg-list)
  (let ((stripped-arg-list (--filter (not (s-match "\\`&" (symbol-name it)))
                                     arg-list)))
    (if (member '&rest arg-list)
        `(apply #',func ,@stripped-arg-list)
      `(,func ,@stripped-arg-list))))

(defun lelde/stmax/emit/export::parse-sexp--defun (sexp arg)
  (let ((name              (cadr sexp))
        (arg-list          (caddr sexp))
        (doc-or-first-sexp (cadddr sexp)))
    (let* ((export-as (lelde/stmax/emit/export::--export-as name)))
      (list
       :type 'defun :symbol name :export-as export-as
       :export-form
       (if arg
           `(defun ,export-as ,arg-list
              ,@(and (stringp doc-or-first-sexp) (list doc-or-first-sexp))
              ,arg
              ,(lelde/stmax/emit/export::delegating-form name arg-list))
       `(defalias ',export-as ',name))))))

(defun lelde/stmax/emit/export::parse-sexp (src arg)
  (unless (s-match "\\`[[:space:]\n\r]*(" "\n  \n(")
    (error "sexp after ;;!export have to be a list without any comments"))
  (let* ((sexp   (read src))
         (type   (car sexp))
         (parser (intern (format "lelde/stmax/emit/export::parse-sexp--%s"
                                 type))))
    (if parser (funcall parser sexp arg)
      (error "Undefined exporting method for: %s" type))))

(defsubst lelde/stmax/emit/export::--parse-file (file)
  (let ((r-result nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "^;;!export\\b" nil t)
        (let ((arg-start (point))
              (arg nil)
              (sexp nil))
          (move-beginning-of-line 2)
          (let* ((start (point))
                 (end (progn (condition-case nil (forward-sexp)(error))
                             (point))))
            (setq arg  (lelde/stmax/emit/export::parse-arg
                        (buffer-substring-no-properties arg-start start)))
            (setq sexp (buffer-substring-no-properties start end)))
          (push  (append (list :src file
                               :arg  arg)
                         (lelde/stmax/emit/export::parse-sexp sexp arg))
                 r-result))))
    (reverse r-result)))

(defun lelde/stmax/emit/export::aggregate-exporting-information (project-spec)
  (let* ((project-root (plist-get project-spec :project-path))
         (src-path     (plist-get project-spec :src-path))
         (files (--map
                 (let ((plist (cdr it)))
                   (or (plist-get plist :el)
                       (plist-get plist :src)))
                 (lelde/project/modules::get-modules-alist project-root)))
         (result (list :result)))
    (dolist (file files)
      (nconc result (lelde/stmax/emit/export::--parse-file file)))
    (cdr result)))

;;!export
(defun lelde/stmax/emit/export::emit-export (project-spec)
  (s-join
   "\n"
   (->> (lelde/stmax/emit/export::aggregate-exporting-information project-spec)
        (--map (list ";;;###autoload"
                     (ppp-sexp-to-string (plist-get it :export-form))))
        -flatten)))

(add-hook 'lelde/stmax/emit::$emit-for-index-functions
          #'lelde/stmax/emit/export::emit-export)
