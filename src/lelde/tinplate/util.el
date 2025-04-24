;; -*- lexical-binding: t -*-

;;!drop-when-bundled
(provide 'lelde/tinplate/util)
(require 'lelde/META)
;;!end

;;;; lelde/tinplate/util

(defun lelde/tinplate/util::Cask-depends-on (dependency indent)
  (s-replace-regexp
   "^" indent
   (mapconcat
   (lambda (it)
     (let* ((pkg   (car it))
            (plist (cdr it))
            (type  (plist-get plist :type)))
       (cond ((eq type 'core)
              (format ";; %s (core)\n" pkg))
             ((eq type 'elpa)
              (ppp-sexp-to-string
               `(depends-on ,(symbol-name pkg) ,(plist-get plist :version))))
             ((eq type 'local)
              (format ";; %s file\n" pkg))
             ((eq type 'lelde)
              (ppp-sexp-to-string
               `(depends-on ,(symbol-name pkg) :git ,(plist-get plist :git))))
             (t (format ";; %s UNKNOWN (%s)\n" pkg type)))
       ))
   dependency)))

(defun lelde/tinplate/util::index-pr (depends)
  (let* ((warnings nil)
         (pr-list (mapconcat
                   (lambda (it)
                     (let* ((pkg     (car it))
                            (plist   (cdr it))
                            (type    (plist-get plist :type))
                            (version (plist-get plist :version)))
                       (cond ((eq type 'core) "")
                             ((eq type 'elpa)
                              (format "%S"
                                      (cons pkg
                                            (when (and (stringp version)
                                                       (not (string= "0"
                                                                      version)))
                                              (list version)))))
                             (t (push pkg warnings)))))
                   depends)))
    (format "(%s)%s" pr-list
            (if warnings (format "
;; WARNING
;; This module depends following packages where aren't on any package archives:
;; %s
" (s-join ", " warnings)) ""))))


(defconst lelde/tinplate/util::$Cask-additional-source
  `())

(defun lelde/tinplate/util::Cask-sources (sources)
  (mapconcat
   (lambda (it)
     (ppp-sexp-to-string
      (if (listp it) (cons 'source it)
        (list 'source it))))
   sources))

;; (lelde/tinplate/util::Cask-sources '(gnu melpa))

(defun lelde/tinplate/util::recipe (@ENV)
  (let ((index   (cdr (assoc "index"   @ENV)))
        (url     (cdr (assoc "url"     @ENV)))
        (repo    (cdr (assoc "repo"    @ENV)))
        (files   (cdr (assoc "files"   @ENV)))
        (errors  nil))
    (let ((recipe (list index)))
      (when (and (stringp url) (null repo)) (setq repo url))
      (if repo
        (cond
         ((string-match "\\`https://\\(github\\|gitlab\\)\\.com/\\(.+\\)" repo)
          (nconc recipe (list:fetcher (intern (match-string 1 repo))
                                      :repo    (match-string 2 repo)))))
        (push (format "
;; WARNING THIS PROJECT DOESN'T HAVE THE DEFINITION OF THE REPOSITORY.
;;
;; At first, add :url or :repo on the `Lelde' file, and run
;;
;;     make %s
;;
;; (:repo is required, if the url isn't github / gitlab repository's url. )
"
                      (f-join (cdr (assoc "recipe-dir" @ENV)) index))
              errors))
      (if errors (s-join "\n" errors)
        (ppp-sexp-to-string recipe)))))

(defun lelde/tinplate/util::make-phoeny-macro (@ENV)
  (let ((files-to-update (cdr (assoc "files-to-update" @ENV))))
    (format
     "
PHONY := help all build package clean clean-all clean-cask update%s\\
	test test-unit test-integration"
     (if (member "Makefile" files-to-update)
         " Makefile-itself" ""))))

(defun lelde/tinplate/util::update-tasks (@ENV)
  (let ((files-to-update (cdr (assoc "files-to-update" @ENV)))
        (template-alist  (cdr (assoc "template-alist" @ENV))))
    (concat
     "
update :=
"
     (mapconcat (lambda (file) (format "
update := $(update) %s
%s: Lelde
\t$(lelde_update) $@
"
                                       file file file))
                (--filter (not (string= "Makefile" it)) files-to-update))
     (mapconcat (lambda (pair)
                  (let ((src (car pair))
                        (dst (cadr pair)))
                    (format "
update := $(update) %s
%s: %s Lelde
\t$(lelde_fill) $< $@
"
                            dst dst src)))
                template-alist)
     (if (member "Makefile" files-to-update)
         "
#>Makefile-itself
#>    Update Makefile itself.
#>
Makefile-itself:
\t$(lelde_update) Makefile

update: $(update) Makefile-itself
"
     "
update: $(update)
"))))
