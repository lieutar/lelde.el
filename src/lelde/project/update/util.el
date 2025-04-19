;; -*- lexical-binding: t -*-

;;!drop-when-bundled
(provide 'lelde/project/update/util)
(require 'lelde/META)
;;!end

;;;; lelde/project/update/util

(defun lelde/project/update/util::Cask-depends-on (dependency indent)
  (s-replace-regexp
   "^" indent
   (mapconcat
   (lambda (it)
     (let* ((pkg   (car it))
            (plist (cdr it))
            (type  (plist-get plist :type)))
       (cond ((eq type 'core)
              (format ";; %s (core)\n" pkg)
              )
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

(defun lelde/project/update/util::index-pr (depends)
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


(defconst lelde/project/update/util::$Cask-additional-source
  `())

(defun lelde/project/update/util::Cask-sources (sources)
  (mapconcat
   (lambda (it)
     (ppp-sexp-to-string
      (if (listp it) (cons 'source it)
        (list 'source it))))
   sources))

;; (lelde/project/update/util::Cask-sources '(gnu melpa))

(defun lelde/project/update/util::recipe (@ENV &optional local)
  (let ((index   (cdr (assoc "index"   @ENV)))
        (version (cdr (assoc "version" @ENV)))
        (drop-version-on-recipe (cdr (assoc "drop-version-on-recipe" @ENV)))
        (url     (cdr (assoc "url"     @ENV)))
        (repo    (cdr (assoc "repo"    @ENV)))
        (files   (cdr (assoc "files"   @ENV)))
        (repo-info nil))
    (when (and (stringp url) (null repo)) (setq repo url))
    (when repo
      (cond
       ((string-match
         "\\`https://\(github\|gitlab\)\.com/\([^/]+/[^/]+\)/?\\'" repo)
        (let ((fetcher))))
       ))
    (or (when repo-info
          (format "%S"
                  (list (intern index))
                  ,@()
                  ,@()
                  ,@()))
        (format
         ";; WARNING THIS PROJECT DOESN'T HAVE THE DEFINITION OF THE REPOSITORY.
;;
;; At first, add :url or :repo on the `Lelde' file, and run
;;
;;     make recipe/%s
;;
;; (:repo is required, if the url isn't github / gitlab repository's url. )
" index))))
