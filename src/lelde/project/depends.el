;; -*- lexical-binding: t -*-
;;!drop-when-bundled
(provide 'lelde/project/depends)
(require 'lelde/META)
;;!end

;;; lelde/project/depends

(defun lelde/project/depends::is-core-module-p (package)
  (let ((path (locate-library (symbol-name package))))
    (and path
         (string-match (format "\\`%s" (regexp-quote lisp-directory)) path)
         t)))

(defsubst lelde/project/depends::detect-repository (package)
  ""
  (if (lelde/project/depends::is-core-module-p package) 'core 'elpa))


(defun lelde/project/depends::parse-version-string--local (location)
   (list :type 'local :location location))

(defun lelde/project/depends::parse-version-string (package verstr)
  ""
  (cond ;; local
        ((s-match "\\`\\(?:\\~\\|/\\)" verstr)
         (lelde/project/depends::parse-version-string--local
          (f-expand verstr)))

        ((s-match "\\`file://" verstr)
         (lelde/project/depends::parse-version-string--local
          (f-expand (s-replace-regexp "\\`file://" "" verstr))))

        ((s-match "\\`\\(?:https?\\|ftp\\)://" verstr)
         (list :type 'remote :location verstr))

        ((s-match "[^[:space:]]" verstr)
         (list :type (lelde/project/depends::detect-repository package)
               :version verstr))
        (t
         (list :type    (lelde/project/depends::detect-repository package)
               :version "0"))))

(defun lelde/project/depends::parse-depends-list (depends-list)
  "Completes lacked infoprmations from Lelde's :depends and
:dev-depends lists from user's emacs environment."
  (let ((r-result nil))
    (while depends-list
      (let ((package (car  depends-list))
            (version (cadr depends-list)))
        (push (cons package
                    (if (stringp version)
                        (progn
                          (setq depends-list (cddr depends-list))
                          (lelde/project/depends::parse-version-string
                           package version))
                      (progn
                        (setq depends-list (cdr depends-list))
                        (list :type
                              (lelde/project/depends::detect-repository package)
                              :version "0"))))
                    r-result)))
    (reverse r-result)))
