;; -*- lexical-binding: t -*-
;;!drop-when-bundled
(provide 'lelde/project/modules)
(require 'lelde/META)
(require 'lelde/project)
;;!end

;;;; lelde/project/modules

(defsubst lelde/project/modules::modules-alist--collect-from-fs (project base)
  (let ((files nil))
    (dolist (fmt '("%s.el" "%s.src.el"))
      (let ((file (f-expand (format fmt project) base)))
        (when (f-exists-p file)(setq files (cons file files)))))
    (dolist (file (directory-files-recursively
                     (f-expand (symbol-name project) base)
                     "\\.el\\'"))
      (when (not (s-match "\\`\\.?#" (f-base file)))
        (setq files (cons file files))))
    (--map
     (list
      (intern (s-replace-regexp "\\(?:\\.src\\)?\\.el\\'" ""
                                (f-relative it base)))
      (if (s-match "\\.src\\.el\\'" it) :src :el) it)
     files)))

(defsubst lelde/project/modules::modules-alist--mearge-duplicated (all)
  (let ((result nil))
    (dolist (slot all)
      (let* ((feature (car slot))
             (previous-slot (assq feature result)))
        (if previous-slot
            (nconc previous-slot (cdr slot))
          (setq result (cons slot result)))))
    result))

(defsubst lelde/project/modules::modules-alist--collect-dependencies (project
                                                                      result)
  (dolist (slot result)
    (let* ((plist   (cdr slot))
           (depends (lelde/project/modules::get-dependencies-from-file
                     (or (plist-get (cdr slot) :src)
                         (plist-get (cdr slot) :el))))
           (internalp (lambda (sym)
                        (s-match (format "\\`%s\\(?:/.*\\)?\\'"
                                         project)
                                 (symbol-name sym))))
           internal external)
      (dolist (mod depends)
        (if (funcall internalp mod)
            (setq internal (cons mod internal))
          (setq external (cons mod external))))
      (plist-put plist :depends-internal internal)
      (plist-put plist :depends-external external))))

(defun lelde/project/modules::get-dependencies-from-file (file)
  "Get a list of dependencies from FILE by searching for `require` forms.

This function has several limitations:
1. It does not handle macros that may expand to `require` forms.
2. It may incorrectly identify `require` forms in argument lists.
3. It does not consider conditional `require` forms
   (e.g., inside `when` or `if`).
4. It may not handle all possible syntaxes of `require`.
5. The argument of require have to be a quoted symbol.

For accurate dependency analysis, manual inspection or more sophisticated
parsing techniques may be necessary."
  (let* ((src (with-temp-buffer
                (insert-file-contents file)
                (buffer-substring-no-properties (point-min) (point-max))))
         (read-src (ignore-errors (read (format "(\n%s\n)" src))))
         (result nil))
    (letrec ((recurse (lambda (tree)
                        (when (and tree (listp tree))
                          (if (eq 'require (car tree))
                              (push (cadr tree) result)
                            (while (and tree (listp tree))
                              (let ((node (car tree)))
                                (funcall recurse node)
                                (setq tree (cdr tree)))))))))
      (funcall recurse read-src))
    (-map 'cadr (--filter (and (listp it) (eq 'quote (car it))) result))))

(defun lelde/project/modules::get-modules-alist (project-root)
  ""
  (let* ((pinfo  (lelde/project::get-project-info project-root))
         (index  (plist-get pinfo :index)))
    (when (stringp index) (setq index (intern index)))
    (let* ((base   (plist-get pinfo :src-path))
           (all    (lelde/project/modules::modules-alist--collect-from-fs index
                                                                          base))
          (result (lelde/project/modules::modules-alist--mearge-duplicated all)))
      (lelde/project/modules::modules-alist--collect-dependencies index result)
      result)))

(defun lelde/project/modules::query-internal-dependencies (mods feature)
  (letrec ((recurse
            (lambda (feature current-path)
              (when (memq feature current-path)
                (error "Cycled dependencies: %s > %s"
                       (s-join " > " (-map 'symbol-name (reverse current-path)))
                       feature))
              (push feature current-path)
              (let* ((slot  (assq feature mods))
                     (plist (cdr slot))
                     (deps  (plist-get plist :depends-internal)))
                (let ((results
                       (->> deps
                            (-map (lambda (dep)
                                    (append (funcall recurse dep current-path)
                                            (list dep))))
                            -flatten)))
                  (pop current-path)
                  results)))))
    (--map (assq it mods) (delete-dups (funcall recurse feature nil)))))
