;; -*- lexical-binding: t -*-
;;!drop-when-bundled
(provide 'lelde/project/init)
(require 'lelde/META)
(require 'lelde/rsc)
(require 'lelde/project)
(require 'lelde/project/update)
;;!end

;;; lelde/project/init

(defun lelde/project/init::--move-to-backup-if-exists (file)
  (let ((new-name file)
        (counter  0))
    (while (f-exists-p new-name)
      (setq new-name (if (zerop counter)
                         (format "%s.bak" file)
                       (format "%s.%s.bak" file counter)))
      (setq counter (1+ counter)))
    (unless (equal file new-name)
      (rename-file file new-name)
      (message "%s is already exists. it was renamed as %s."
               file new-name))))

;;!export
(defun lelde/project/init::init-project ()
  ""
  (unless (prinfo/git::is-valid-dot-git-p ".git")
    (unless (zerop (shell-command "git init" nil nil))
      (error "\"git init\" was failsed"))
    (unless (prinfo/git::is-valid-dot-git-p ".git")
      (error
       "WTH? \"git init\" was success. However, there isn't valid \".git\"")))
  (when (f-exists-p "Cask") (delete-file "Cask"))
  (lelde/project/init::--move-to-backup-if-exists "Lelde")
  (lelde/cli::init)
  (let* ((pinfo (lelde/project::get-project-info "."))
         (index (plist-get pinfo :index))
         (skipped nil)
         (files
          (cons "Lelde"
                (->>
                 (lelde/project/update::update-file--make-template-alist
                  pinfo "template")
                 (-map #'car)
                 (--filter (not (string= "Lelde" it)))))))
    (dolist (file files)
      (lelde/project/init::--move-to-backup-if-exists file))
    (apply #'lelde/project/update::update-files files)
    (let ((scripts-dir (plist-get pinfo :scripts-path)))
      (dolist (f (directory-files scripts-dir))
        (let ((file (f-expand f scripts-dir)))
          (when (f-file-p file) (chmod file #o755)))))
    ))
