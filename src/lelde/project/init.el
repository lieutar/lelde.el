;; -*- lexical-binding: t -*-

;;!drop-when-bundled
(provide 'lelde/project/init)
(require 'lelde/META)
(require 'lelde/rsc)
(require 'lelde/project)
(require 'lelde/project/update)
;;!end

;;; lelde/project/init

(defun lelde/project/init::init-project ()
  ""
  (unless (prinfo/git::is-valid-dot-git-p ".git")
    (unless (zerop (shell-command "git init" nil nil))
      (error "\"git init\" was failsed"))
    (unless (prinfo/git::is-valid-dot-git-p ".git")
      (error
       "WTH? \"git init\" was success. However, there isn't valid \".git\"")))
  (when (f-exists-p "Cask") (delete-file "Cask"))
  (when (f-exists-p "Lelde")
    (message "Lelde is already exists. it was renamed as Lelde.bak")
    (rename-file "Lelde" "Lelde.bak"))
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
      (when (f-exists-p file)
        (rename-file file (format "%s.bak" file))
        (message "%s is already exists. it was renamed as %s.bak" file file)))
    (apply #'lelde/project/update::update-files files)))

;;(let((default-directory"~/work/emacs/elminfo.el"))(lelde/project/init::init-project))
