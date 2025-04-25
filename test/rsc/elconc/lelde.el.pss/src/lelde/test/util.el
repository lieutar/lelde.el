;; -*- lexical-binding: t -*-
;;!drop-when-bundled
(provide 'lelde/test/util)
(require 'lelde/project)
(require 'lelde/META)
;;!end

;;;; lelde/test/util

;;!export
(defun lelde/test/util::test-rsc-content (test-spec path)
  ""
  (f-read (lelde/test/util::test-rsc-expand test-spec path)))

;;!export
(defun lelde/test/util::test-rsc-copy (test-spec path &rest spec)
  ""
  (let ((from (lelde/test/util::test-rsc-expand test-spec path))
        (to   (f-expand (if (memq :to spec)
                            (plist-get spec :to)
                          (f-filename path)))))
    (if (f-dir-p from)
        (let ((keep-time     (if (memq :keep-time spec)
                                 (plist-get spec :keep-time) t))
              (parents       (if (memq :parents spec)
                                 (plist-get spec :parents) nil))
              (copy-contents (if (memq :copy-contents spec)
                                 (plist-get spec :copy-contents) t)))
          (copy-directory from to keep-time parents copy-contents))
      (with-temp-file to
        (insert-file-contents from)))))

;;!export
(defun lelde/test/util::test-rsc-untar (test-spec tarball &optional options)
  "Expand the TARBALL into `default-directory'.
This function detects compression format from TARBALL's suffix automatically,
But it can be specified by OPTIONS manually."
  (setq options
    (or options
        (cond ((string-match "\\(?:tar\\.?\\|\\.\\)gz\\'"   tarball)"-xzf")
              ((string-match "\\(?:tar\\.?\\|\\.\\)bz2?\\'" tarball)"-xjf")
              ((string-match "\\(?:tar\\.?\\|\\.\\)xz?\\'"  tarball)"-xJf")
              ((string-match "\\(?:tar\\.?\\|\\.\\)z?\\'"   tarball)"-xZf")
              (t "-xf"))))
  (call-process "tar" nil nil nil options
                (lelde/test/util::test-rsc-expand test-spec tarball))
  )

;;!export
(defun lelde/test/util::test-rsc-expand (test-spec path)
  "Return full path to the resource."
  (f-expand path (plist-get test-spec :test-rsc-path)))

;;!export
(defun lelde/test/util::test-byte-compile-no-warnings (&optional test-spec)
  "Test that byte-compiling the package does not produce warnings."
  (setq test-spec (or test-spec (lelde/project::get-project-info ".")))
  (let ((target (f-expand (format "%s.el" (plist-get test-spec :index))
                          (plist-get test-spec :project-path)))
        (byte-compile-error-on-warn t))
    (byte-compile-file target)))
