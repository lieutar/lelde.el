;; -*- lexical-binding: t -*-
(provide 'c0000/sub)

;;!export
(defun c0000/sub::sub-ex-a ()
  "document of a")

;;!export (interactive (list 1 2 3 4))
(defun c0000/sub::sub-ex-cmd (a &optional b &rest c)
  "document of sub-ex-cmd")

;;!export (interactive)
(defun c0000/sub::sub-no-args ())

;;!export (interactive (list 1 2 3))
(defun c0000/sub::sub-with-args (a b c))

;;!export (interactive (list 1 2))
(defun c0000/sub::sub-with-optional (a &optional b))

;;!export
(defvar c0000/sub::$var-a nil)


(defun c0000/sub::internal ())
