;; -*- lexical-binding: t -*-
;;; {{index}}.el --- {{description-brief}}

;; {{copyright}}

;; Author: {{author}}
;; Version: {{version}}
;; Keywords: {{(keywords) (s-join ", " keywourds)}}
;; URL: {{url}}

;;; License:

;; {{lincense}}

;;; Commentary:
;;
;; {{(description) (lelde/smac::smac-el-comment description t)}}
;;
;;  About deteils of this, see: README.md
;;

;;; Code:

;;!drop-when-bundled
(require 'lelde/META)
(require 'lelde/customize)

(require 'lelde/project/init)   ;; cli
(require 'lelde/project/update) ;; cli
(require 'lelde/fill)           ;; cli (with tinplate)
(require 'lelde/bundle)         ;; cli (with elconc)
(require 'lelde/stmax/emit)     ;; utils (with stmax)

(require 'lelde/test) ;; test helper
(require 'lelde/dpac) ;; development package manager
;;!end
;;!bundle
;;!static-macro
(lelde/stmax/emit::emit-for-index)

(lelde/init::init)
(provide 'lelde)
