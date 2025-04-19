;; -*- lexical-binding: t -*-
{{()(lelde/stmax/emit::emit-index-header)}}

;;; Code:

;;!drop-when-bundled
(require '{{index}}/META)
;;!end
;;!bundle
;;!static-macro
(lelde-emit-for-index)

(provide '{{index}})
;; {{index}}.el ends here.
