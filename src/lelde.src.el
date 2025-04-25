;;!static-macro
(lelde/stmax/emit::emit-index-header)

;;!drop-when-bundled
(require 'lelde/META)
(require 'lelde/project/init)   ;; cli
(require 'lelde/project/update) ;; cli
(require 'lelde/tinplate)       ;; cli (with tinplate)
(require 'lelde/elconc)         ;; cli (with elconc)
(require 'lelde/stmax)          ;; cli (with stmax)
(require 'lelde/test)           ;; test helper
;;!end
;;!insert-bundled
;;!static-macro
(lelde/stmax/emit::emit-for-index)

(provide 'lelde)
