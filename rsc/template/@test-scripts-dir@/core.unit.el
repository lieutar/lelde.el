(require 'ert)
(require '{{index}}/core)

(ert-deftest {{index}}-test/core::hello ()
  "Hello function is defined."
  (should (fboundp '{{index}}/core::hello )))

;; This is the sample of unit test script.
;; `ert' tests can be used under `lelde' by default.
;; If you want to use other test frameworks customize your Lelde meta eld file.
;; for example following setting:
;;
;; :test-feature "-l buttercup"
;; :test-runner  "-f buttercup-run"
;;
;; it can activate using buttercup with `ert'
