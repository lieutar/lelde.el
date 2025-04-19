(require 'ert)
(require 'lelde)
(ert-deftest byte-compile-{{index}}-with-no-warnings ()
  "Test"
  (should (lelde-test-byte-compile-no-warnings)))
