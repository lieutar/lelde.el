(require 'ert)
(require 'check-declare)
(require 'checkdoc)

(ert-deftest {{index}}/check-declare ()
  "Test integrated files by check-declare"
  (should-not (check-declare-file (locate-library "{{index}}.el"))))

(ert-deftest {{index}}/check-doc ()
  "Test integrated files by checkdoc"
  (should-not (checkdoc-file (locate-library "{{index}}.el"))))
