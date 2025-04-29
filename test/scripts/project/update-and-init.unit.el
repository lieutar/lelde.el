;; -*- lexical-binding: t -*-
(require 'lelde/project/update)
(require 'lelde/project/init)
(require 'lelde/test)
(lelde/test::test-setup)
(require 'lelde-test/project/update-and-init)
(lelde-test/project/update-and-init lelde/project/init::init-project)
