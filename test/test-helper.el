(require 'f)

(defvar atilde-test-path
  (f-dirname load-file-name))

(defvar atilde-root-path
  (f-parent atilde-test-path))

(add-to-list 'load-path atilde-root-path)
