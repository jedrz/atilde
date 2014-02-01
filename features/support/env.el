(require 'f)

(defvar atilde-support-path
  (f-dirname load-file-name))

(defvar atilde-features-path
  (f-parent atilde-support-path))

(defvar atilde-root-path
  (f-parent atilde-features-path))

(add-to-list 'load-path atilde-root-path)

(require 'atilde)
(require 'espuds)
(require 'ert)
