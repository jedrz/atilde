;;; atilde.el --- auto tilde

;; Version: 0.0.0

(require 'dash)

(defvar atilde-words '("a" "e" "i" "o" "u" "w" "z"
                       "od" "nad" "pod"))

(defun atilde-build-regexp (word)
  (format "\\(^%s\\| %s\\)" word word))

(defun atilde-insert-tilde? ()
  (let ((start (line-beginning-position)))
    (--any? (looking-back (atilde-build-regexp it) start)
            atilde-words)))

(defun atilde-space ()
  (interactive)
  (insert (if (atilde-insert-tilde?) "~" " ")))

(provide 'atilde)

;;; atilde.el ends here
