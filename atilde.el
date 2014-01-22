;;; atilde.el --- auto tilde

;; Version: 0.0.0

(require 'dash)

(defvar atilde-words '("a" "e" "i" "o" "u" "w" "z"
                       "A" "E" "I" "O" "U" "W" "Z"
                       "od" "nad" "pod"))

(defun atilde-insert-tilde? ()
  (let ((before-space-pos (max (line-beginning-position) (point-min))))
    (save-excursion
      (--any? (search-backward it before-space-pos t)
              atilde-words))))

(defun atilde-space ()
  (interactive)
  (insert (if (atilde-insert-tilde?) "~" " ")))

(provide 'atilde)

;;; atilde.el ends here
