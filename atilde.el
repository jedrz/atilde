;;; atilde.el --- auto tilde

;; Version: 0.0.0

(require 'dash)

(defvar atilde-words '("a" "e" "i" "o" "u" "w" "z"
                       "od" "nad" "pod"))

(defvar atilde-regexp (format
                       "\\(%s\\)"
                       (--reduce-from (concat acc
                                              "\\|^" it
                                              "\\| " it)
                                      (let ((f (car atilde-words)))
                                        (concat "^" f "\\| " f))
                                      (cdr atilde-words))))

(defun atilde-insert-tilde? ()
  (looking-back atilde-regexp (line-beginning-position)))

(defun atilde-space ()
  (interactive)
  (insert (if (atilde-insert-tilde?) "~" " ")))

(provide 'atilde)

;;; atilde.el ends here
