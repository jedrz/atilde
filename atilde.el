;;; atilde.el --- auto tilde

;; Version: 0.0.0

(require 'dash)
(require 'expand-region)

(defvar atilde-words '("a" "e" "i" "o" "u" "w" "z"
                       "od" "nad" "pod"))

(defvar atilde-regexp (format
                       "\\<\\(%s\\)"
                       (s-join "\\|" atilde-words)))

(defvar atilde-ignored-envs '("\\begin{displaymath}"
                              "\\begin{displaystyle}"))

(defvar atilde-ignore-regexp (format
                              "\\(%s\\)"
                              (s-join "\\|"
                                      (--map
                                       (s-replace "\\" "\\\\" it)
                                       atilde-ignored-envs))))

(defun atilde-in-ignored-env? ()
  (save-excursion
    (let ((expand-region-fast-keys-enabled nil)
          (stop nil))
      (while (and (not stop)
                  (> (point) (point-min)))
        (er/expand-region 1)
        (when (looking-at atilde-ignore-regexp)
          (setq stop t)))
      stop)))

(defun atilde-insert-tilde? ()
  (looking-back atilde-regexp (line-beginning-position)))

(defun atilde-space ()
  (interactive)
  (when (and (atilde-insert-tilde?)
             (not (atilde-in-ignored-env?)))
    (setq last-command-event ?~))
  (call-interactively 'self-insert-command))

(provide 'atilde)

;;; atilde.el ends here
