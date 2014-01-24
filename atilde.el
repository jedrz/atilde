;;; atilde.el --- auto tilde

;; Version: 0.0.0

(require 'dash)
(require 'expand-region)

(defvar atilde-words
  '("a" "e" "i" "o" "u" "w" "z"
    "od" "nad" "pod")
  "A list of words after which tilde can be inserted.")

(defvar atilde-regexp
  (format
   "\\<\\(%s\\)"
   (s-join "\\|" atilde-words))
  "A regexp detecting `atilde-words'.")

(defvar atilde-ignored-envs
  '("\\begin{displaymath}"
    "\\begin{displaystyle}")
  "A list of ignored environments.")

(defvar atilde-ignore-regexp
  (format
   "\\(%s\\)"
   (s-join "\\|"
           (--map
            (s-replace "\\" "\\\\" it)
            atilde-ignored-envs)))
  "A regexp to check if we are looking at the beginning of an environment.")

(defun atilde-in-ignored-env? ()
  "Check if point is in an ignored environment.

See `atilde-ignored-envs' for a list of ignored environments."
  (save-excursion
    (let ((expand-region-fast-keys-enabled nil)
          (stop nil))
      (while (and (not stop)
                  (> (point) (point-min)))
        (er/expand-region 1)
        (when (looking-at atilde-ignore-regexp)
          (setq stop t)))
      stop)))

(defun atilde-insert-tilde-after-word? ()
  "Check if tilde can be inserted at point."
  (looking-back atilde-regexp (line-beginning-position)))

(defun atilde-insert-tilde? ()
  "Check if tilde can be inserted at point."
  (and (atilde-insert-tilde-after-word?)
       (not (atilde-in-ignored-env?))))

(defun atilde-space ()
  "Insert tilde or space.

Tilde is inserted after `atilde-words'. If the point is in an
ignored environment (see `atilde-ignored-envs') then always space
is inserted."
  (interactive)
  (when (atilde-insert-tilde?)
    (setq last-command-event ?~))
  (call-interactively 'self-insert-command))

(provide 'atilde)

;;; atilde.el ends here
