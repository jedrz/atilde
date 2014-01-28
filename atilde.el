;;; atilde.el --- auto tilde

;; Version: 0.0.0

(require 'dash)
(require 's)

(defvar atilde-words
  '("a" "e" "i" "o" "u" "w" "z"
    "od" "nad" "pod")
  "A list of words after which tilde can be inserted.")

(defvar atilde-ignored-envs
  '(("\\begin{displaymath}" . "\\end{displaymath}")
    ("\\begin{displaystyle}" . "\\end{displaystyle}"))
  "A list of ignored environments consisting of pairs with beginnings
and endings of an environment.")

(defun atilde-build-words-regexp ()
  "Build regexp that matches any from `atilde-words'."
  (->> atilde-words
    (s-join "\\|")
    (format "\\<\\(%s\\)")))

(defun atilde-build-env-regexp ()
  "Build regexp that matches any beginning of an ignored environment."
  (->> atilde-ignored-envs
    (-map 'car)
    (-map 'regexp-quote)
    (s-join "\\|")
    (format "\\(%s\\)")))

(defun atilde-find-nearest-beg-env ()
  "Find nearest beginning of ignored environment.

Returns a cons cell with position and string with ending environment
that ends found environment.
If nothing has been found returns nil."
  (save-excursion
    (when (re-search-backward (atilde-build-env-regexp) nil t)
      (let ((env (match-string 1)))
        (when env
          (cons
           (point)
           (cdr (assoc env atilde-ignored-envs))))))))

(defun atilde-find-nearest-end-env (env)
  "Find nearest ending ENV."
  (save-excursion
    (search-forward env nil t)))

(defun atilde-in-ignored-env? ()
  "Check if point is in an ignored environment.

See `atilde-ignored-envs' for a list of ignored environments."
  (-when-let (start-env (atilde-find-nearest-beg-env))
    (let* ((start (car start-env))
           (env (cdr start-env))
           (end (atilde-find-nearest-end-env env)))
      (or (and end (< start (point)) (< (point) end))
          (and start (not end))))))

(defun atilde-in-verb? ()
  "Check if point is in verb.

The point is considered to be in verb environment if is:
- between the verb beginning and the ending delimiter,
- after the verb beginning and there is no delimiter."
  (save-excursion
    (let* ((pattern "\\<verb\\(.\\)")
           (point (point))
           (start (re-search-backward pattern (line-beginning-position) t))
           (verb-delim (match-string 1))
           (end (when verb-delim
                  (goto-char (+ (point) (length "verb.")))
                  (re-search-forward verb-delim (line-end-position) t))))
      (or (and start end (< start point) (< point end))
          (and start (not end))))))

(defun atilde-check-prev-word? ()
  "Check if previous word is the one from `atilde-words'."
  (looking-back (atilde-build-words-regexp) (line-beginning-position)))

(defun atilde-insert-tilde? ()
  "Check if tilde can be inserted at point."
  (and (atilde-check-prev-word?)
       (not (atilde-in-verb?))
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
