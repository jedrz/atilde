;;; atilde.el --- auto tilde

;; Version: 0.0.0

(require 'dash)
(require 's)

(defgroup atilde nil
  "Automatically insert tildes in tex buffer."
  :prefix "atilde-"
  :group 'tools) ;; FIXME: change group

(defgroup atilde-faces nil
  "Faces for atilde."
  :prefix "atilde-"
  :group 'atilde)

(defvar atilde-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") 'atilde-space)
    map)
  "Keymap of `atilde-mode'.")

(defvar atilde-words
  '("a" "e" "i" "o" "u" "w" "z"
    "od" "nad" "pod")
  "A list of words after which tilde can be inserted.")

(defvar atilde-between-regexps
  '(("\\<[[:digit:]]+" . ".\\."))
  "A list of regexps between which tilde should be inserted.")

(defvar atilde-ignored-envs
  '(("\\begin{displaymath}" . "\\end{displaymath}")
    ("\\begin{displaystyle}" . "\\end{displaystyle}"))
  "A list of ignored environments consisting of pairs with beginnings
and endings of an environment.")

(defvar atilde-highlight-missing-tildes t
  "Non nil if missing tildes should be highlighted.")

(defface atilde-missing-tilde
  '((t (:background "Red")))
  "Face to mark missing tildes.")

(define-minor-mode atilde-mode
  "Toggle automatically inserting of tildes in buffer.

With a prefix argument ARG, enable `atilde-mode' if ARG is positive,
and disable it otherwise. If called from Lisp, enable `atilde-mode' mode
if ARG is omitted or nil."
  :init-value nil
  :lighter " ~"
  :keymap atilde-mode-map
  :group 'atilde
  :require 'atilde
  (cond
   (atilde-mode
    (when atilde-highlight-missing-tildes
      ;; Add overlays for entire buffer when turning on.
      (save-restriction
        (widen)
        (atilde-add-overlays))
      (add-hook 'after-change-functions 'atilde-handle-change nil t)))
   (t
    (remove-hook 'after-change-functions 'atilde-handle-change t)
    ;; Remove overlays for entire buffer when turning off.
    (save-restriction
      (widen)
      (atilde-delete-overlays)))))

(defun atilde-build-words-regexp ()
  "Build regexp that matches any from `atilde-words'."
  (->> atilde-words
    (s-join "\\|")
    (format "\\<\\(%s\\)")))

(defun atilde-build-between-regexp ()
  "Buld regexp that matches any pair from `atilde-between-regexps'.

Between given regexps whitespace characters are also being matched."
  (->> atilde-between-regexps
    (--map (concat (car it) "\\s-+" (cdr it)))
    (s-join "\\|")
    (format "\\(%s\\)")))

(defun atilde-build-between-before-regexp ()
  "Buld regexp that matches any first elem from `atilde-between-regexps'."
  (->> atilde-between-regexps
    (-map 'car)
    (s-join "\\|")
    (format "\\(%s\\)")))

(defun atilde-build-between-after-regexp ()
  "Buld regexp that matches any second elem from `atilde-between-regexps'."
  (->> atilde-between-regexps
    (-map 'cdr)
    (s-join "\\|")
    (format "\\(%s\\)")))

(defun atilde-build-env-regexp ()
  "Build regexp that matches any beginning of an ignored environment."
  (->> atilde-ignored-envs
    (-map 'car)
    (-map 'regexp-quote)
    (s-join "\\|")
    (format "\\(%s\\)")))

(defun atilde-find-nearest-beg-env ()
  "Find nearest beginning of ignored environment.

Returns string with ending environment that ends found environment."
  (when (re-search-backward (atilde-build-env-regexp) nil t)
    (let ((env (match-string 1)))
      (when env
        (cdr (assoc env atilde-ignored-envs))))))

(defun atilde-find-nearest-end-env (env)
  "Find nearest ending ENV."
  (search-forward env nil t))

(defun atilde-in-ignored-env? ()
  "Check if point is in an ignored environment.

See `atilde-ignored-envs' for a list of ignored environments."
  (save-excursion
    (let ((point (point)))
      (-when-let (end-env (atilde-find-nearest-beg-env))
        (let ((start (point))
              (end (atilde-find-nearest-end-env end-env)))
          (or (and end (< start point) (< point end))
              (not end)))))))

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

(defun atilde-in-comment? ()
  "Check if point is in a comment."
  (nth 4 (syntax-ppss)))

(defun atilde-check-prev-word? ()
  "Check if previous word is the one from `atilde-words'."
  (looking-back (atilde-build-words-regexp) (line-beginning-position)))

(defun atilde-insert-between? ()
  "Check if point is between any pair of regexps from `atilde-between-regexps'."
  (save-excursion
    (and (re-search-backward (atilde-build-between-before-regexp)
                             (save-excursion
                               (forward-line -1)
                               (point))
                             t)
         (looking-at (atilde-build-between-regexp)))))

(defun atilde-insert-tilde? ()
  "Check if tilde can be inserted at point."
  (and (or (atilde-check-prev-word?)
           (atilde-insert-between?))
       (not (atilde-in-comment?))
       (not (atilde-in-verb?))
       (not (atilde-in-ignored-env?))))

(defun atilde-space (arg)
  "Insert tilde or space ARG times.

Tilde is inserted after `atilde-words' and also between any
characters matching regular expression from
`atilde-between-regexp' variable.

If the point is in an ignored environment (see
`atilde-ignored-envs') then always space is inserted."
  (interactive "p")
  (let ((last-command-event-copy last-command-event))
    (save-excursion
      (when (and
             ;; Search backward for nearest whitespace chars.
             (re-search-backward "\\S-\\s-+"
                                 (save-excursion
                                   (forward-line -1)
                                   (point))
                                 t)
             (atilde-insert-tilde?)
             ;; Search forward for whitespace chars again.
             (re-search-forward "\\s-+" nil t))
        ;; Replace them with a single tilde.
        (replace-match "~")))
    (setq last-command-event last-command-event-copy))
  (if (atilde-insert-tilde?)
      (progn
        (setq last-command-event ?~)
        (self-insert-command 1))
    (self-insert-command arg)))

(defun atilde-handle-change (beg end len)
  "Add or remove overlays for given buffer change."
  (if (= len 0)
      (atilde-add-overlays beg end)     ; text added
    (atilde-delete-overlays beg end)))  ; text removed

(defun atilde-get-missing-tildes-positions (&optional beg end)
  "Return positions of spaces where tilde should be inserted.

If BEG and END are not nil then spaces are searched only between
BEG and END."
  (setq beg (or beg (point-min))
        end (or end (point-max)))
  (let (space-pos)
    (save-excursion
     (goto-char end)
     (while (search-backward " " beg t)
       (when (atilde-insert-tilde?)
         (setq space-pos (cons (point) space-pos)))))
    space-pos))

(defun atilde-add-overlays (&optional beg end)
  "Add overlays for missing tildes in the current buffer.

If BEG and END are not nil then overlays are added only between BEG and END."
  (-each (atilde-get-missing-tildes-positions beg end) 'atilde-add-overlay))

(defun atilde-add-overlay (space-pos)
  "Add overlay with warning face for SPACE-POS."
  (let ((overlay (make-overlay space-pos (1+ space-pos))))
    (overlay-put overlay 'atilde-overlay t)
    (overlay-put overlay 'face 'atilde-missing-tilde)))

(defun atilde-filter-overlays (overlays)
  "Return all atilde overlays from OVERLAYS."
  (--filter (overlay-get it 'atilde-overlay) overlays))

(defun atilde-overlays-in (beg end)
  "Return all atilde overlays between BEG and END."
  (atilde-filter-overlays (overlays-in beg end)))

(defun atilde-delete-overlays (&optional beg end)
  "Remove all atilde overlays in the current buffer.

If BEG and END are not nil then overlays are deleted only between BEG and END."
  (setq beg (or beg (point-min))
        end (or end (point-max)))
  (-each (atilde-overlays-in beg end) 'delete-overlay))

(provide 'atilde)

;;; atilde.el ends here
