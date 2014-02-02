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

(defvar atilde-ignored-envs
  '(("\\begin{displaymath}" . "\\end{displaymath}")
    ("\\begin{displaystyle}" . "\\end{displaystyle}"))
  "A list of ignored environments consisting of pairs with beginnings
and endings of an environment.")

(defvar atilde-highlight-missing-tildes t)

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
    (atilde-add-overlays)
    (add-hook 'after-change-functions 'atilde-handle-change nil t))
   (t
    (remove-hook 'after-change-functions 'atilde-handle-change t)
    (atilde-delete-overlays))))

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

(defun atilde-in-comment? ()
  "Check if point is in a comment."
  (nth 4 (syntax-ppss)))

(defun atilde-check-prev-word? ()
  "Check if previous word is the one from `atilde-words'."
  (looking-back (atilde-build-words-regexp) (line-beginning-position)))

(defun atilde-insert-tilde? ()
  "Check if tilde can be inserted at point."
  (and (atilde-check-prev-word?)
       (not (atilde-in-comment?))
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

(defun atilde-handle-change (beg end length)
  "Remove all overlays and add again after each buffer change."
  ;; FIXME: use params
  (atilde-delete-overlays)
  (atilde-add-overlays))

(defun atilde-get-missing-tildes-positions ()
  "Return positions of spaces where tilde should be inserted."
  (let (space-pos)
    (save-excursion
     (goto-char (point-max))
     (while (search-backward " " nil t)
       (when (atilde-insert-tilde?)
         (setq space-pos (append space-pos (list (point)))))))
    (nreverse space-pos)))

(defun atilde-add-overlays ()
  "Add overlays for missing tildes."
  (-each (atilde-get-missing-tildes-positions) 'atilde-add-overlay))

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

(defun atilde-delete-overlays ()
  "Remove all atilde overlays in the current buffer."
  (save-restriction
    (widen)
    (-each (atilde-overlays-in (point-min) (point-max)) 'delete-overlay)))

(provide 'atilde)

;;; atilde.el ends here
