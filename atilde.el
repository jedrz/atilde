;;; atilde.el --- Automatically insert ~ after some words

;; Copyright (C) 2014 Łukasz Jędrzejewski

;; Author: Łukasz Jędrzejewski <jedrzejewskiluk@gmail.com>
;; URL: https://github.com/jedrz/atilde
;; Version: dev
;; Keywords: convenience
;; Package-Requires: ((dash "1.2.0") (s "1.2.0"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Automatically insert a single tilde after or between some words.

;; To quickly start up put this file somewhere in your `load-path' and add the
;; following lines in your .emacs:

;;     (require 'atilde)
;;     (add-hook 'latex-mode-hook 'atilde-mode)

;; With `atilde-mode' enabled, pressing the space key insert a hard space
;; (tilde character) after Polish vowels or some short words.  Tildes are
;; also inserted between some words like: 2014~r.

;; This package is indented to use in LaTeX buffers since some environments
;; surrounding the cursor are ignored and tildes are never inserted
;; automatically.

;; All missing tildes can be inserted in entire buffer with
;; `atilde-query-replace' command.

;; Positions of missing tildes are marked by default in red color.

;; To add new words after or between which tildes should be inserted, add a new
;; environment as ignored one or disable highlighting positions of missing
;; tildes do:

;;     M-x customize-group atilde RET

;; Some (most) regexps were copied from:

;;     ftp://ftp.gust.org.pl/pub/GUST/contrib/GUSTPROG/porzadki.pl

;; For a more descriptive instructions check out:

;;     https://github.com/jedrz/atilde

;;; Code:

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

;; Note: every alternative is enclosed by a group not to match the previous
;; string since below regexps are concatenated with other regexps.

(defcustom atilde-after-regexps
  (list
   "[aeiouwz]"
   "[[:alpha:]]\\{2\\}"                 ; two letter words
   (concat
    "\\("
    "mgr\\|inż\\.\\|dr\\|prof\\.\\|hab\\.\\|bp\\|ks\\.\\|o+\\.\\|św\\.\\|"
    "prez\\.\\|przew\\.\\|red\\.\\|min\\.\\|gen\\.\\|płk\\|mjr\\|kpt\\.\\|"
    "hab\\|ks\\|o+\\|św\\|prez\\|przew\\|red\\|min\\|gen\\|"
    "tab\\.\\|tabl\\.\\|ry[cs]\\.\\|rozdz\\.\\|nr\\|"
    "[stz]\\.\\|ss\\.\\|vol\\.\\|art\\.)"
    "\\)"))
  "A list of regexps after which tilde can be inserted."
  :group 'atilde
  :type '(repeat regexp))

(defcustom atilde-between-regexps
  '(("[[:digit:]]+" . "[[:digit:]]+")
    ("\\([[:digit:]]\\|[XLVIM]\\)+" . "\\(r\\.\\|w\\.\\)")
    ("[[:digit:]]+" . "\\(tys\\.\\|mln\\|mld\\)")
    ("[[:digit:]]+" . "[kdcmn]?[glmVAW]") ; FIXME: case-sensitivity
    ("[[:digit:]]+" .
     "\\(tys\\.\\|mln\\|mld\\|zł\\|gr\\|ha\\|t\\|mies\\|godz\\|min\\|sek\\)"))
  "A list of regexps between which tilde should be inserted."
  :group 'atilde
  :type '(alist :key-type regexp :value-type regexp))

(defcustom atilde-ignored-envs
  '(("\\\\begin{displaymath}" . "\\\\end{displaymath}")
    ("\\\\begin{displaystyle}" . "\\\\end{displaystyle}"))
  "A list of regexps containing ignored environments.

Each cons cell consists of pairs with beginnings and endings of
an environment."
  :group 'atilde
  :type '(alist :key-type regexp :value-type regexp))

(defcustom atilde-highlight-missing-tildes t
  "Set to non-nil if missing tildes should be highlighted."
  :group 'atilde
  :type 'boolean
  :safe 'booleanp)

(defcustom atilde-mode-line-lighter " ~"
  "A mode line lighter string of `atilde-mode'."
  :group 'atilde
  :type 'string)

(defface atilde-missing-tilde
  '((t (:background "Red")))
  "Face to mark missing tildes."
  :group 'atilde-faces)

(defvar atilde-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") 'atilde-space)
    map)
  "Keymap of `atilde-mode'.")

;;;###autoload
(define-minor-mode atilde-mode
  "Toggle automatically inserting of tildes in buffer.

With a prefix argument ARG, enable `atilde-mode' if ARG is positive,
and disable it otherwise. If called from Lisp, enable `atilde-mode' mode
if ARG is omitted or nil."
  :init-value nil
  :lighter atilde-mode-line-lighter
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

(defconst atilde-tilde "~"
  "A tilde character.")

(defconst atilde-whitespace-string " \t\n"
  "A string to use in `skip-chars-*' functions.")

(defconst atilde-whitespace-regexp "[ \t\n]+"
  "Regexp matching spaces, tabulators and newlines.")

(defconst atilde-not-whitespace-regexp "[^ \t\n]"
  "Regexp that doesn't match spaces, tabulators or newlines.")

(defun atilde-in-ignored-env? ()
  "Check if point is in an ignored environment.

See `atilde-ignored-envs' for a list of ignored environments."
  (-any?
   (lambda (regexp-pair)
     (save-excursion
       (let ((point (point))
             (start (re-search-backward (car regexp-pair) nil t))
             (end (re-search-forward (cdr regexp-pair) nil t)))
         (or (and start end (< start point) (< point end))
             (and start (not end))))))
   atilde-ignored-envs))

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

(defun atilde-insert-after? ()
  "Check if previous word match a regexp from `atilde-after-regexps'.

All whitespace characters before the cursor are ignored."
  (save-excursion
    (skip-chars-backward atilde-whitespace-string)
    (-any? (lambda (regexp)
             (looking-back
              ;; To match previous word separated with whitespace characters.
              (concat "\\<" regexp)
              (line-beginning-position)))
           atilde-after-regexps)))

(defun atilde-insert-between? ()
  "Check if point is between any pair of regexps from `atilde-between-regexps'."
  (-any?
   (lambda (regexp-pair)
     (save-excursion
       (and
        (re-search-backward
         (atilde-build-between-beginning-regexp (car regexp-pair))
         ;; Take into account text up to first occurrence
         ;; of whitespace character only.
         (save-excursion
           (skip-chars-backward atilde-whitespace-string)
           (re-search-backward atilde-whitespace-regexp
                               (save-excursion
                                 (forward-line -1)
                                 (point))
                               t))
         t)
        (looking-at (atilde-build-between-regexp regexp-pair)))))
   atilde-between-regexps))

(defun atilde-build-between-regexp (regexp-pair)
  "Build regexp matching whitespace characters between REGEXP-PAIR."
  (concat (atilde-build-between-beginning-regexp (car regexp-pair))
          atilde-whitespace-regexp
          (cdr regexp-pair)))

(defun atilde-build-between-beginning-regexp (regexp)
  "Match empty string too at the beginning of REGEXP."
  (concat "\\<" regexp))

(defun atilde-insert-tilde? ()
  "Check if tilde can be inserted at point."
  (and (or (atilde-insert-after?)
           (atilde-insert-between?))
       (not (atilde-in-comment?))
       (not (atilde-in-verb?))
       (not (atilde-in-ignored-env?))))

;;;###autoload
(defun atilde-space (arg)
  "Insert tilde or space ARG times.

Tilde is inserted after or between any characters matching
regular expressions from `atilde-after-regexps' and
`atilde-between-regexps' variables.

If the point is in an ignored environment (see
`atilde-ignored-envs') then always space is inserted."
  (interactive "p")
  (let ((last-command-event-copy last-command-event))
    (save-excursion
      (when (and
             ;; Search backward for nearest whitespace chars.
             (let ((result (re-search-backward
                            (concat atilde-not-whitespace-regexp
                                    atilde-whitespace-regexp)
                            (save-excursion
                              (forward-line -1)
                              (point))
                            t)))
               ;; Skip first non-whitespace character matched by above regexp.
               (when (not (eobp))
                 (forward-char))
               result)
             (atilde-insert-tilde?)
             ;; Search forward for whitespace chars again.
             (re-search-forward atilde-whitespace-regexp nil t))
        ;; Replace them with a single tilde.
        (replace-match atilde-tilde)))
    (setq last-command-event last-command-event-copy))
  (if (atilde-insert-tilde?)
      (progn
        (setq last-command-event ?~)
        (self-insert-command 1))
    (self-insert-command arg)))

(defun atilde-handle-change (beg end len)
  "Add or remove overlays for given buffer change (BEG, END and LEN)."
  (if (= len 0)
      (atilde-add-overlays beg end)     ; text added
    (atilde-delete-overlays beg end)))  ; text removed

(defun atilde-get-missing-tildes-positions (&optional beg end)
  "Return positions of whitespace characters where tilde should be inserted.

Returned positions are cons cells specifying regions which should
be replaced with a single tilde character.

If BEG and END are not nil then spaces are searched only between
BEG and END."
  (setq beg (or beg (point-min))
        end (or end (point-max)))
  (let (positions)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward atilde-whitespace-regexp end t)
        (when (save-match-data
                (atilde-insert-tilde?))
          (let ((region (cons
                         ;; Subtract the length of found whitespace characters
                         ;; since the point is located at the end of them.
                         (- (point) (length (match-string 0)))
                         (point))))
            (setq positions (cons region positions))))))
    (nreverse positions)))

(defun atilde-get-missing-tildes-positions-as-markers (&optional beg end)
  "Return marker pairs where whitespace characters should be replaced.

If BEG and END are not nil then spaces are searched only between
BEG and END."
  (let ((positions (atilde-get-missing-tildes-positions beg end)))
    (-map (lambda (position)
            (let ((m-beg (make-marker))
                  (m-end (make-marker)))
              (set-marker m-beg (car position))
              (set-marker m-end (cdr position))
              (cons m-beg m-end)))
          positions)))

(defun atilde-discard-markers (marker-pairs)
  "Set each marker in MARKER-PAIRS to nil.

Allow not needed markers to be garbage collected."
  (-each marker-pairs 'atilde-discard-marker))

(defun atilde-discard-marker (marker-pair)
  "Set markers in MARKER-PAIR to nil."
  (set-marker (car marker-pair) nil)
  (set-marker (cdr marker-pair) nil))

(defun atilde-add-overlays (&optional beg end)
  "Add overlays for missing tildes in the current buffer.

If BEG and END are not nil then overlays are added only between BEG and END."
  (-each (atilde-get-missing-tildes-positions beg end) 'atilde-add-overlay))

(defun atilde-add-overlay (position)
  "Add overlay with warning face for POSITION.

POSITION should be a cons cell specifying the beginning and the
end of a new overlay."
  (let ((overlay (make-overlay (car position) (cdr position))))
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

;;;###autoload
(defun atilde-query-replace (&optional force)
  "Insert all missing tildes into the current buffer.

With a prefix argument (FORCE) replace all proper whitespace
characters without asking for permission."
  (interactive "P")
  (if force
      (let ((marker-pairs (atilde-get-missing-tildes-positions-as-markers)))
        (save-excursion
          (-each marker-pairs 'atilde-replace-whitespace))
        (atilde-discard-markers marker-pairs))
    (error "Query replacing not implemented yet")))

(defun atilde-replace-whitespace (position)
  "Replace text at given POSITION with single tilde."
  (let ((beg (car position))
        (end (cdr position)))
    (delete-region beg end)
    (goto-char beg)
    (insert atilde-tilde)))

(provide 'atilde)

;;; atilde.el ends here
