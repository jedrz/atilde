(require 'atilde)
(require 'ert)
(require 'dash)

(defmacro atilde-test-with-text (text &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (latex-mode)
     (atilde-mode)
     ,@body))

(ert-deftest atilde-test/get-missing-tildes-positions ()
  "Test reported whitespace characters."
  (atilde-test-with-text "foo a  z bar od word 2014\n r."
    (let ((result-positions (atilde-get-missing-tildes-positions))
          (real-positions '((6 . 8) (9 . 10) (16 . 17) (26 . 28))))
      (should result-positions)
      (--each (-zip result-positions real-positions)
        (let ((result (car it))
              (real (cdr it)))
          (should (equal result real)))))))

(ert-deftest atilde-test/get-missing-tildes-positions-as-markers ()
  "Test whether markers match corresponding positions."
  (atilde-test-with-text "foo a z bar od word"
    (let ((marker-pairs (atilde-get-missing-tildes-positions-as-markers))
          (positions (atilde-get-missing-tildes-positions)))
      (should marker-pairs)
      (--each (-zip marker-pairs positions)
        (let ((marker-pair (car it))
              (position (cdr it)))
          (should (= (car marker-pair) (car position)))
          (should (= (cdr marker-pair) (cdr position))))))))

(ert-deftest atilde-test/discard-markers ()
  "Test whether markers are set to nil."
  (atilde-test-with-text "foo a z bar od word"
    (let ((marker-pairs (atilde-get-missing-tildes-positions-as-markers)))
      (atilde-discard-markers marker-pairs)
      (--each marker-pairs
        (should-not (marker-position (car it)))
        (should-not (marker-position (cdr it)))))))

(ert-deftest atilde-test/add-overlay ()
  "Test `atilde-add-overlay' for adding an overlay."
  (with-temp-buffer
    (insert "foo")
    (atilde-add-overlay '(1 . 3))
    (let* ((overlays (overlays-in (point-min) (point-max)))
           (overlay (car overlays))
           (properties (overlay-properties overlay)))
      (should (= (length overlays) 1))
      (should (= (length properties) 4)) ; because symbols and values are
                                         ; counted
      (should (eq (overlay-get overlay 'atilde-overlay) t))
      (should (eq (overlay-get overlay 'face) 'atilde-missing-tilde))
      (should (= (overlay-start overlay) 1))
      (should (= (overlay-end overlay) 3)))))

(ert-deftest atilde-test/overlays-positions ()
  "Test if overlays are properly placed."
  (atilde-test-with-text "foo a z bar od word"
    (let ((overlays (sort (atilde-overlays-in (point-min) (point-max))
                          (lambda (o1 o2)
                            (< (overlay-start o1) (overlay-start o2)))))
          (positions (atilde-get-missing-tildes-positions)))
      (should overlays)
      (--each (-zip overlays positions)
        (let* ((overlay (car it))
               (beg (overlay-start overlay))
               (end (overlay-end overlay))
               (pos (cdr it)))
          (should (= beg (car pos)))
          (should (= end (cdr pos))))))))

(ert-deftest atilde-test/overlays-in ()
  "Test overlays returned by `atilde-overlays-in'."
  (with-temp-buffer
    (insert "foo")
    (atilde-add-overlay '(2 . 3))
    (let ((o1 (make-overlay 1 2))
          (o2 (make-overlay 2 3)))
      (overlay-put o1 'o1 t)
      (overlay-put o2 'o2 t))
    (let* ((atilde-overlays (atilde-overlays-in (point-min) (point-max)))
           (overlay (car atilde-overlays)))
      (should (= (length atilde-overlays) 1))
      (should (eq (overlay-get overlay 'atilde-overlay) t)))))

(ert-deftest atilde-test/delete-overlays-after-turning-off ()
  "Test if overlays are removed after turning off atilde mode."
  (atilde-test-with-text "a foo"
    (atilde-mode -1)
    (should-not (atilde-overlays-in (point-min) (point-max)))))

(ert-deftest atilde-test/add-overlays-after-turning-on ()
  "Test if overlays are removed after turning on atilde mode."
  (atilde-test-with-text "a foo"
    (let* ((overlay (car (atilde-overlays-in (point-min) (point-max))))
           (beg (overlay-start overlay)))
      (should (= beg 2)))))

(ert-deftest atilde-test/no-overlays ()
  "Test if there are no overlays when `atilde-highlight-missing-tildes' is nil."
  (let ((atilde-highlight-missing-tildes nil))
    (atilde-test-with-text "foo a bar"
      (should-not (atilde-overlays-in (point-min) (point-max))))))

(ert-deftest atilde-test/buffer-narrowing ()
  "Test overlays after narrowing and widening a buffer."
  (with-temp-buffer
    (insert "a foo\ne foo\nu foo\n")
    (goto-char (point-min))
    (forward-line)
    ;; After narrowing I should see 1 overlay.
    (narrow-to-region (line-beginning-position) (line-end-position))
    (latex-mode)
    (atilde-mode)
    (should (= (length (atilde-overlays-in (point-min) (point-max))) 1))
    ;; After widening I should see 3 overlays.
    (widen)
    (should (= (length (atilde-overlays-in (point-min) (point-max))) 3))
    ;; After narrowing and turning off the mode there should be no overlays.
    (goto-char (point-min))
    (forward-line)
    (narrow-to-region (line-beginning-position) (line-end-position))
    (atilde-mode -1)
    (widen)
    (should (= (length (atilde-overlays-in (point-min) (point-max))) 0))))
