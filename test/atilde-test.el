(require 'ert)
(require 'dash)

;; FIXME: replace the way of loading package
(load (expand-file-name "atilde.el"))

(defmacro atilde-test-with-text (text &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (latex-mode)
     (atilde-mode)
     ,@body))

(ert-deftest atilde-test/get-missing-tildes-positions ()
  "Test reported spaces."
  (atilde-test-with-text "foo a z bar od word"
    (let ((result-positions (atilde-get-missing-tildes-positions))
          (real-positions '(6 8 15)))
      (should result-positions)
      (--each (-zip result-positions real-positions)
        (let ((result (car it))
              (real (cdr it)))
          (should (= result real)))))))

(ert-deftest atilde-test/add-overlay ()
  "Test `atilde-add-overlay' for adding an overlay."
  (with-temp-buffer
    (insert "foo")
    (atilde-add-overlay 1)
    (let* ((overlays (overlays-in (point-min) (point-max)))
           (overlay (car overlays))
           (properties (overlay-properties overlay)))
      (should (= (length overlays) 1))
      (should (= (length properties) 4)) ; because symbols and values are
                                         ; counted
      (should (eq (overlay-get overlay 'atilde-overlay) t))
      (should (eq (overlay-get overlay 'face) 'atilde-missing-tilde))
      (should (= (overlay-start overlay) 1))
      (should (= (overlay-end overlay) 2)))))

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
          (should (= beg pos))
          (should (= end (1+ pos))))))))

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
