(require 'ert)
(require 'dash)

;; FIXME: replace the way of loading package
(load (expand-file-name "atilde.el"))

(defmacro atilde-test-with-temp-buffer (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (latex-mode)
     (atilde-mode)
     ,@body))

(ert-deftest atilde-test/get-missing-tildes-positions ()
  (atilde-test-with-temp-buffer
   (insert "foo a z bar od word")
   (let ((result-positions (atilde-get-missing-tildes-positions))
         (real-positions '(6 8 15)))
     (should result-positions)
     (--each (-zip result-positions real-positions)
       (let ((result (car it))
             (real (cdr it)))
         (should (= result real)))))))

(ert-deftest atilde-test/overlays-positions ()
  (with-temp-buffer
   (insert "foo a z bar od word")
   ;; FIXME remove turning on latex- and atilde-mode here.
   (latex-mode)
   (atilde-mode)
   (let ((overlays (sort (atilde-overlays-in (point-min) (point-max))
                         (lambda (o1 o2)
                           (< (overlay-start o1) (overlay-start o2)))))
         (positions '(6 8 15)))
     (should overlays)
     (--each (-zip overlays positions)
       (let* ((overlay (car it))
              (beg (overlay-start overlay))
              (end (overlay-end overlay))
              (pos (cdr it)))
         (should (= beg pos))
         (should (= end (1+ pos))))))))

(ert-deftest atilde-test/delete-overlays-after-turning-off ()
  (atilde-test-with-temp-buffer
   (insert "a foo")
   (atilde-mode -1)
   (should-not (atilde-overlays-in (point-min) (point-max)))))

(ert-deftest atilde-test/add-overlays-after-turning-on ()
  (with-temp-buffer
    (latex-mode)
    (insert "a foo")
    (atilde-mode)
    (let* ((overlay (car (atilde-overlays-in (point-min) (point-max))))
           (beg (overlay-start overlay)))
      (should (= beg 2)))))
