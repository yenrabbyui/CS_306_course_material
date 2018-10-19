(defun compare-min (p1 p2)
    (if (< (car p1) (car p2))
        (car p1)
      (car p2)))

  (defun compare-max (p1 p2)
    (if (> (cdr p1) (cdr p2))
        (cdr p1)
      (cdr p2)))

  (defun r-min-max (array i j)
    (if (= i (1- j)) ;; base case
        (let ((e1 (aref array i))
              (e2 (aref array j)))
          (if (< e1 e2)
              (cons e1 e2) (cons e2 e1)))
      ;; else recurse
      (let* ((mid (/ (+ i j) 2))
             (mm1 (r-min-max array i mid))
             (mm2 (r-min-max array (1+ mid) j)))
        (cons (compare-min mm1 mm2) (compare-max mm1 mm2)))))
