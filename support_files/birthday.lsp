BEGIN_SRC emacs-lisp
  (require 'cl)

  (defun birthday-problem (&optional max-num)
    (if (null max-num) (setq max-num 10000))
    (let ((num-days-in-year 365)
          (grand-sum 0)
          (n 0)
          A counter)
      (loop repeat max-num
            do (setq A (make-vector num-days-in-year 0)
                     counter 0)
            (loop until (= 2 (elt A n))
                  do (setq n (random num-days-in-year))
                  (incf (elt A n))
                  (incf counter))
            (incf grand-sum counter))
      (ceiling (/ grand-sum (float max-num)))))
END_SRC