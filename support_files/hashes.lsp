BEGIN_SRC emacs-lisp
  (require 'cl)

  (defun hash-function-1 (key m)
    (mod (loop for c across key sum c) m))
END_SRC

BEGIN_SRC emacs-lisp
  (require 'cl)

  (defun hash-function-2 (key m)
    (let ((h 0))
      (loop for c across key do (setq h (+ (* h 31) c)))
      (mod h m)))
END_SRC