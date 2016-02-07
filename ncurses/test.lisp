(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-quickcheck)
(shadow 'cl-quickcheck:report '#:cl-user)
(use-package :cl-quickcheck)

(setf *print-length* 14)

(load "point.lisp")

(quickcheck

  (let ((z (zero-point)))
    (is= 0 (point-x z))
    (is= 0 (point-y z)))

  (for-all ((x (lambda () (funcall an-integer)))
            (y (lambda () (funcall an-integer))))
    (let ((a-point (make-instance 'point :x x :y y)))
      (is= x (point-x a-point))
      (is= y (point-y a-point))))
  )

