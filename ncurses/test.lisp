(setf *random-state* (make-random-state t))

(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-quickcheck)
(shadow 'cl-quickcheck:report '#:cl-user)
(use-package :cl-quickcheck)

;;; The reports of random state by quickcheck are not useful. Check manually
;;; that the random state is actually random by doing something like (funcall
;;; an-integer) after pasting the lines above into a fresh sbcl session.
;;;
;;; If you want the same random state every time you run the tests, please
;;; just comment out the first line above.

(setf *print-length* 6)

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

  (for-all ((x (lambda () (funcall an-integer)))
            (y (lambda () (funcall an-integer))))
    (let* ((a-point (make-instance 'point :x x :y y))
           (b-point (decr-x a-point)))
      (is= x (point-x a-point))
      (is= y (point-y a-point))
      (is= (1- x) (point-x b-point))
      (is= y      (point-y b-point))))

  (for-all ((x (lambda () (funcall an-integer)))
            (y (lambda () (funcall an-integer))))
    (let* ((a-point (make-instance 'point :x x :y y))
           (b-point (decr-y a-point)))
      (is= x (point-x a-point))
      (is= y (point-y a-point))
      (is= x      (point-x b-point))
      (is= (1- y) (point-y b-point))))

  (for-all ((x (lambda () (funcall an-integer)))
            (y (lambda () (funcall an-integer))))
    (let* ((a-point (make-instance 'point :x x :y y)))
      (displace-left a-point)
      (is= (1- x) (point-x a-point))
      (is= y      (point-y a-point))))

  (for-all ((x (lambda () (funcall an-integer)))
            (y (lambda () (funcall an-integer))))
    (let* ((a-point (make-instance 'point :x x :y y)))
      (displace-right a-point)
      (is= (1+ x) (point-x a-point))
      (is= y      (point-y a-point))))

  (for-all ((x (lambda () (funcall an-integer)))
            (y (lambda () (funcall an-integer))))
    (let* ((a-point (make-instance 'point :x x :y y)))
      (displace-up a-point)
      (is= x      (point-x a-point))
      (is= (1- y) (point-y a-point))))

  (for-all ((x (lambda () (funcall an-integer)))
            (y (lambda () (funcall an-integer))))
    (let* ((a-point (make-instance 'point :x x :y y)))
      (displace-down a-point)
      (is= x      (point-x a-point))
      (is= (1+ y) (point-y a-point))))

  )

