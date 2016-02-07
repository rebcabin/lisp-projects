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

