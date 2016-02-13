(defstruct point
  (x 0 :type integer)
  (y 0 :type integer))

(defun zero-point ()
  (make-point))

(defmethod decr-x ((p point))
  (make-point :x (1- (point-x p)) :y (point-y p)))

(defmethod decr-y ((p point))
  (make-point :x (point-x p) :y (1- (point-y p))))

(defmethod displace-left ((p point))
  (decf (point-x p))
  t)

(defmethod displace-right ((p point))
  (incf (point-x p))
  t)

(defmethod displace-up ((p point))
  (decf (point-y p))
  t)

(defmethod displace-down ((p point))
  (incf (point-y p))
  t)
