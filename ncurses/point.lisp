(defclass point ()
  ((x :accessor point-x
      :initform 0
      :initarg  :x)
   (y :accessor point-y
      :initform 0
      :initarg  :y)))

(defun point (x y)
  (make-instance 'point :x x :y y))

(defun zero-point ()
  (make-instance 'point :x 0 :y 0))

(defmethod decr-x ((p point))
  (point (1- (point-x p)) (point-y p)))

(defmethod decr-y ((p point))
  (point (point-x p) (1- (point-y p))))

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
