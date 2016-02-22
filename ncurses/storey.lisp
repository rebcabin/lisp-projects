;;; A storey has a matrix of /tiles/.

(defconstant +storey-width+  256)
(defconstant +storey-height+ 256)

(defun make-storey-matrix
    (&key
       (width +storey-width+)
       (height +storey-height+))
  (make-array `(,height ,width)
              :initial-element nil))

(defclass storey ()
  ((matrix :accessor storey-matrix :initform nil)
   (width  :accessor storey-width  :type integer :initform 0 :initarg :width)
   (height :accessor storey-height :type integer :initform 0 :initarg :height)
   (bounding-box :accessor storey-bounding-box
                 :type     box
                 :initform nil)
   ))

(defmethod initialize-instance
    :around
    ((s storey)
     &key (width +storey-width+) (height +storey-height+))
  (call-next-method s :width width :height height)
  (setf (slot-value s 'matrix)
        (make-storey-matrix :width width :height height))
  (setf (slot-value s 'bounding-box)
        (make-instance 'box :left 0 :top 0 :width width :height height)))
