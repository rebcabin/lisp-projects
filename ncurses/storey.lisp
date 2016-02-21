;;; A storey has a matrix of /tiles/.

(defconstant +storey-width+  256)
(defconstant +storey-height+ 256)

(defun make-storey-matrix (&key
                             (width +storey-width+)
                             (height +storey-height+))
  (make-array `(,height ,width)
              :initial-element nil))

(defstruct storey
  (matrix))

