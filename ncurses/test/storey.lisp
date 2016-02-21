(load "tile.lisp")
(load "storey.lisp")

(quickcheck
 (is= 4 4)
 (let ((m (make-storey-matrix :width 10 :height 6)))
   (setf (aref m 2 1)
         (make-instance 'tile))
   (is equal (type-of (aref m 2 1)) 'tile))
 (let* ((s (make-storey :matrix
                        (make-storey-matrix :width 10 :height 6)))
        (t0 (type-of (storey-matrix s)))
        (cl (first t0))
        (et (second t0))
        (ds (third t0)))
   (is equal cl 'simple-array)
   (is equal et t)
   (is equal ds '(6 10)))
 )
