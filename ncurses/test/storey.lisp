(load "tile.lisp")
(load "storey.lisp")

(quickcheck
 (let ((m (make-storey-matrix :width 10 :height 6)))
   (setf (aref m 2 1)
         (make-instance 'tile))
   (is equal (type-of (aref m 2 1)) 'tile))

 (let* ((ss (make-instance 'storey :width 11 :height 7))
        (t0 (type-of (storey-matrix ss)))
        (cl (first t0))
        (et (second t0))
        (ds (third t0)))
   (describe ss)
   (is equal cl 'simple-array)
   (is equal et t)
   (is equal ds '(7 11)))
)
