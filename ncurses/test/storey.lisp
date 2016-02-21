(load "tile.lisp")
(load "storey.lisp")

(quickcheck
 (is= 4 4)
 (let ((m (make-storey :width 10 :height 6)))
   (setf (aref m 2 1)
         (make-instance 'tile))
   (is equal (type-of (aref m 2 1) 'tile)))
)
