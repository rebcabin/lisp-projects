(load "point.lisp")
(load "box.lisp")
(load "world.lisp")
(load "storey.lisp")
(load "room.lisp")

(defun a-positive-box ()
  (make-instance 'box
                 :left   (funcall an-integer)
                 :top    (funcall an-integer)
                 :width  (a-positive-index)
                 :height (a-positive-index)))

(defun a-positive-storey ()
  (make-instance 'storey
                 :width  (a-positive-index)
                 :height (a-positive-index)))

(quickcheck

 (for-all ((b #'a-positive-box)
           (s #'a-positive-storey))
          (let ((r (create-m-room b s)))
            (test (boxes-equal b (m-room-its-box r))))
          )
 (let* ((s (a-positive-storey))
        (bb (storey-bounding-box s))
        ;; Is it ok for this room to have a non-positive bounding box?
        (rb (make-instance 'box
                           :left 3
                           :top 2
                           :width (- (box-width bb) 2)
                           :height (- (box-height bb) 3)))
        (rm (create-m-room rb s))
        (m  (storey-matrix s))
        )
   (describe rb)
   (describe s)
   ;; (put-room rm)
   (setf (aref m 1 2) #\*)
   (let ((*print-length* 1000))
     (print m))
   )
 )

