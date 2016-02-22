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
        (bb (storey-bounding-box s)))
   (describe s)
   )
 )

