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

(quickcheck

 (for-all ((b #'a-positive-box))
          (let ((r (create-m-room b)))
            (test (boxes-equal b (m-room-its-box r))))
          )

  )

