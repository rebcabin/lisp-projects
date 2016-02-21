(load "point.lisp")
(load "box.lisp")

;;; Example quickcheck generator (mnemonic illustration)

(defun a-color ()
  (pick-weighted
    (1 'red)
    (2 'green)
    (1 'blue)))

;;; Until we know better, we'll allow boxes to have negative width and height.

(defun a-positive-index ()
  (+ 1 (funcall an-index)))

(defun a-positive-box ()
  (make-instance 'box
                 :left   (funcall an-integer)
                 :top    (funcall an-integer)
                 :width  (a-positive-index)
                 :height (a-positive-index)))

(defun an-m-block ()
  (make-instance 'm-block
                 :left (funcall an-integer)
                 :top  (funcall an-integer)))

(quickcheck

  ;; Checking quickcheck itself:

 (is= 4 (+ 1 3))

 (is = 4 (+ 1 3))

 (for-all ((c #'a-color))
          (test (symbolp c))
          (test (or (equal c 'red)
                    (equal c 'green)
                    (equal c 'blue))))

 ;; Make sure a box of unit width and height contains a point that rattles
 ;; around.

 (let ((b (make-instance 'box :left 0 :top 0 :width 1 :height 1))
       (p (zero-point)))
   (test (point-in-box-p p b))
   (displace-right p)
   (test (point-in-box-p p b))
   (displace-down p)
   (test (point-in-box-p p b))
   )

 ;; A unit box cannot contain a point if the boundary of the box is excluded.

 (let ((b (make-instance 'box :left 0 :top 0 :width 1 :height 1))
       (p (zero-point)))
   (test (not (point-in-box-p p b :boundary 1)))
   (displace-right p)
   (test (not (point-in-box-p p b :boundary 1)))
   (displace-down p)
   (test (not (point-in-box-p p b :boundary 1)))
   )

 ;; Make sure m-blocks work.

 (for-all ((bi #'an-m-block))
          (test (and (= 1 (box-width  bi))
                     (= 1 (box-height bi)))))

 ;; Make sure m-blocks test as inside sufficiently large boxes.

 (for-all ((bo #'a-positive-box))
          (let ((bi (make-instance 'm-block
                                   :left (box-left bo)
                                   :top  (box-top  bo))))
            (test (box-in-box-p bi bo))))

 )
