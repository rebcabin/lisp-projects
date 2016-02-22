(load "geo.lisp")

(quickcheck
 (is= 4 4)

 (let ((w (make-instance 'wall)))
   (is equal
       (slot-value w 'render-char)
       +regular-wall-glyph+))

 )

