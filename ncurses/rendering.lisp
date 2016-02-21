(defmethod write-clip-char ((bb box)
                            (x integer) (y integer) (c character)
                            (gw function))
  "gw is a glyph-writer, passed in to ease testing."
  (when (and (>= x (box-left   bb))
             (<  x (box-right  bb))
             (>= y (box-top    bb))
             (<  y (box-bottom bb)))
    (funcall gw x y c)))

;;; Cycles a character to show streams running across a screen.

(defmethod incr ((c character))
  (code-char (1+ (char-code c))))

;;; Internal functions may have variables with short names.

;;; A little pattern, here, is to have methods for dispatch-on-type covered by
;;; functions with named, keyword parameters to ease the burden on memory.

(defmethod draw-line% ((bb box) (tl point) (br point)
                       (gf function) (gw function))
  "Bresenham's cribbed from http://goo.gl/9ptT1g."
  (let* (; (c  #\0) ;debugging
         (x1 (point-y tl))
         (x2 (point-y br))
         (y1 (point-x tl))
         (y2 (point-x br))
         (dist-x (abs (- x1 x2)))
         (dist-y (abs (- y1 y2)))
         (steep  (> dist-y dist-x)))
    (when steep
      (psetf x1 y1 y1 x1
             x2 y2 y2 x2))
    (when (> x1 x2)
      (psetf x1 x2 x2 x1
             y1 y2 y2 y1))
    (let* ((delta-x (- x2 x1))
           (delta-y (abs (- y1 y2)))
           (erroire (floor delta-x 2))
           (y-step  (if (< y1 y2) 1 -1))
           (y       y1))
      (loop
        :for x :upfrom x1 :to x2
        :do (if steep
                (write-clip-char bb x y (funcall gf x y :straight) gw)
                (write-clip-char bb y x (funcall gf x y :swapped ) gw))
            (setf erroire (- erroire delta-y))
            ;(setf c (incr c))
            (when (< erroire 0)
              (incf y y-step)
              (incf erroire delta-x)))) ))

(defun draw-line (&key
                    bounding-box
                    from-point
                    to-point
                    glyph-fn
                    glyph-writer-fn)
  (draw-line% bounding-box
              from-point
              to-point
              glyph-fn
              glyph-writer-fn))

(defmethod draw-box% ((bb box) (bd box) (gl function) (gw function))
  (let ((tl (box-top-left                     bd))
        (tr (decr-x (box-top-right            bd)))
        (bl (decr-y (box-bottom-left          bd)))
        (br (decr-x (decr-y (box-bottom-right bd)))))
    (draw-line% bb tl tr gl gw)
    (draw-line% bb tl bl gl gw)
    (draw-line% bb br tr gl gw)
    (draw-line% bb br bl gl gw)))

(defun draw-box (&key
                   bounding-box
                   box-to-draw
                   glyph-fn
                   glyph-writer-fn)
  (draw-box% bounding-box
             box-to-draw
             glyph-fn
             glyph-writer-fn))



