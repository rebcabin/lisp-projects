;;; R E N D E R I N G ==========================================================

(defmethod write-clip-char
    ((bb box) (c character) (x integer) (y integer)
     (glyph-writer function))
  (when (and (>= x (box-left   bb))
             (<  x (box-right  bb))
             (>= y (box-top    bb))
             (<  y (box-bottom bb)))
    (funcall glyph-writer x y c)
    ))

(defun window-glyph-writer (x y c)
  '(write-char-at-point *standard-window* c x y)
  ;; Must use low-level mvwaddch so we can write to the last position in the
  ;; screen. See https://manned.org/mvwaddch: "If scrollok is not enabled,
  ;; writing a character at the lower right margin succeeds. However, an error
  ;; is returned because it is not possible to wrap to a new line.
  ;; --> ignore errors here <-
  (charms/ll:mvwaddch (charms::window-pointer *standard-window*)
                      y x
                      (charms::character-to-c-char c)))

(defmethod incr ((c character))
  (code-char (1+ (char-code c))))

(defmethod draw-line% ((bb box) (tl point) (br point) (glypher function))
  "Bresenham's cribbed from http://goo.gl/9ptT1g."
  (declare (ignorable bb))
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
                (write-clip-char
                 bb (funcall glypher x y 'straight)
                 x y
                 #'window-glyph-writer)
                (write-clip-char
                 bb (funcall glypher x y 'swapped )
                 y x
                 #'window-glyph-writer))
            (setf erroire (- erroire delta-y))
            ;(setf c (incr c))
            (when (< erroire 0)
              (incf y y-step)
              (incf erroire delta-x)))) ))

(defun draw-line (&key bounding-box from-point to-point glyph-fn)
  (draw-line% bounding-box from-point to-point glyph-fn))

(defmethod draw ((b box) (wb box) (c character))
  (let ((tl (box-top-left b))
        (tr (decr-x (box-top-right b)))
        (bl (decr-y (box-bottom-left b)))
        (br (decr-x (decr-y (box-bottom-right b))))
        (gl (lambda (x y dir) (declare (ignorable x y dir)) c)))
    (draw-line% *window-box* tl tr gl)
    (draw-line% *window-box* tl bl gl)
    (draw-line% *window-box* br tr gl)
    (draw-line% *window-box* br bl gl)
    ))

