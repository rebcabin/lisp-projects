(load "point.lisp")
(load "box.lisp")
(load "rendering.lisp")

;;; Coordinates label the infinitely thin lines between pixels.

(defparameter *the-written-state*  '())
(defparameter *the-expected-state* '())

(defun mock-glyph-writer (x y c)
  (push `(,x ,y ,c) *the-written-state*))

(quickcheck

  (is= 5 (+ 2 3))

  (let ((bb (make-instance 'box :left 42 :top 24 :width 3 :height 2)))
    (write-clip-char bb 42 24 #\. #'mock-glyph-writer)
    (is equalp `((42 24 #\.)) *the-written-state*)

    (setf *the-written-state*  '())
    (setf *the-expected-state* '())

    (for-all ((x (lambda () (funcall an-integer)))
              (y (lambda () (funcall an-integer)))
              (c #'a-char))
      (when (and (>= x 42)
                 (<  x 45)
                 (>= y 24)
                 (<  y 26))
        (push `(,x ,y ,c) *the-expected-state*))
      (write-clip-char bb x y c #'mock-glyph-writer))

    (is equalp *the-written-state* *the-expected-state*))


  (setf *the-written-state*  '())
  (setf *the-expected-state* '())

  ;; (funcall an-integer) can return negatives. Therefore, we can get a box with
  ;; negative width or height. All rendering operations should clip in such a box.

  (for-all ((lf (lambda () (funcall an-integer)))
            (tp (lambda () (funcall an-integer)))
            (wd (lambda () (funcall an-index)))
            (ht (lambda () (funcall an-index))))
   (let ((bb (make-instance 'box :left lf :top tp :width wd :height ht)))
     (for-all ((x (lambda () (funcall an-integer)))
               (y (lambda () (funcall an-integer)))
               (c #'a-char))
       (when (and (>= x lf)
                  (<  x (+ lf wd))
                  (>= y tp)
                  (<  y (+ tp ht)))
         (push `(,x ,y ,c) *the-expected-state*))
       (write-clip-char bb x y c #'mock-glyph-writer))

     (is equalp *the-written-state* *the-expected-state*)))

  (print *the-written-state*)

  )
