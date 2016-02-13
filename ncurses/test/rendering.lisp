(load "point.lisp")
(load "box.lisp")
(load "rendering.lisp")

;;; Coordinates label the infinitely thin lines between pixels.

(defparameter *the-written-state*  '())
(defparameter *the-expected-state* '())

(defun mock-glyph-writer (x y c)
  (push `(,x ,y ,c) *the-written-state*))

(defun clearum ()
  (setf *the-written-state*  '())
  (setf *the-expected-state* '()))

(defmacro write-to-box% (w-expr h-expr)
  `(progn
     (clearum)
     (for-all ((lf (lambda () (funcall an-integer)))
               (tp (lambda () (funcall an-integer)))
               (wd (lambda () ,w-expr))
               (ht (lambda () ,h-expr)))
       (let ((bb (make-instance 'box :left lf :top tp :width wd :height ht)))
         (for-all ((x (lambda () (funcall an-integer)))
                   (y (lambda () (funcall an-integer)))
                   (c #'a-char))
           (write-clip-char bb x y c #'mock-glyph-writer))
         (is equalp *the-written-state* *the-expected-state*)))))

(quickcheck

  ;; Make sure quickcheck still works.

  (is= 5 (+ 2 3))

  ;; Make a particular small box and check that characters are correctly clipped
  ;; when written inside it.

  (let ((bb (make-instance 'box :left 42 :top 24 :width 3 :height 2)))
    (write-clip-char bb 42 24 #\. #'mock-glyph-writer)
    (is equalp `((42 24 #\.)) *the-written-state*)

    (clearum)

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

  (clearum)

  ;; Make a box at a random location and with random width and height. Write
  ;; characters at random positions within the box, checking clipping. Use
  ;; "an-index" to ensure box has non-negative width and height.

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

  ;; Boxes with non-positive width or height are legal. All rendering operations
  ;; should clip in such a box.

  (write-to-box% (- (funcall an-index)) (funcall an-integer))
  (write-to-box% (funcall an-integer)   (- (funcall an-index)))
  (write-to-box% (- (funcall an-index)) (- (funcall an-index)))

  (write-to-box% 0                    (funcall an-integer))
  (write-to-box% (funcall an-integer) 0)
  (write-to-box% 0                    0)

  )
