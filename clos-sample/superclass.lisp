(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-quickcheck)
(use-package :cl-quickcheck)

(defclass foo ()
  ((x :accessor foo-x :initarg :x)))

(defclass bar (foo) ())

(describe (make-instance 'foo :x 42))
(describe (make-instance 'bar :x 39))

(defmethod get-x ((f foo))
  (foo-x f))

(print (format nil "get-x 1: ~A" (get-x (make-instance 'foo :x 42))))
(print (format nil "get-x 2: ~A" (get-x (make-instance 'bar :x 39))))

(defmethod get-xs ((f foo) (g foo))
  (list (foo-x f) (foo-x g)))

(print
 (format nil "get-xs: ~A"
         (get-xs (make-instance 'foo :x 42)
                 (make-instance 'bar :x 39))))

(defclass box ()
  ((left   :accessor box-left   :initform 0 :initarg :left  )
   (top    :accessor box-top    :initform 0 :initarg :top   )
   (width  :accessor box-width  :initform 0 :initarg :width )
   (height :accessor box-height :initform 0 :initarg :height)))

(defmethod box-bottom ((b box))
  (+ (box-top b) (box-height b)))

(defmethod box-right ((b box))
  (+ (box-left b) (box-width b)))


(defclass m-block (box) ())
(defmethod initialize-instance
    :around
    ((mb m-block)
     &key (left 0) (top 0))
  (call-next-method mb :left left :top top :width 1 :height 1))

(defmethod box-in-box-p ((bi box) (bo box) &key (boundary 0))
  (let ((lb (+ (box-left   bo) boundary))
        (rb (- (box-right  bo) boundary))
        (tb (+ (box-top    bo) boundary))
        (bb (- (box-bottom bo) boundary))
        (xl (box-left   bi))
        (xr (box-right  bi))
        (yt (box-top    bi))
        (yb (box-bottom bi)))
    (and (>= xl lb)
         (<= xr rb)
         (>= yt tb)
         (<= yb bb))))

(print
 (format
  nil
  "box-in-box-p: ~A"
  (let ((bo (make-instance 'box :left  0 :width 1 :top  0 :height 1))
        (bi (make-instance 'm-block :left 0 :top 0)))
    (describe bo)
    (describe bi)
    (box-in-box-p bi bo))))

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

(describe (a-positive-box))
(describe (an-m-block))

(print
 (format
  nil
  "box-in-box-p 2: ~A"
  (let ((bo (a-positive-box))
        (bi (an-m-block)))
    (describe bo)
    (describe bi)
    (box-in-box-p bi bo))))


