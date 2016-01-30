(load "~/quicklisp/setup.lisp")
(ql:quickload :lparallel)
(use-package :cl :lparallel)

;; None of the commented-out lines made appreciable difference.

(declaim (optimize (speed 3) (safety 0)))
;; (proclaim '(inline image-add))
;; (declaim (inline (image-add)))

(defun image-add (to from val)
  (declare (type (simple-array single-float (*))
                 to from))
  (declare (type single-float val))
  (let ((size (array-dimension to 0)))
    (dotimes (i size)
      (setf (aref to i) (+ (aref from i) val)))))

(defparameter HORZ 800)
(defparameter VERT 800)

(defparameter PERF-REPS 1000)

(defun test% ()
  (declare (type fixnum HORZ VERT PERF-REPS))
  (let ((to (make-array (* HORZ VERT) :element-type 'single-float))
        (fm (make-array (* HORZ VERT) :element-type 'single-float)))
    (time (dotimes (_ PERF-REPS)
            (image-add to fm 42.0)))))

(setf lparallel:*kernel* (lparallel:make-kernel 4))

(defun test ()
  (declare (type fixnum HORZ VERT PERF-REPS))
  (let ((to (make-array (* HORZ VERT) :element-type 'single-float))
        (fm (make-array (* HORZ VERT) :element-type 'single-float)))
    (time (dotimes (_ PERF-REPS)
            (lparallel:pmap-into to (lambda (x) (+ x 42f0)) fm)))))

(test)
