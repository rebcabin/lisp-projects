(load "~/quicklisp/setup.lisp")
(ql:quickload :lparallel)

(declaim (optimize (speed 3) (safety 0)))

(defparameter HORZ 800)
(defparameter VERT 800)

(defparameter PERF-REPS 1000)

(setf lparallel:*kernel* (lparallel:make-kernel 4))

(defun test ()
  (declare (type fixnum HORZ VERT PERF-REPS))
  (let ((to (make-array (* HORZ VERT) :element-type 'single-float))
        (fm (make-array (* HORZ VERT) :element-type 'single-float)))
    (time (dotimes (_ PERF-REPS)
            (lparallel:pmap-into to (lambda (x) (+ x 42f0)) fm)))))

(test)
