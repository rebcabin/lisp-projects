(setf *random-state* (make-random-state t))

(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-quickcheck)
(shadow 'cl-quickcheck:report '#:cl-user)
(use-package :cl-quickcheck)

;;; The reports of random state by quickcheck are not useful. Check manually
;;; that the random state is actually random by doing something like (funcall
;;; an-integer) after pasting the lines above into a fresh sbcl session.
;;;
;;; If you want the same random state every time you run the tests, please
;;; just comment out the first line above.

(setf *print-length* 6)

(load "test/point.lisp")
(load "test/box.lisp")
(load "test/rendering.lisp")
