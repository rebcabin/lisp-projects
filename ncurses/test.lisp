(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-quickcheck)

(let ((*random-state* (make-random-state t))
      (*print-length* 6)
      (*load-verbose* t))

  (shadow 'cl-quickcheck:report '#:cl-user)
  (use-package :cl-quickcheck)

  (load "test/point.lisp")
  (load "test/box.lisp")
  (load "test/rendering.lisp")
  (load "test/world.lisp")
  (load "test/storey.lisp")
  (load "test/room.lisp")
  )
