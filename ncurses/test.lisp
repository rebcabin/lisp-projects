(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-quickcheck)
(ql:quickload :defenum)
(ql:quickload :alexandria)
(ql:quickload :hash-set)

(let ((*random-state* (make-random-state t))
      (*print-length* 6)
      (*load-verbose* t))

  (shadow 'cl-quickcheck:report '#:cl-user)
  (shadow 'defenum:enum         '#:cl-user)
  (use-package :cl-quickcheck)
  (use-package :defenum)

  ;; This sequence of 'load' expressions should parallel the 'load's at the head
  ;; of any main like 'charms-test-5.lisp'.

  (load "test/point.lisp")
  (load "test/box.lisp")
  (load "test/glyph.lisp")
  (load "test/geo.lisp")
  (load "test/world.lisp")
  (load "test/storey.lisp")
  (load "test/rendering.lisp")
  (load "test/room.lisp")
  )
