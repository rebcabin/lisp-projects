(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-quickcheck)

(cl-quickcheck:quickcheck
 (load "self-test.lisp")
 (load "updoc.lisp")
 (load "alpha.lisp")

 (load "inv-idx.lisp")
 (load "lsets.lisp")
 (load "money.lisp")
 (load "qcpaper.lisp"))
