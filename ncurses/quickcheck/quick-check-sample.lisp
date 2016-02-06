(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-quickcheck)

;; (load "cl-quickcheck.lisp")

(defmethod resilient-load ((file-name string))
  (load (concatenate 'string (sb-posix:getcwd) "/" file-name)))

(shadow 'cl-quickcheck:report '#:cl-user)

(cl-quickcheck:quickcheck
 (resilient-load "self-test.lisp")
 (resilient-load "updoc.lisp")
 (resilient-load "alpha.lisp")
  ;; Try some sample testers
 (resilient-load "inv-idx.lisp")
 (resilient-load "lsets.lisp")
 (resilient-load "money.lisp")
 (resilient-load "qcpaper.lisp"))
