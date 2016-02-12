;;             _   _               _   _
;;  __ ___ _ _| |_(_)_ _ _  _ __ _| |_(_)___ _ _  ___
;; / _/ _ \ ' \  _| | ' \ || / _` |  _| / _ \ ' \(_-<
;; \__\___/_||_\__|_|_||_\_,_\__,_|\__|_\___/_||_/__/

;;; (defparameter *cont* #'identity) ; This is wrong!  It is "special,"
;;; which means that current-continuation in ,@body in =bind becomes
;;; bound to #'(lambda ,parms ,@body) instead of to its lexical value,
;;; and it is the lexical value we want. SBCL, problematically, does not
;;; have a convention for lexically global variables. Therefore,
;;; compilation fails with a warning that current-continuation is an
;;; undefined variable. This causes operational problems in slime, which
;;; interrupts flow to confirm that you want to load the fasl anyway. A
;;; workaround is to use "C-c C-l," i.e., "slime-load-file" instead of
;;; "C-c C-k," "slime-compile-and-load-file."

;;; TODO: RESEARCH how either to disable the warning or to
;;; create kosher lexical global variables.

(setq current-continuation #'identity)         ; This doesn't compile alone!

(defmacro =lambda (parms &body body)
  `#'(lambda (current-continuation ,@parms) ,@body))

(defmacro =defun (name parms &body body)
  (let ((f (intern (concatenate 'string "=" (symbol-name name)))))
    `(progn
       (defmacro ,name ,parms
         `(,',f current-continuation ,,@parms))
       (defun ,f (current-continuation ,@parms) ,@body))))

(defmacro =bind (parms expr &body body)
  `(let ((current-continuation #'(lambda ,parms ,@body))) ,expr))

(defmacro =values (&rest retvals)
  `(funcall current-continuation ,@retvals))

(defmacro =funcall (fn &rest args)
  `(funcall ,fn current-continuation ,@args))

(defmacro =apply (fn &rest args)
  `(apply ,fn current-continuation ,@args))

