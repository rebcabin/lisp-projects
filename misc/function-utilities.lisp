;;   __              _   _
;;  / _|_  _ _ _  __| |_(_)___ _ _  ___
;; |  _| || | ' \/ _|  _| / _ \ ' \(_-<
;; |_|  \_,_|_||_\__|\__|_\___/_||_/__/

(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

(defmacro flip (form)
  (let ((a (gensym)) (b (gensym)))
    `(lambda (,a ,b) (,form ,b ,a))))

(defmacro cons-reverse ((a . b))
  (progn
    ;; (format t "A: ~a~%" a)
    ;; (format t "B: ~a~%" b)
    ;; (format t "(A . B): ~a~%" `(,a . ,b))
    `'(,b . ,a)))

(defmacro fn (expr)
  "fn's argument should be an expression of the form (operator
. arguments). The operator can be the name of a function or macro -- or
\"compose\", which is treated specially. The arguments can be names of
functions or macros of one argument, or expressions that could be
arguments to fn."
  `(function ,(rbuild expr)))

(defun rbuild (expr)
  (if (or (atom expr) (eq (car expr) 'lambda))
      expr
      (if (eq (car expr) 'compose)      ; special treatment for "compose"
          (build-compose (cdr expr))
          (build-call (car expr) (cdr expr)))))

(defun build-compose (fns)
  (let ((g (gensym)))
    `(lambda (,g)
       ,(labels ((rec (fns)
                      (if fns
                          `(,(rbuild (car fns))
                             ,(rec (cdr fns)))
                          g)))
                (rec fns)))))

(defun build-call (op fns)
  (let ((g (gensym)))
    `(lambda (,g)
       (,op ,@(mapcar #'(lambda (f)
                          `(,(rbuild f) ,g))
                      fns)))))

(defun lrec (rec &optional base)
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                     base)
                 (funcall rec (car lst)
                          #'(lambda ()
                              (self (cdr lst)))))))
    #'self))

(defmacro alrec (rec &optional base)
  "cltl2 version"
  (let ((gfn (gensym)))
    `(lrec #'(lambda (it ,gfn)
               (symbol-macrolet ((rec (funcall ,gfn)))
                 ,rec))
           ,base)))

