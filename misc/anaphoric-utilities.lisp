;;                      _
;;  __ _ _ _  __ _ _ __| |_  ___ _ _ __ _
;; / _` | ' \/ _` | '_ \ ' \/ _ \ '_/ _` |
;; \__,_|_||_\__,_| .__/_||_\___/_| \__,_|
;;                |_|

(defmacro aif (test-form then-form &optional else-form)
  "Example: (aif it (foo it) (bar it))"
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  "Example: (awhen it (foo it) (bar it))"
  `(aif ,test-form
        (progn ,@body)))

(defmacro awhile (expr &body body)
  "Example: (awhile it (foo it) (bar it))"
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro aand (&rest args)
  "Example: (aand (owner x0) (address it) (town it))"
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif , (car args) (aand ,@(cdr args))))))

(defmacro acond (&rest clauses)
  "Example: (acond (42 (print it)) (t nil))"
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (sym (gensym)))
        `(let ((,sym , (car cl1)))
           (if ,sym
               (let ((it ,sym)) ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))



