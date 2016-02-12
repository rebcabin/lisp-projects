;;                _         _ _
;;  ____  _ _ __ | |__  ___| (_)__ ___
;; (_-< || | '  \| '_ \/ _ \ | / _(_-<
;; /__/\_, |_|_|_|_.__/\___/_|_\__/__/
;;     |__/

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))

(defun explode (sym)
  (map 'list #'(lambda (c)
                 (intern (make-string 1 :initial-element c)))
       (symbol-name sym)))

