;;  _ __  __ _ _ __ _ __  ___ _ _ ___
;; | '  \/ _` | '_ \ '_ \/ -_) '_(_-<
;; |_|_|_\__,_| .__/ .__/\___|_| /__/
;;            |_|  |_|

;;; mapa-b
;;; map0-n
;;; map1-n
;;; range
;;; range-a-b
;;; map->
;;; mapcars
;;; rmapcar
;;; mappend

(defun mapa-b (fn a b &optional (step 1))
  (map-> fn a
         #'(lambda (x) (> x b))
         #'(lambda (x) (+ x step))))

(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun range (n)
  (map1-n #'identity n))

(defun range-a-b (a b)
  (mapa-b #'identity a b))

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

(defun mapcars (fn &rest lsts)
  "Map over the conc of multiple inputs without unnecessary consing."
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))

(defun rmapcar (fn &rest args)
  "Map over identically shaped trees."
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
             #'(lambda (&rest args)
                 (apply #'rmapcar fn args))
             args)))

(defmacro mappend (fn &rest lists)
  "Example: (mappend #'list '(1 2 3) '(4 5 6)) ~~> (1 4 2 5 3 6); The
all-powerful flatmap -- non-destructive version."
  `(apply #'append (mapcar ,fn ,@lists)))

