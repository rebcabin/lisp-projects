;;  _ _    _
;; | (_)__| |_ ___
;; | | (_-<  _(_-<
;; |_|_/__/\__/__/

;;; last1
;;; single
;;; append1
;;; conc1
;;; mklist
;;; flatten
;;; prune

(proclaim '(inline last1 single append1 conc1 mklist))

(defun last1 (lst)
  "Example: (last1 '(1 2 3)) ~~> 3; Produces the last element (rather
than the last cons) in a list."
  (car (last lst)))

(defun single (lst)
  "Example: (single '(1)) ~~> T; True if the list is a singleton."
  (and (consp lst) (not (cdr lst))))

(defun append1 (lst obj)
  "Example: (append1 () 1) ~~> 1; Appends a single item to a list."
  (append lst (list obj)))

(defun conc1 (lst obj)
  "Example: (conc1 () 1) ~~> 1; Destructively appends a single item to a
list."
  (nconc lst (list obj)))

(defun mklist (obj)
  "Examples: (mklist 1) ~~> (1), (mklist '(1)) ~~> (1); Produces a
singleton list from a non-list object, or just the object itself if the
object is a list."
  (if (listp obj) obj (list obj)))

(defun flatten (xs)
  "Example: (flatten '(a (b c) ((d e) f))) ~~> (A B C D E F)"
  (labels ((rec (xs acc)
             (cond ((null xs) acc)
                   ((atom xs) (cons xs acc))
                   (t (rec (car xs) (rec (cdr xs) acc))))))
    (rec xs nil)))

(defun prune (test tree)
  "Example: (prune #'evenp '(1 2 (3 (4 5) 6) 7 8 (9))) ~~> (1 (3 (5)) 7 (9))"
  (labels ((rec (tree acc)
             (cond ((null tree)
                    (nreverse acc))
                   
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   
                   (t
                    (rec (cdr tree)
                         (if (funcall test (car tree))
                             acc
                             (cons (car tree) acc)))))))
    (rec tree nil)))

