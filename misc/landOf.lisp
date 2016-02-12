;;                      _
;;  __ _ _  _ ___ _____(_)_ _  __ _   __ _ __ _ _ __  ___
;; / _` | || / -_|_-<_-< | ' \/ _` | / _` / _` | '  \/ -_)
;; \__, |\_,_\___/__/__/_|_||_\__, | \__, \__,_|_|_|_\___|
;; |___/                      |___/  |___/

(defparameter *small* 1)
(defparameter *big* 100)

(defun guess-my-number ()
  (ash (+ *small* *big*) -1))

(defun smaller ()
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))

(defun bigger ()
  (setf *small* (1+ (guess-my-number)))
  (guess-my-number))

(defun start-over ()
  (defparameter *small* 1)
  (defparameter *big* 100)
  (guess-my-number))

;;                                      _
;;  __ _ __ _ _ __  ___   ___ _ _  __ _(_)_ _  ___
;; / _` / _` | '  \/ -_) / -_) ' \/ _` | | ' \/ -_)
;; \__, \__,_|_|_|_\___| \___|_||_\__, |_|_||_\___|
;; |___/                          |___/


(defparameter *nodes*
  '((living-room (you are in the living-room.
                  a wizard is snoring loudly on the couch.))
    (garden (you are in a beautiful garden.
             there is a well in front of you.))
    (attic (you are in the attic.
            there is a giant welding torch in the corner.))))

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(defparameter *edges*
  '((living-room
     (garden west door)
     (attic upstairs ladder))
    (garden
     (living-room east door))
    (attic
     (living-room downstairs ladder))))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path
                          (cdr (assoc location edges)))))

(defparameter *objects*
  '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
                                   (bucket  living-room)
                                   (chain   garden)
                                   (frot    garden)))

(defun objects-at (loc objs obj-locs)
  "Produces all the objects in a given object-list 'objs' that are present at a
given location 'loc.'"
  (labels ((at-loc-p (obj)
             (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-locs)
  (labels ((describe-obj (obj)
             `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-locs)))))

(defparameter *location* 'living-room)

(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths    *location* *edges*)
          (describe-objects  *location* *objects* *object-locations*)))

(defun walk (direction)
  (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
    (if next
        (progn (setf *location* (car next))
               (look))
        '(you cannot go that way.))))

;;; A more flexible design for "pickup" would be to decrement the count
;;; of objects at the current location and increment the count of
;;; matching objects in the inventory. Such a design requires count data
;;; to be stored with objects at locations.

(defun pickup (object)
  (cond ((member object
                 (objects-at *location* *objects* *object-locations*))
         ;; Notice that the object has not be removed from its original
         ;; location. Pickup actually replicates the object on the
         ;; body. Because 'body appears first in a list of locations,
         ;; however, "looK" will report it on the body and not in the
         ;; original location, because "look" eventually uses "assoc,"
         ;; which reports the first find; e.g. (assoc 1 '((1 . a) (1 . b)))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        (t '(you cannot get that.))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(defun say-hello ()
  (princ "Please type your name: ")
  (let ((name (read-line)))
    (princ "Nice to meet you, ")
    (princ name)))

(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read ()
  (let ((cmd (read-from-string
              (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x) (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

;;       _   _ _ _ _   _
;;  _  _| |_(_) (_) |_(_)___ ___
;; | || |  _| | | |  _| / -_|_-<
;;  \_,_|\__|_|_|_|\__|_\___/__/

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names
                    collect (gensym))))
    `(let (,@(loop for g in gensyms
                collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names
                    collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms
                      collect `(,n ,g)))
                ,@body)))))

(defmacro insinuate (term (head . tail))
  `(,head ,term . ,tail))

(defmacro extenuate (term (head . tail))
  `(,head ,@tail ,term))

;;; I don't yet have total confidence in the following macros; I am
;;; concerned that they might evaluate "fs" too often. However, they
;;; seem to work properly. My guess is that because they expand
;;; recursively into macro applications, no evaluation takes place till
;;; the very end. Macroexpand-1 is uninstructive at understanding my own
;;; creation.

(defmacro -> (f &rest fs)
  "Shove expressions into the first argument slots of following expressions, e.g., (-> a (b z) (c y) ... (m n)) ~~> (m (... (c (b a z) y) ...) n) (-> 1 (list 2 3) (list 4 5) (list 6 7)) ~~> (((1 2 3) 4 5) 6 7)"
  (if (null fs)
      f
      `(-> (insinuate ,f ,(car fs)) ,@(cdr fs))))

(defmacro ->> (f &rest fs)
  "Shove expressions into the last argument slots of following expressions, e.g., (->> a (b z) (c y) ... (m n)) ~~> (m n (... (c y (b z a)) ...)) (->> 1 (list 2 3) (list 4 5) (list 6 7)) ~~> (6 7 (4 5 (2 3 1)))"
  (if (null fs)
      f
      `(->> (extenuate ,f ,(car fs)) ,@(cdr fs))))

;;            _ _       _          _      __             _
;;  _  _ _ _ (_) |_ ___| |_ ___ __| |_   / _|_ ____ __ _| |__
;; | || | ' \| |  _|___|  _/ -_|_-<  _| |  _| '  \ V  V / / /
;;  \_,_|_||_|_|\__|    \__\___/__/\__| |_| |_|_|_\_/\_/|_\_\

(defvar *test-name* nil)
(defvar *test-pass-count* 0)
(defvar *test-fail-count* 0)
(defvar *test-total-count* 0)

(defun reset-test-statistics ()
  (setf *test-pass-count* 0)
  (setf *test-fail-count* 0)
  (setf *test-total-count* 0))

(defun report-test-statistics ()
  (format t "total number of tests: ~4d~%" *test-total-count*)
  (format t "total passed tests:    ~4d~%" *test-pass-count*)
  (format t "total failed tests:    ~4d~%" *test-fail-count*))

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can combine-results
   of other test functions or use 'check' to run individual test cases."
  `(defun ,name ,parameters
    (let ((*test-name* (append *test-name* (list ',name))))
      ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case, reporting results by
side-effect."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "A non-short-circuiting AND: combine the results (as booleans) of
evaluating 'forms' in order, without short-circuiting on the first NIL
result. Will report a global NIL if any result is NIL."
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (if result
      (incf *test-pass-count*)
      (incf *test-fail-count*))
  (incf *test-total-count*)
  (format t "~:[FAIL~;pass~] #~a ... ~a: ~a~%"
          result *test-total-count* *test-name*
          (if result t form))
  result)

;;  _ _    _
;; | (_)__| |_ ___
;; | | (_-<  _(_-<
;; |_|_/__/\__/__/

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

;;  _ __  __ _ _ __ _ __  ___ _ _ ___
;; | '  \/ _` | '_ \ '_ \/ -_) '_(_-<
;; |_|_|_\__,_| .__/ .__/\___|_| /__/
;;            |_|  |_|

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

(deftest test-mappers ()
  (check
    (equal '(1 2 3)   (map1-n #'identity 3))
    (equal '(0 1 2 3) (map0-n #'identity 3))

    (equal '(2 4 6)   (mapcar #'+ '(1 2 3) (range 3)))

    (equal (mapcar (lambda (x) (* x x)) (range 6))
           (mapcars (lambda (x) (* x x)) (range 3) (range-a-b 4 6)))

    (flet ((square (x) (* x x)))
      (equal (mapcar #'square (range 6))
             (mapcars #'square (range 3) (range-a-b 4 6))))

    (flet ((square (x) (* x x)))
      (equal '(1 4 (9 16 (25) 36) 49 (64 81))
             (rmapcar #'square '(1 2 (3 4 (5) 6) 7 (8 9)))))
    (equal '(1 4 (9 16 (25) 36) 49 (64 81))
             (rmapcar #'*
                      '(1 2 (3 4 (5) 6) 7 (8 9))
                      '(1 2 (3 4 (5) 6) 7 (8 9))))
    (equal '(1 4 (9 16 (25) 36) 49 (64 81))
           (rmapcar #'*
                    '(1 2 (3 4 (5   ) 6) 7 (8 9   )   )
                    '(1 2 (3 4 (5 11) 6) 7 (8 9 10) 12)))

    (equal '(1 4 2 5 3 6) (mappend #'list '(1 2 3) '(4 5 6)))

    (equal '(4 3 2 1) (->> () (cons 1) (cons 2) (cons 3) (cons 4)))

    (equal '(((1 2) 3) 4) (-> 1 (list 2) (list 3) (list 4)))
    ))

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

(deftest test-anaphora ()
  (check
    (= 42 (aand 42))
    (= 43 (aand 1 (+ 42 it)))
    (= 86 (aand 1 (+ 42 it) (* 2 it)))
    (= 42 (acond (42 it) (t nil)))
    (= 42 (awhile 42 (return it)))
    (= 42 (awhen 41 (1+ it)))
    (= 42 (aif 41 (1+ it)))
    ))

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

(deftest test-symbolics ()
  (check
    (equal "3.141592653589793d0 pieces of PI"
           (mkstr pi " pieces of " 'pi))

    (eq    '|ARMadiLL0|
           (symb 'ar "Madi" #\L #\L 0))

    (eq '|(a . b)|
        (symb "(a . b)"))

    (equal (explode 'pinto)
           '(P I N T O))
    ))

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

(defmacro fn (expr) `(function ,(rbuild expr)))

(defun rbuild (expr)
  (if (or (atom expr) (eq (car expr) 'lambda))
      expr
      (if (eq (car expr) 'compose)      ; special treatment
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

(deftest test-functions ()
  (check
    (= 42
       (funcall (compose (lambda (x) (+ 2 x))
                         (lambda (x) (* 5 x)))
                8))

    (let ((a 2) (b 5) (c 8))
      (= 42
         (funcall (compose (lambda (x) (+ a x))
                           (lambda (x) (* b x)))
                  c)))

    (equal (mklist 42)
           (funcall (compose #'list #'1+) 41))

    (funcall (alrec (and (oddp it) rec) t)
             '(1 3 5))

    (equal (mklist  4)
           (funcall (fn (compose list 1+ truncate)) pi))

    (equal 6
           (funcall (fn (compose (lambda (x) (+ x 3)) truncate))
                    pi))

    (equal '(nil t nil nil)
           (mapcar (fn (and integerp oddp))
                   '(c 3 p 0)))

    (equal '(t t t nil)
           (mapcar (fn (or integerp symbolp))
                   '(c 3 p 0.2)))

    (equal '(2 2 4 4 6 6)
           (map1-n (fn (if oddp 1+ identity)) 6))

    (equal '((0 1 2) (1 2 3) (2 3 4))
           (mapcar (fn (list 1- identity 1+)) '(1 2 3)))

    (equal '(C (D) 2 |3-4| 3.4)
           (remove-if (fn (or (and integerp oddp)
                              (and consp cdr)))
                      '(1 (a b) c (d) 2 3-4 3.4 (e f g))))
    ))

;;             _   _               _   _
;;  __ ___ _ _| |_(_)_ _ _  _ __ _| |_(_)___ _ _  ___
;; / _/ _ \ ' \  _| | ' \ || / _` |  _| / _ \ ' \(_-<
;; \__\___/_||_\__|_|_||_\_,_\__,_|\__|_\___/_||_/__/

;(defparameter *cont* #'identity) ; This may be wrong! (is it "special?")
;(setq *cont* #'identity)         ; This doesn't compile alone!

(defmacro =lambda (parms &body body)
  `#'(lambda (*cont* ,@parms) ,@body))

(defmacro =defun (name parms &body body)
  (let ((f (intern (concatenate 'string
                                "=" (symbol-name name)))))
    `(progn
       (defmacro ,name ,parms
         `(,',f *cont* ,,@parms))
       (defun ,f (*cont* ,@parms) ,@body))))

(defmacro =bind (parms expr &body body)
  `(let ((*cont* #'(lambda ,parms ,@body))) ,expr))

(defmacro =values (&rest retvals)
  `(funcall *cont* ,@retvals))

(defmacro =funcall (fn &rest args)
  `(funcall ,fn *cont* ,@args))

(defmacro =apply (fn &rest args)
  `(apply ,fn *cont* ,@args))

(deftest test-continuations ()
  (check))

;;                _             _          _
;;  _ __  __ _ __| |_ ___ _ _  | |_ ___ __| |_
;; | '  \/ _` (_-<  _/ -_) '_| |  _/ -_|_-<  _|
;; |_|_|_\__,_/__/\__\___|_|    \__\___/__/\__|

(defun test-utilities ()
  (reset-test-statistics)
  (combine-results
    (test-mappers)
    (test-anaphora)
    (test-symbolics)
    (test-functions)
    (test-continuations)
    )
  (report-test-statistics))

(test-utilities)

;;                                       _
;;  __ _ __ _ _ __  ___ ___ _____ ____ _| |
;; / _` / _` | '  \/ -_)___/ -_) V / _` | |
;; \__, \__,_|_|_|_\___|   \___|\_/\__,_|_|
;; |___/

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space)           (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit            (cons item (tweak-text rest nil lit)))
            (caps           (cons (char-upcase item) (tweak-text rest nil lit)))
            (t              (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
  (-> (string-trim "() " (prin1-to-string lst))
    (coerce 'list)
    (tweak-text t nil)
    (coerce 'string)
    (princ))
  (fresh-line))

;;   ___ _              _             ____
;;  / __| |_  __ _ _ __| |_ ___ _ _  |__  |
;; | (__| ' \/ _` | '_ \  _/ -_) '_|   / /
;;  \___|_||_\__,_| .__/\__\___|_|    /_/
;;                |_|

(defparameter *wizard-nodes*
  '((living-room (you are in the living-room.
                  a wizard is snoring loudly on the couch.))
    (garden (you are in a beautiful garden.
             there is a well in front of you.))
    (attic (you are in the attic. there
            is a giant welding torch in the corner.))))
(defparameter *wizard-edges*
  '((living-room
     (garden west door)
     (attic upstairs ladder))
    (garden
     (living-room east door))
    (attic
     (living-room downstairs ladder))))

;;                     _           _
;;  __ _ _ _ __ _ _ __| |_ _____ _(_)___
;; / _` | '_/ _` | '_ \ ' \___\ V / |_ /
;; \__, |_| \__,_| .__/_||_|   \_/|_/__|
;; |___/         |_|

(defun dot-name (exp)
  (substitute-if
   #\_
   (complement #'alphanumericp)
   (prin1-to-string exp)))

(defparameter *max-label-length* 30)
(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
        (if (> (length s) *max-label-length*)
            (concatenate
             'string
             (subseq s 0 (- *max-label-length* 3)) "...")
            s))
      ""))

(defun nodes->dot (nodes)
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes))

(defun edges->dot (edges)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line)
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ "\"];"))
                (cdr node)))
        edges))

(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

(defun dot->file (fname thunk)
  (with-open-file (*standard-output*
                   fname
                   :direction :output
                   :if-exists :supersede)
    (funcall thunk)))

(defun graph->file (fname nodes edges)
  (dot->file fname
             (lambda ()
               (graph->dot nodes edges))))

(defun uedges->dot (edges)
  (maplist (lambda (lst)
             (mapc (lambda (edge)
                     (unless (assoc (car edge) (cdr lst))
                       (fresh-line)
                       (princ (dot-name (caar lst)))
                       (princ "--")
                       (princ (dot-name (car edge)))
                       (princ "[label=\"")
                       (princ (dot-label (cdr edge)))
                       (princ "\"];")))
                   (cdar lst)))
           edges))

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->file (fname nodes edges)
  (dot->file fname (lambda () (ugraph->dot nodes edges))))
