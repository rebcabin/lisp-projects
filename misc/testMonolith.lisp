(load "macro-utilities.lisp")
(load "unit-test-framework.lisp")
(load "list-utilities.lisp")
(load "mapper-utilities.lisp")
(load "function-utilities.lisp")
(load "anaphoric-utilities.lisp")
(load "symbolic-utilities.lisp")
(load "continuations.lisp")

;;  _          _
;; | |_ ___ __| |_ ___ _ __  __ _ _ __ _ __  ___ _ _ ___
;; |  _/ -_|_-<  _|___| '  \/ _` | '_ \ '_ \/ -_) '_(_-<
;;  \__\___/__/\__|   |_|_|_\__,_| .__/ .__/\___|_| /__/
;;                               |_|  |_|

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

    (equal '(1 2 3 4) (-> 1 (list 2) (list 3) (list 4) (flatten)))
    ))

;;  _          _                           _
;; | |_ ___ __| |_ ___ __ _ _ _  __ _ _ __| |_  ___ _ _ __ _
;; |  _/ -_|_-<  _|___/ _` | ' \/ _` | '_ \ ' \/ _ \ '_/ _` |
;;  \__\___/__/\__|   \__,_|_||_\__,_| .__/_||_\___/_| \__,_|
;;                                   |_|

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

;;  _          _                     _         _ _
;; | |_ ___ __| |_ ___ ____  _ _ __ | |__  ___| (_)__ ___
;; |  _/ -_|_-<  _|___(_-< || | '  \| '_ \/ _ \ | / _(_-<
;;  \__\___/__/\__|   /__/\_, |_|_|_|_.__/\___/_|_\__/__/
;;                        |__/

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

;;  _          _        __              _   _
;; | |_ ___ __| |_ ___ / _|_  _ _ _  __| |_(_)___ _ _  ___
;; |  _/ -_|_-<  _|___|  _| || | ' \/ _|  _| / _ \ ' \(_-<
;;  \__\___/__/\__|   |_|  \_,_|_||_\__|\__|_\___/_||_/__/

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

    (let ((xs '(1 2 3)))
      (equal
       (reverse xs)
       (reduce (flip cons) xs :initial-value ())
       ))

    (equal nil (reduce (fn cons) () :initial-value ()))

    (equal nil (reduce (flip cons) () :initial-value ()))

    (equal '(((nil . 1) . 2) . 3)
           (reduce (fn cons) '(1 2 3) :initial-value ()))
    ))

;;  _          _        __ _ _         _
;; | |_ ___ __| |_ ___ / _(_) |___ ___(_)___
;; |  _/ -_|_-<  _|___|  _| | / -_)___| / _ \
;;  \__\___/__/\__|   |_| |_|_\___|   |_\___/

(deftest test-file-io ()
  (check
    (equal "Hello, World!"
           (with-open-file (in "foobar.txt")
             (read-line in)))
    
    (equal "Hello, World!"
           (let ((in (open "foobar.txt")))
             (let ((val (read-line in)))
               (close in)
               val)))
    (equal nil
           (let ((in (open "/dev/garbage/thisdoesnotexist.txt"
                           :if-does-not-exist nil)))
             (when in
               (format t "~a~%" (read-line in))
               (close in))
             in))
    
    (equal 42
           (let ((in (open "foobar.txt" :if-does-not-exist nil)))
             (when in
               (loop for line = (read-line in nil)
                  while line do (format t "~a~%" line))
               (close in)
               42)))
    ))

;;  _          _                  _   _               _   _
;; | |_ ___ __| |_ ___ __ ___ _ _| |_(_)_ _ _  _ __ _| |_(_)___ _ _  ___
;; |  _/ -_|_-<  _|___/ _/ _ \ ' \  _| | ' \ || / _` |  _| / _ \ ' \(_-<
;;  \__\___/__/\__|   \__\___/_||_\__|_|_||_\_,_\__,_|\__|_\___/_||_/__/

;;; Continuations generates a warning because of a global, non-special
;;; variable, "currentcontinuation." The compiler in slime terminates on
;;; this warning. To run these tests, do
;;;
;;; slime-load-file
;;;
;;; and not C-c C-k (compile & load file)

(=defun add1 (n) (=values (1+ n)))

(=defun message ()
  (=values 'hello 'world))

(=defun baz ()
  (=bind (m n) (message)
    (=values (list m n))))
            
(deftest test-continuations ()
  (check
    (= 42 (=values (1+ 41)))
    (= 42 (add1 41))
    (= 43 (add1 42))
    (equal (baz) '(hello world))
    ))

;;  _          _                _
;; | |_ ___ __| |_   _ __  __ _(_)_ _
;; |  _/ -_|_-<  _| | '  \/ _` | | ' \
;;  \__\___/__/\__| |_|_|_\__,_|_|_||_|

(defun test-utilities ()
  (reset-test-statistics)
  (let ((result (combine-results
                  (test-mappers)
                  (test-anaphora)
                  (test-symbolics)
                  (test-functions)
                  (test-file-io)
                  (test-continuations)
                  )))
    (report-test-statistics)
    result))

(test-utilities)
