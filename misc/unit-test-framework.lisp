;;            _ _       _          _      __             _
;;  _  _ _ _ (_) |_ ___| |_ ___ __| |_   / _|_ ____ __ _| |__
;; | || | ' \| |  _|___|  _/ -_|_-<  _| |  _| '  \ V  V / / /
;;  \_,_|_||_|_|\__|    \__\___/__/\__| |_| |_|_|_\_/\_/|_\_\

(compile-file "macro-utilities.lisp") (load "macro-utilities.fasl")

(defvar *test-name* nil)
(defvar *test-pass-count* 0)
(defvar *test-fail-count* 0)
(defvar *test-total-count* 0)

(defun reset-test-statistics ()
  (setf *test-pass-count* 0)
  (setf *test-fail-count* 0)
  (setf *test-total-count* 0))

(defun report-test-statistics ()
  (format t "total number of tests: ~5d~%" *test-total-count*)
  (format t "total passed tests:    ~5d~%" *test-pass-count*)
  (format t "total failed tests:    ~5d~%" *test-fail-count*))

;;; TODO: Add support for documentation strings.

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can combine-results
   of other test functions or use 'check' to run individual test cases."
  `(defun ,name ,parameters
    (let ((*test-name* (append *test-name* (list ',name))))
      ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case, reporting results by
side-effect. Calls \"combine-results.\""
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "A non-short-circuiting AND; combine the results (as booleans) of
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


