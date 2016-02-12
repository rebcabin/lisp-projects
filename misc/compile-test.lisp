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
  (if (null fs)
      f
      `(-> (insinuate ,f ,(car fs)) ,@(cdr fs))))

(defmacro ->> (f &rest fs)
  (if (null fs)
      f
      `(->> (extenuate ,f ,(car fs)) ,@(cdr fs))))

(->  42 (1+) (1-) (print))
(->> 42 (1+) (1-) (print))

