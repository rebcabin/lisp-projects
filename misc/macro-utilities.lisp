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

