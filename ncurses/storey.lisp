;;; A storey is a matrix of /tiles/ or /cells/ (the two words are synonyms).

(defconstant +storey-width+  256)
(defconstant +storey-height+ 256)

(defun make-storey ()
  (make-array `(,+storey-width+
                ,+storey-height+)
              :element-type '(signed-byte 16)))

