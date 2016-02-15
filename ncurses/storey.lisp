;;; A storey is a matrix of /tiles/ or /cells/ (the two words are synonyms).

(defconstant +storey-width+  256)
(defconstant +storey-height+ 256)

(defun make-storey ()
  (make-array `(,+world-width+
                ,+world-height+)
              :element-type '(signed-byte 16)))

(defmethod put-room ((r room)))
