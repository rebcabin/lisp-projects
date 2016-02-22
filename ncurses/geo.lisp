(defclass geo ()
  ((render-char :initform +regular-floor-glyph+ :initarg :render-char)))

(defclass wall (geo) ())
(defmethod initialize-instance
    :after
    ((w wall)
     &key (render-char +regular-wall-glyph+))
  (setf (slot-value w 'render-char)
        render-char))

(defclass door (geo) ())
(defclass rock (geo) ())
(defclass trap (geo) ())
