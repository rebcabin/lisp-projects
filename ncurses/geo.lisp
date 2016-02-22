

(defclass geo ()
  ((render-char :initform +regular-floor-glyph+ :initarg :render-char)))

(defclass wall (geo) ())
(defclass door (geo) ())
(defclass rock (geo) ())
(defclass trap (geo) ())
