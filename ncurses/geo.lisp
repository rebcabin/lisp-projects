

(defclass geo ()
  ((render-char :initform #\. :initarg :render-char)))

(defclass wall (geo) ())
(defclass door (geo) ())
(defclass rock (geo) ())
(defclass trap (geo) ())
