(defclass m-box ()
  ((left   :accessor m-box-left   :initform 0 :type integer :initarg :left  )
   (top    :accessor m-box-top    :initform 0 :type integer :initarg :top   )
   (width  :accessor m-box-width  :initform 0 :type integer :initarg :width )
   (height :accessor m-box-height :initform 0 :type integer :initarg :height)))
(setf foo (make-instance 'm-box :left 42 :top -39 :width 5 :height  11))
(describe foo)

(defclass m-block (m-box) ())
(defmethod initialize-instance
    :around
    ((mb m-block)
     &key (left 0) (top 0))
  (call-next-method mb :left left :top top
                    :width 1 :height 1))
(setf foo (make-instance 'm-block :left 17 :top -34 :width 5 :height  11))
(describe foo)




;; (defstruct s-box
;;   (left   0 :type integer)
;;   (top    0 :type integer)
;;   (width  0 :type integer)
;;   (height 0 :type integer))
;; (setf foo (make-s-box
;;            :left    42
;;            :top    -39
;;            :width    5
;;            :height  11))
;; (describe foo)

;; (defstruct (s-block (:include s-box)))
;; (setf bar (make-s-block))
;; (describe bar)

