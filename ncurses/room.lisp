;;; An m-room has a bounding box that delimits its world coordinates. It's
;;; a submatrix of the world array that contains objects such as walls and
;;; floor tiles (we can't use the word "room" because it's already part of
;;; the cl package).

(defstruct m-room
  its-box)

(defmethod create-m-room ((bb box))
  (make-m-room :its-box bb))
