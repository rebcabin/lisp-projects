;;; An m-room has a bounding box that delimits its world coordinates. It's
;;; a submatrix of the world array that contains objects such as walls and
;;; floor tiles (we can't use the word "room" because it's already part of
;;; the cl package).

(defstruct m-room
  its-storey
  its-box)

(defmethod create-m-room ((bb box) (st storey))
  (make-m-room :its-box    bb
               :its-storey st))

(defmethod room-box-to-its-storey-writer-maker ((r m-room))
  (let* ((b (m-room-its-box    r))
         (s (m-room-its-storey r))
         (m (storey-matrix s)))
    (lambda (x y c)
      (setf (aref m x y) c)
      )))

(defmethod put-room ((r m-room))
  (let* ((f (room-box-to-its-storey-writer-maker r))
         (bw (m-room-its-box r))
         (bb (storey-bounding-box (m-room-its-storey r))))
    (draw-box :bounding-box bb
              :box-to-draw  bw
              :glyph-fn (basic-glypher +regular-wall-glyph+)
              :glyph-writer-fn f)))

