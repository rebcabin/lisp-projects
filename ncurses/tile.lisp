(defclass tile ()
  ((geo       :accessor tile-geo       :initform nil :initarg :geo)
   (critter   :accessor tile-critter   :initform nil :initarg :critter)
   (treasures :accessor tile-treasures :initform nil :initarg :treasures)
   (lighted   :accessor tile-lighted   :initform nil :initarg :lighted
              )))
