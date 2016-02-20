;;; The world is be a sequence of storeys. Each storey is a matrix of cells or
;;; tiles. Tiles contain things. Every tile contains one thing of type "geo."
;;; The default geo is nil, which means "nothing but the floor." Other geos are
;;; walls, doors, rocks (e.g., granite, quartz). A tile may contain one or zero
;;; "critters." A critter is either "me" or a "monster." A tile may contain zero
;;; or more "items." Some items include "treasures" and "traps." A tile may be
;;; lit or dark


(defconstant +wall+  1)
(defconstant +floor+ 2)
(defconstant +me+    3)

(defconstant +least-monster-number+    1001)
(defconstant +greatest-monster-number+ 2000)

