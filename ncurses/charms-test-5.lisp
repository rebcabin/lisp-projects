(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-charms)

;;; "Storeys" is a pun on "story" and the levels of a dungeon. We avoid the word
;;; "level" because it's ambiguous between the level of advancement of a
;;; character and the storey of the dungeon.

(defpackage #:charms-storeys
  (:use #:cl #:charms))

(in-package #:charms-storeys)

;;; D E B U G G I N G ==========================================================

(defparameter *dump-all* t)

;;; P O I N T ==================================================================

(defclass point ()
  ((x :accessor point-x
      :initform 0
      :initarg  :x)
   (y :accessor point-y
      :initform 0
      :initarg  :y)))

(defun point (x y)
  (make-instance 'point :x x :y y))

(defun zero-point ()
  (make-instance 'point :x 0 :y 0))

(defmethod decr-x ((p point))
  (point (1- (point-x p)) (point-y p)))

(defmethod decr-y ((p point))
  (point (point-x p) (1- (point-y p))))

(defmethod displace-left ((p point))
  (decf (point-x p))
  t)

(defmethod displace-right ((p point))
  (incf (point-x p))
  t)

(defmethod displace-up ((p point))
  (decf (point-y p))
  t)

(defmethod displace-down ((p point))
  (incf (point-y p))
  t)

;;; D I S P L A Y ==============================================================

(defconstant regular-wall-char #\#)
(defconstant regular-me-char   #\@)

(defmethod dump ((w window) (thing string) (dump-p point))
  (if *dump-all*
      (write-string-at-point w thing (point-x dump-p) (point-y dump-p))))

(defun dumpw (thing x y)
  (dump *standard-window* thing (point x y)))

;;; B O X ======================================================================

;;; We'll have a world-box that contains a window box that contains room-boxes.

(defclass box ()
  ((left
    :accessor box-left
    :initform 0
    :initarg  :left)
   (top
    :accessor box-top
    :initform 0
    :initarg  :top)
   (width
    :accessor box-width
    :initform 0
    :initarg  :width)
   (height
    :accessor box-height
    :initform 0
    :initarg  :height)))

(defmethod flip-about-diagonal-pinning-upper-left ((b box))
  (make-instance 'box
                 :left   (box-left   b)
                 :top    (box-top    b)
                 :width  (box-height b)
                 :height (box-width  b)))

(defmethod box-bottom ((b box))
  (+ (box-top b) (box-height b)))

(defmethod box-right ((b box))
  (+ (box-left b) (box-width b)))

(defmethod box-top-left ((b box))
  (make-instance 'point :x (box-left  b) :y (box-top    b)))

(defmethod box-top-right ((b box))
  (make-instance 'point :x (box-right b) :y (box-top    b)))

(defmethod box-bottom-left ((b box))
  (make-instance 'point :x (box-left  b) :y (box-bottom b)))

(defmethod box-bottom-right ((b box))
  (make-instance 'point :x (box-right b) :y (box-bottom b)))

(defmethod displace ((p point) (b box) direction)
  (case direction
    ((:left)  (let ((lb (box-left b))
                    (px (point-x  p)))
                (when (> (- px 1) lb) (displace-left p))))
    ((:up)    (let ((tb (box-top b))
                    (py (point-y  p)))
                (when (> (- py 1) tb) (displace-up p))))
    ((:right) (let ((rb (box-right b))
                    (px (point-x   p)))
                (when (< px (- rb 2)) (displace-right p))))
    ((:down)  (let ((bb (box-bottom b))
                   (py (point-y   p)))
               (when (< py (- bb 2)) (displace-down p))))
    ))

;;; T I M E R ==================================================================

(defvar *start* nil)
(defvar *stop* nil)

(defun start/stop/clear ()
  "Start, stop, and clear the timer successively."
  (cond
    (*stop*        (setf   *start* nil  *stop*  nil))
    ((not *start*) (setf   *stop*  nil  *start* (get-internal-real-time)))
    (t             (setf   *stop* (get-internal-real-time)))))

(defun time-elapsed ()
  "Compute the time elapsed since *START* (to *END* if set). If the timer hasn't
started, return NIL."
  (and *start*
       (/ (- (or *stop* (get-internal-real-time))
             *start*)
          internal-time-units-per-second)))

;;; W I N D O W ================================================================

(defun window-box ()
  (multiple-value-bind (width height)
      (window-dimensions *standard-window*)
    (make-instance 'box :top 0 :left 0
                   :width width :height height)))

(defparameter *window-box* nil)

(defmethod box-midpoint ((b box))
  (let* ((lf (box-left   b))
         (rt (box-right  b))
         (tp (box-top    b))
         (bt (box-bottom b))
         (mx (floor (- rt lf) 2))
         (my (floor (- bt tp) 2)))
    (make-instance 'point :x mx :y my)))

(defun window-mid-point ()
  (box-midpoint *window-box*))

;;; R E N D E R I N G ==========================================================

(load "rendering.lisp")

;;; S E T - U P ================================================================

(defun set-up-colors ()
  (charms/ll:start-color)
  (charms/ll:init-pair 1 charms/ll:COLOR_WHITE charms/ll:COLOR_BLUE)
  (charms/ll:init-pair 2 charms/ll:COLOR_WHITE charms/ll:COLOR_GREEN)
  (charms/ll:init-pair 3 charms/ll:COLOR_WHITE charms/ll:COLOR_RED)
  (charms/ll:init-pair 4 charms/ll:COLOR_WHITE charms/ll:COLOR_YELLOW)
  (charms/ll:wbkgdset (charms::window-pointer *standard-window*)
                      (charms/ll:COLOR-PAIR 1)))

(defun set-up-input ()
  (disable-echoing)
  (charms/ll:curs-set 0)
  (enable-extra-keys *standard-window*)
  (enable-raw-input :interpret-control-characters t)
  (enable-non-blocking-mode *standard-window*))

;;; E N T I T I E S ============================================================

(defparameter *me-point* (zero-point))

(defun set-up-characters ()
  (setf *me-point* (window-mid-point)))

;;; M O V E M E N T   A N D   C O N T R O L ====================================

;; down-arrow:  U+0102	Ă	Latin Capital Letter A with breve
;; up-arrow:    U+0103	ă	Latin Small Letter A with breve
;; left-arrow:  U+0104	Ą	Latin Capital Letter A with ogonek
;; right-arrow: U+0105	ą	Latin Small Letter A with ogonek

(defun char-command (c)
  (case c
    ((#\u0102 #\j) :down)
    ((#\u0103 #\k) :up)
    ((#\u0104 #\h) :left)
    ((#\u0105 #\l) :right)
    (otherwise c)))

(defun move-character (c)
  (case c
    ((#\u0102 #\j) (displace *me-point* *window-box* :down ))
    ((#\u0103 #\k) (displace *me-point* *window-box* :up   ))
    ((#\u0104 #\h) (displace *me-point* *window-box* :left ))
    ((#\u0105 #\l) (displace *me-point* *window-box* :right))
    ((#\y)
     (displace *me-point* *window-box* :up)
     (displace *me-point* *window-box* :left))
    ((#\b)
     (displace *me-point* *window-box* :down)
     (displace *me-point* *window-box* :left))
    ((#\u)
     (displace *me-point* *window-box* :up)
     (displace *me-point* *window-box* :right))
    ((#\n)
     (displace *me-point* *window-box* :down)
     (displace *me-point* *window-box* :right))

    (otherwise c)))

(defun control-process (c)
  (case c
    ((nil) nil)
    ((#\Space) (start/stop/clear))
    ((#\q #\Q) t)))

;;; R E N D E R I N G ==========================================================

(defun paint-screen ()
  (let* ((me-x (point-x *me-point*))
         (me-y (point-y *me-point*)))
    (write-char-at-point *standard-window*
                         regular-me-char me-x me-y)))

(defun write-glyph (x y c)
  '(write-char-at-point *standard-window* c x y)
  ;; Must use low-level mvwaddch so we can write to the last position in the
  ;; screen. See https://manned.org/mvwaddch: "If scrollok is not enabled,
  ;; writing a character at the lower right margin succeeds. However, an error
  ;; is returned because it is not possible to wrap to a new line.
  ;; --> ignore errors here <-
  (charms/ll:mvwaddch (charms::window-pointer *standard-window*)
                      y x
                      (charms::character-to-c-char c)))

;;; M A I N ====================================================================

(defun main ()
  (let ((last-non-nil-c #\-))
    (with-curses ()
      (setf *window-box* (window-box))
      (set-up-colors)
      (set-up-input)
      (set-up-characters)
      (loop :named driver-loop
            :for c := (get-char *standard-window* :ignore-error t)
            ;; Because we're in non-blocking mode, get-char returns constantly,
            ;; even when no key has been pressed. Must always check
            ;; last-non-nil-c instead of the return value of get-char.
            :do (progn
                  (setf last-non-nil-c (or c last-non-nil-c))

                  (clear-window *standard-window*)

                  (draw-line :bounding-box    *window-box*
                             :from-point      (make-instance 'point :x 48 :y 12)
                             :to-point        (make-instance 'point :x  7 :y  7)
                             :glyph-fn        (lambda (x y dir) (declare (ignorable x y dir)) #\.)
                             :glyph-writer-fn #'write-glyph)

                  ;; rooms, items, and characters
                  (draw *window-box* *window-box* regular-wall-char #'write-glyph)
                  (move-character c)

                  ;; debugging and HUD
                  (dumpw (format nil "~A" (char-command last-non-nil-c)) 2 3)
                  (dumpw (format nil "~A" last-non-nil-c)                2 2)
                  (dumpw (format nil "[~A|~A|~A] [~A|~A|~A]"
                                 (box-left *window-box*)
                                 (point-x *me-point*)
                                 (box-right *window-box*)
                                 (box-top *window-box*)
                                 (point-y *me-point*)
                                 (box-bottom *window-box*)) 2 4)

                  (paint-screen)
                  (refresh-window *standard-window*)

                  (when (control-process c)
                    (return-from driver-loop)) )))))

(main)
