(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-charms)

;;; "Storeys" is a pun on "story" and the levels of a dungeon. We avoid the word
;;; "level" because it's ambiguous between the level of advancement of a
;;; character and the storey of the dungeon.

(defpackage #:charms-storeys
  (:use #:cl #:charms))

(in-package #:charms-storeys)

;;; L I B R A R I E S ==========================================================

(load "point.lisp")
(load "box.lisp")
(load "world.lisp")
(load "storey.lisp")
(load "room.lisp")
(load "rendering.lisp")

;;; D E B U G G I N G ==========================================================

(defparameter *dump-all* t)

(defmethod dump ((w window) (thing string) (dump-p point))
  (if *dump-all*
      (write-string-at-point w thing (point-x dump-p) (point-y dump-p))))

(defun dumpw (thing x y)
  (dump *standard-window* thing (make-point :x x :y y)))

;;; D I S P L A Y ==============================================================

(defconstant regular-wall-char #\%)
(defconstant regular-me-char   #\@)

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
    (make-instance 'box :top 0 :left 0 :width width :height height)))

(defparameter *window-box* nil)

(defun window-mid-point ()
  (box-midpoint *window-box*))

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
    ((#\u0102 #\j) (displace-confined *me-point* *window-box* :down ))
    ((#\u0103 #\k) (displace-confined *me-point* *window-box* :up   ))
    ((#\u0104 #\h) (displace-confined *me-point* *window-box* :left ))
    ((#\u0105 #\l) (displace-confined *me-point* *window-box* :right))
    ((#\y)
     (displace-confined *me-point* *window-box* :up)
     (displace-confined *me-point* *window-box* :left))
    ((#\b)
     (displace-confined *me-point* *window-box* :down)
     (displace-confined *me-point* *window-box* :left))
    ((#\u)
     (displace-confined *me-point* *window-box* :up)
     (displace-confined *me-point* *window-box* :right))
    ((#\n)
     (displace-confined *me-point* *window-box* :down)
     (displace-confined *me-point* *window-box* :right))

    (otherwise c)))

(defun control-process (c)
  (case c
    ((nil) nil)
    ((#\Space) (start/stop/clear))
    ((#\q #\Q) t)))

;;; R E N D E R I N G ==========================================================

(defun write-glyph (x y c)
  '(write-char-at-point *standard-window* c x y)
  ;; Must use low-level mvwaddch so we can write to the last position in the
  ;; screen. See https://manned.org/mvwaddch: "If scrollok is not enabled,
  ;; writing a character at the lower right margin succeeds. However, an error
  ;; is returned because it is not possible to wrap to a new line.
  ;; --> ignore errors here <--
  (charms/ll:mvwaddch (charms::window-pointer *standard-window*)
                      y x
                      (charms::character-to-c-char c)))

(defmethod basic-glypher ((c character))
  "Given a character, produces a function of position and direction that can
produce other characters."
  (lambda (x y direction)
    (declare (ignorable x y direction))
    c))

(defun render-me ()
  (write-glyph (point-x *me-point*)
               (point-y *me-point*)
               regular-me-char))

;;; M A I N ====================================================================

(defun a-sample-m-room ()
  (make-instance 'box :left 7 :top 7 :width 3 :height 3))

(defun a-clipped-m-room ()
  (make-instance 'box :left -7 :top -7 :width 20 :height 20))

(defmethod draw-m-room ((room box))
  (draw-box :bounding-box    *window-box*
            :box-to-draw     room
            :glyph-fn        (basic-glypher regular-wall-char)
            :glyph-writer-fn #'write-glyph))

(defun main ()
  (let ((last-non-nil-c #\-))
    (with-curses ()
      (setf *window-box* (window-box))
      (set-up-colors)
      (set-up-input)
      (set-up-characters)
      (loop :named driver-loop
            :for c := (get-char *standard-window* :ignore-error t)
            :do (progn
                  ;; Because we're in non-blocking mode, get-char returns constantly,
                  ;; even when no key has been pressed. Must always check
                  ;; last-non-nil-c instead of the return value of get-char.
                  (setf last-non-nil-c (or c last-non-nil-c))
                  ;; Would be nice to have a "with-window" macro for this pattern.
                  (clear-window *standard-window*)

                  (draw-line :bounding-box    *window-box*
                             :from-point      (make-point :x  7 :y  7)
                             :to-point        (make-point :x  7 :y  7)
                             :glyph-fn        (basic-glypher #\*)
                             :glyph-writer-fn #'write-glyph)

                  ;; rooms, items, and characters

                  (draw-m-room (a-clipped-m-room))

                  (draw-m-room (a-sample-m-room))

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

                  (render-me)

                  (refresh-window *standard-window*)

                  (when (control-process c)
                    (return-from driver-loop)) )))))

(main)
