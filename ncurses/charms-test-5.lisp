(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-charms)

;;; "Storeys" is a pun on "story" and the levels of a dungeon. We avoid the word
;;; "level" because it's ambiguous between the level of advancement of a
;;; character and the storey of the dungeon.

(defpackage #:charms-storeys
  (:use :cl :charms))

(in-package :charms-storeys)

(defparameter *dump-all* t)

(defclass point ()
  ((x :accessor point-x
      :initform 0
      :initarg  :x)
   (y :accessor point-y
      :initform 0
      :initarg  :y)))

(defconstant regular-wall-str "#")
(defconstant regular-me-str   "@")

(defmethod dump ((w window) (thing string) (dump-p point))
  (if *dump-all*
      (write-string-at-point w thing (point-x dump-p) (point-y dump-p))))

(defun write-me (wscr y x)
  (mvwaddstr wscr y x "@"))

(defun dump-me (scr max-y c)
  (if *dump-all*
      (mvwaddstr scr (- max-y 5) 2 (format nil "~A, ~A" c (code-char c)))))

(defun dump-ncurses-symbols ()
  (if *dump-all*
      (do-external-symbols (s (find-package :cl-ncurses))
        (print s))))

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

;;; TODO: factor out the rendering surface into a class.

(defun move-me-1 (me-y me-x my-box))

(defvar *start* nil)
(defvar *stop* nil)

(defun start/stop/clear ()
  "Start, stop, and clear the timer successively."
  (cond
    (*stop*        (setf *start* nil
                         *stop* nil))
    ((not *start*) (setf *stop* nil
                         *start* (get-internal-real-time)))
    (t             (setf *stop* (get-internal-real-time)))))

(defun time-elapsed ()
  "Compute the time elapsed since *START* (to *END* if set). If the timer hasn't started, return NIL."
  (and *start*
       (/ (- (or *stop* (get-internal-real-time))
             *start*)
          internal-time-units-per-second)))

;;; Rendering function

(defun paint-time ()
  "Paint the elapsed time to the center of the screen."
  (multiple-value-bind (width height)
      (window-dimensions *standard-window*)
    (let* ((dt (time-elapsed))
           (printed-time (if dt
                             (format nil "~,2F" dt)
                             "Press [SPACE] to start/stop/clear; q to quit"))
           (length/2 (floor (length printed-time) 2))
           (half-width (- (floor width 2) length/2))
           (half-height (floor height 2)))
      (write-string-at-point *standard-window*
                             printed-time
                             half-width
                             half-height))))

;;; Main driver

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

;; down-arrow:  U+0102	Ă	Latin Capital Letter A with breve
;; up-arrow:    U+0103	ă	Latin Small Letter A with breve
;; left-arrow:  U+0104	Ą	Latin Capital Letter A with ogonek
;; right-arrow: U+0105	ą	Latin Small Letter A with ogonek

(defun main ()
  "Start the timer program."
  (let ((last-non-nil-c "--nothing-from-keyboad-yet--"))
    (with-curses ()
      (set-up-colors)
      (set-up-input)
      (loop :named driver-loop
            :for c := (get-char *standard-window* :ignore-error t)
            :do (progn
                  ;; Capture char state
                  (setf last-non-nil-c
                        (if (not (null c))
                            c last-non-nil-c))
                  ;; Redraw time
                  (clear-window *standard-window*)
                  (paint-time)
                  (dump *standard-window*
                        (format nil "~A"
                                (case last-non-nil-c
                                  ((\LATIN_SMALL_LETTER_A_WITH_OGONEK) "right-arrow")))
                        (make-instance 'point :x 2 :y 3))
                  (dump *standard-window*
                        (format nil "~A" last-non-nil-c)
                        (make-instance 'point :x 2 :y 2))
                  (refresh-window *standard-window*)
                  ;; Process input
                  (case c
                    ((nil) nil)
                    ((#\Space) (start/stop/clear))
                    ((#\q #\Q) (return-from driver-loop))) )))))

(main)
