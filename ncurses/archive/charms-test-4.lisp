(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-charms)

(in-package :cl-ncurses)

(defparameter *dump-all* t)

(defun init-color-ncurses ()
  (start-color)
  (init-pair COLOR_BLACK   COLOR_BLACK   COLOR_BLACK)
  (init-pair COLOR_GREEN   COLOR_GREEN   COLOR_BLACK)
  (init-pair COLOR_RED     COLOR_RED     COLOR_BLACK)
  (init-pair COLOR_CYAN    COLOR_CYAN    COLOR_BLACK)
  (init-pair COLOR_WHITE   COLOR_WHITE   COLOR_BLACK)
  (init-pair COLOR_MAGENTA COLOR_MAGENTA COLOR_BLACK)
  (init-pair COLOR_BLUE    COLOR_BLUE    COLOR_BLACK)
  (init-pair COLOR_YELLOW  COLOR_YELLOW  COLOR_BLACK))

(defun dump-pos (wscr scr-y scr-x y x)
  (if *dump-all*
      (mvwaddstr wscr scr-y scr-x (format nil "~A, ~A" x y))))

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

(defmethod draw ((b box) (scr sb-alien-internals:alien-value))
  (mvwaddstr scr (box-top b) (box-left b) "#")
  )

(defun render-screen (scr storey max-y max-x me-y me-x c)
  (erase)
  (draw storey scr)
  (dump-pos scr (- max-y 4) 2 max-x max-y)
  (write-me scr me-y me-x)
  (dump-me scr max-y c)
  (wrefresh scr))

(defun setup-screen (scr)
  (init-color-ncurses)
  (curs-set 0)
  (init-pair 1 COLOR_WHITE COLOR_BLUE)
  (init-pair 2 COLOR_WHITE COLOR_GREEN)
  (init-pair 3 COLOR_WHITE COLOR_RED)
  (init-pair 4 COLOR_WHITE COLOR_YELLOW)
  (erase)
  (bkgd (COLOR-PAIR 1))
  (box scr 0 0)
  (attron WA_BOLD)
  (keypad scr 1)
  (noecho))

(defun teardown-screen (scr max-y)
  (mvwaddstr scr (- max-y 2) 2 "Press any key to exit.")
  (wrefresh scr)
  (echo)
  (keypad scr 0)
  (wgetch scr))

(defun move-me-1 (me-y me-x my-box))

(defun run-screen ()
    (let ((y 0)
          (x 0)
          (me-x 0)
          (me-y 0)
          (probe-x 0)
          (delay (/ 30000.0 1000000.0))
          (direction 1)
          (scr (initscr))
          (title "ping-ponging..."))
      (setup-screen scr)
      (let* ((max-y  (getmaxy *stdscr*))
             (max-x  (getmaxx *stdscr*))
             (me-y   (floor (/ max-y 2)))
             (me-x   (floor (/ max-x 2)))
             (storey (make-instance 'box :left 0 :top 0
                                         :width max-x :height max-y)))
        (render-screen scr storey max-y max-x me-y me-x 0)
        (loop for n from 1 to 10 do
          (let ((c (getch)))
            (render-screen scr storey max-y max-x me-y me-x c)))
        (teardown-screen scr max-y))
      (endwin)))

(run-screen)
