(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-ncurses)

(in-package :cl-ncurses)

(let ((y 0)
      (x 0)
      (next-x 0)
      (delay (/ 30000.0 1000000.0))
      (direction 1))
  (initscr)
  (noecho)
  (curs-set 0)
  (let ((max-x (getmaxx *stdscr*))
        (max-y (getmaxy *stdscr*)))
    (mvprintw 0 0 (format "~A" max-x))
    (mvprintw 0 1 (format "~A" max-y))
    (endwin)))

