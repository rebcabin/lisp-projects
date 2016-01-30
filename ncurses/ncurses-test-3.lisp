(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-ncurses)

(in-package :cl-ncurses)

(initscr)
(noecho)
(curs-set 0)

(let ((y 0))
 (loop for x from 0 to 99 do
   (progn
     (clear)
     (mvprintw y x "o")
     (refresh)
     (sleep 1))))

