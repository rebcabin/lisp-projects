(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-charms)
(ql:quickload :defenum)
(ql:quickload :alexandria)
(ql:quickload :hash-set)

(defpackage #:card-trainer
  (:shadow "enum")
  (:use #:cl #:charms #:defenum))

(in-package #:card-trainer)

(defconstant +nsuits+ 4)
(defconstant +npips+ 13)

(defparameter *suits* #(:S :H :D :C))
(defparameter *pips*  #(:A :2 :3 :4 :5 :6 :7 :8 :9 :T :J :Q :K))

(defun flatten (tree &key (levels 1000000000000))
  "Like Wolfram's _flatten_, takes an optional _levels_ argument and flattens
only that many levels out of the given _tree_ argument. _Levels_ defaults to
a practical infinity, causing _flatten_ to produce a fully flattened list."
  (let ((result '()))
    (labels ((scan (item level)
               (if (and (> level 0) (listp item))
                   (map nil (lambda (i) (scan i (- level 1))) item)
                   (push item result))))
      ;; Must add one level to the input levels because the recursive map strips
      ;; off one level of list-ness. Consider (flatten '((1)) :levels 1). "Item"
      ;; is '((1)) in the body of "scan." The branch executed is "(map nil
      ;; (lambda (i) (scan i (- level 1))) item)," so the first thing handed to
      ;; the lambda is the car of item, namely '(1). If "level" were 1, we would
      ;; execute the other branch of the scan in the next level down and would
      ;; push '(1) into the result instead of pushing 1.
      (scan tree (+ 1 levels)))
    (nreverse result)))

(defun riffle-lists (l1 l2)
  (flatten (map 'list #'list l1 l2) :levels 1))

;;                 _ _
;;  __ __ _ _ _ __| | |_____ _  _
;; / _/ _` | '_/ _` | / / -_) || |
;; \__\__,_|_| \__,_|_\_\___|\_, |
;;                           |__/

(defstruct cardkey peg oldpeg num pip npeg nato heb monday)

(defparameter *cardkey-column-names*
  (mapcar #'string-upcase '("peg" "oldpeg" "num" "pip" "npeg" "nato" "heb" "monday")))

(defparameter *cardkey-column-keywords*
  (mapcar #'alexandria:make-keyword *cardkey-column-names*))

(defun create-cardkey (lyst)
  (apply #'make-cardkey (riffle-lists *cardkey-column-keywords* lyst)))

(defparameter *keys*
  ;; peg          oldpeg       num pip  npeg     nato       heb       monday
  #(("jail"        "bat"         1 "SA" "TEA"    "ALPHA"    "ARYEH"   "JAN 01")
    ("judge"       "bean"        2 "S2" "NOAH"   "BAKER"    "BAYIT"   "JAN 08")
    ("chalk"       "beam"        3 "S3" "ME"     "CHARLIE"  "GAMAL"   "JAN 15")
    ("chef"        "bar"         4 "S4" "RAY"    "DELTA"    "DALET"   "JAN 22")
    ("chip"        "ball"        5 "S5" "LAW"    "ECHO"     "HAR"     "JAN 29")
    ("case"        "badge"       6 "S6" "JAW"    "FOXTROT"  "VERED"   "FEB 05")
    ("cat"         "back"        7 "S7" "KEY"    "GOLF"     "ZAHAV"   "FEB 12")
    ("can"         "beef"        8 "S8" "FEE"    "HOTEL"    "CHAMOR"  "FEB 19")
    ("cam"         "pipe"        9 "S9" "PEA"    "INDIA"    "TAYAS"   "FEB 26")
    ("car"         "base"       10 "ST" "TOES"   "JULIET"   "YAD"     "MAR 05")
    ("coal"        "potato"     11 "SJ" "TOT"    "KILO"     "KISEH"   "MAR 12")
    ("cage"        "baton"      12 "SQ" "TAN"    "LIMA"     "LECHEM"  "MAR 19")
    ("cake"        "podium"     13 "SK" "TAM"    "MIKE"     "MAYIM"   "MAR 26")

    ("cuff"        "rat"        14 "HA" "TAR"    "NOVEMBER" "NACHASH" "APR 02")
    ("cap"         "rain"       15 "H2" "TAIL"   "OSCAR"    "SEFER"   "APR 09")
    ("face"        "ram"        16 "H3" "TISSUE" "PAPA"     "AYIN"    "APR 16")
    ("fat"         "rear"       17 "H4" "TACK"   "QUEBEC"   "PANIM"   "APR 23")
    ("fan"         "rail"       18 "H5" "TAFFY"  "ROMEO"    "TZIPPOR" "APR 30")
    ("fame"        "rash"       19 "H6" "TAP"    "SIERRA"   "QUF"     "MAY 07")
    ("fare"        "rack"       20 "H7" "NOSE"   "TANGO"    "ROSH"    "MAY 14")
    ("fall"        "reef"       21 "H8" "NET"    "UNIFORM"  "SHAD"    "MAY 21")
    ("fish"        "rope"       22 "H9" "NUN"    "VICTOR"   "TANIN"   "MAY 28")
    ("fig"         "race"       23 "HT" "NAME"   "WHISKEY"  ""        "JUN 04")
    ("fife"        "rat-a-tat"  24 "HJ" "NERO"   "X-RAY"    ""        "JUN 11")
    ("fob"         "rattan"     25 "HQ" "NAIL"   "YANKEE"   ""        "JUN 18")
    ("bat"         "radium"     26 "HK" "NICHE"  "ZULU"     ""        "JUN 25")

    ("back"        "cat"        27 "CA" "tea"    "alpha"    "aryeh"   "JUL 02")
    ("beef"        "can"        28 "C2" "noah"   "baker"    "bayit"   "JUL 09")
    ("pipe"        "cam"        29 "C3" "me"     "charlie"  "gamal"   "JUL 16")
    ("thesis"      "car"        30 "C4" "ray"    "delta"    "dalet"   "JUL 23")
    ("toast"       "coal"       31 "C5" "law"    "echo"     "har"     "JUL 30")
    ("dozen"       "cage"       32 "C6" "jaw"    "foxtrot"  "vered"   "AUG 06")
    ("twosome"     "cake"       33 "C7" "key"    "golf"     "zahav"   "AUG 13")
    ("dowser"      "cuff"       34 "C8" "fee"    "hotel"    "chamor"  "AUG 20")
    ("diesel"      "cap"        35 "C9" "pea"    "india"    "tayas"   "AUG 27")
    ("wood-sage"   "case"       36 "CT" "toes"   "juliet"   "yad"     "SEP 03")
    ("tusk"        "cadet"      37 "CJ" "tot"    "kilo"     "kiseh"   "SEP 10")
    ("adhesive"    "katana"     38 "CQ" "tan"    "lima"     "lechem"  "SEP 17")
    ("teaspoon"    "catamaran"  39 "CK" "tam"    "mike"     "mayim"   "SEP 24")

    ("tights"      "tot"        40 "DA" "tar"    "november" "nachash" "OCT 01")
    ("teetotaler"  "tan"        41 "D2" "tail"   "oscar"    "sefer"   "OCT 08")
    ("titan"       "tam"        42 "D3" "tissue" "papa"     "ayin"    "OCT 15")
    ("totem"       "tar"        43 "D4" "tack"   "quebec"   "panim"   "OCT 22")
    ("tatar"       "tail"       44 "D5" "taffy"  "romeo"    "tzippor" "OCT 29")
    ("title"       "tissue"     45 "D6" "tap"    "sierra"   "quf"     "NOV 05")
    ("death-watch" "tack"       46 "D7" "nose"   "tango"    "rosh"    "NOV 12")
    ("hot-dog"     "taffy"      47 "D8" "net"    "uniform"  "shad"    "NOV 19")
    ("auto-da-fe"  "tap"        48 "D9" "nun"    "victor"   "tanin"   "NOV 26")
    ("teddy-bear"  "toes"       49 "DT" "name"   "whiskey"  ""        "DEC 03")
    ("dance"       "teetotaler" 50 "DJ" "nero"   "x-ray"    ""        "DEC 10")
    ("doughnut"    "titan"      51 "DQ" "nail"   "yankee"   ""        "DEC 17")
    ("athenian"    "totem"      52 "DK" "niche"  "zulu"     ""        "DEC 24")
    ))

(defparameter *cardkeys*
  (map 'list #'create-cardkey *keys*))

;;                 _     _            _
;;  __ __ _ _ _ __| |___| |_  __ _ __| |_
;; / _/ _` | '_/ _` |___| ' \/ _` (_-< ' \
;; \__\__,_|_| \__,_|   |_||_\__,_/__/_||_|

(defparameter *cardhash*
  (make-hash-table :test #'equal))

(defun install-cardkey (ck)
  (setf (gethash (cardkey-pip ck)
                 *cardhash*)
        ck))

(map nil #'install-cardkey *cardkeys*)

;;     _        _
;;  __| |___ __| |__
;; / _` / -_) _| / /
;; \__,_\___\__|_\_\

(defparameter *deck*
  (let ((d (make-array (* +nsuits+ +npips+) :element-type 'string)))
    (do ((s 0 (1+ s)))
        ((<= +nsuits+ s))
      (do ((p 0 (1+ p)))
          ((<= +npips+ p))
        (setf (aref d (+ p (* s +npips+)))
              (concatenate 'string
                           (format nil "~A" (aref *suits* s))
                           (format nil "~A" (aref *pips*  p))))))
    d))

;;; From https://goo.gl/8fCKZL
(defun nshuffle-array (array)
  (let ((*random-state* (make-random-state t)))
   (loop for i from (length array) downto 2
         do (rotatef (aref array (random i))
                     (aref array (1- i)))
         finally (return array))))

(print *deck*)
(print (nshuffle-array *deck*)) ;; *deck* is modified in-place

;;     _       _
;;  __| |_ _ _(_)_ _  __ _ ___
;; (_-<  _| '_| | ' \/ _` (_-<
;; /__/\__|_| |_|_||_\__, /__/
;;                   |___/

(defun string-builder ()
  (make-array '(0) :element-type 'base-char
                   :fill-pointer 0 :adjustable t))

;; (print
;;  (with-output-to-string (sb nil)
;;    (do ((s 0 (1+ s)))
;;        ((<= +nsuits+ s))
;;      (format sb "~&")
;;      (do ((p 0 (1+ p)))
;;          ((<= +npips+ p))
;;        (format sb "~A~A " (aref *suits* s) (aref *pips* p))))))

;; (print
;;  (let ((str (string-builder)))
;;    (with-output-to-string (sb str)
;;      (do ((s 0 (1+ s)))
;;          ((<= +nsuits+ s))
;;        (format sb "~&")
;;        (do ((p 0 (1+ p)))
;;            ((<= +npips+ p))
;;          (format sb "~A~A " (aref *suits* s) (aref *pips* p))))
;;      str)))

;; (print
;;  (let ((sb (make-string-output-stream)))
;;    (do ((s 0 (1+ s)))
;;        ((<= +nsuits+ s))
;;      (format sb "~&")
;;      (do ((p 0 (1+ p)))
;;          ((<= +npips+ p))
;;        (format sb "~A~A " (aref *suits* s) (aref *pips* p))))
;;    (get-output-stream-string sb)))

;;  _ _ _   _   _          __
;; | (_) |_| |_| |___ ___ / _|____ __
;; | | |  _|  _| / -_)___|  _(_-< '  \
;; |_|_|\__|\__|_\___|   |_| /__/_|_|_|

(defstruct fsm-state nym action)

(defparameter *current-state* :deal-card)

;;                     _     _            __
;;  _  _ ___ ___ _ _  (_)_ _| |_ ___ _ _ / _|__ _ __ ___
;; | || (_-</ -_) '_| | | ' \  _/ -_) '_|  _/ _` / _/ -_)
;;  \_,_/__/\___|_|   |_|_||_\__\___|_| |_| \__,_\__\___|

(defun control-process (c)
  (case c
    ((nil) nil)
    ((#\c)           t)
    ((#\q #\Q #\Esc) t)))

(defmacro with-loop-frame (loop-name &rest body)
  `(progn
     ;; Because we're in non-blocking mode, get-char returns constantly,
     ;; even when no key has been pressed. Must always check
     ;; last-non-nil-c instead of the return value of get-char.
     (setf last-non-nil-c (or c last-non-nil-c))
     (clear-window *standard-window*)
     ,@body
     (refresh-window *standard-window*)
     (when (control-process c) (return-from ,loop-name))
     ))

(defmethod dump ((w window) (thing string) x y)
  (write-string-at-point w thing x y))

(defun dumpc (thing x y)
  (dump *standard-window* (format nil "~A ~A" thing (char-code thing)) x y))

(defun set-up-input ()
  (disable-echoing)
  (charms/ll:curs-set 0)
  (enable-extra-keys *standard-window*)
  (enable-raw-input :interpret-control-characters t)
  (enable-non-blocking-mode *standard-window*))

(defun main ()
  (let ((last-non-nil-c #\-))
    (with-curses ()
      (set-up-input)
      (loop :named driver-loop
            :for c := (get-char *standard-window* :ignore-error t)
            :do (with-loop-frame driver-loop
                  (dumpc last-non-nil-c 2 5))))))

(main)
