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

(defstruct cardkey peg num pip npeg nato heb monday)

(defun flatten (tree &key (levels 1000000000000))
  (let ((result '()))
    (labels ((scan (item l)
               (if (and (> l 0) (listp item))
                   (map nil (lambda (i) (scan i (- l 1))) item)
                   (push item result))))
      (scan tree (+ 1 levels)))
    (nreverse result)))

(defun append-nulls (lst n)
  (if (< (length lst) n)
      (dotimes (i (- n (length lst)))
        (setq lst (append lst (list 'null)))))
  lst)

(defun riffle-lists (lst1 lst2)
  (let ((m (max (length lst1) (length lst2))))
    (flatten (mapcar (lambda (a b) (list a b))
                     (append-nulls lst1 m)
                     (append-nulls lst2 m)))))

(defun create-cardkey (lyst)
  (apply make-cardkey lyst))

(defparameter *keys*
  #(("jail"         1 "SA" "TEA"    "ALPHA"    "ARYEH"   "JAN 01")
    ("judge"        2 "S2" "NOAH"   "BAKER"    "BAYIT"   "JAN 08")
    ("chalk"        3 "S3" "ME"     "CHARLIE"  "GAMAL"   "JAN 15")
    ("chef"         4 "S4" "RAY"    "DELTA"    "DALET"   "JAN 22")
    ("chip"         5 "S5" "LAW"    "ECHO"     "HAR"     "JAN 29")
    ("case"         6 "S6" "JAW"    "FOXTROT"  "VERED"   "FEB 05")
    ("cat"          7 "S7" "KEY"    "GOLF"     "ZAHAV"   "FEB 12")
    ("can"          8 "S8" "FEE"    "HOTEL"    "CHAMOR"  "FEB 19")
    ("cam"          9 "S9" "PEA"    "INDIA"    "TAYAS"   "FEB 26")
    ("car"         10 "ST" "TOES"   "JULIET"   "YAD"     "MAR 05")
    ("coal"        11 "SJ" "TOT"    "KILO"     "KISEH"   "MAR 12")
    ("cage"        12 "SQ" "TAN"    "LIMA"     "LECHEM"  "MAR 19")
    ("cake"        13 "SK" "TAM"    "MIKE"     "MAYIM"   "MAR 26")

    ("cuff"        14 "HA" "TAR"    "NOVEMBER" "NACHASH" "APR 02")
    ("cap"         15 "H2" "TAIL"   "OSCAR"    "SEFER"   "APR 09")
    ("face"        16 "H3" "TISSUE" "PAPA"     "AYIN"    "APR 16")
    ("fat"         17 "H4" "TACK"   "QUEBEC"   "PANIM"   "APR 23")
    ("fan"         18 "H5" "TAFFY"  "ROMEO"    "TZIPPOR" "APR 30")
    ("fame"        19 "H6" "TAP"    "SIERRA"   "QUF"     "MAY 07")
    ("fare"        20 "H7" "NOSE"   "TANGO"    "ROSH"    "MAY 14")
    ("fall"        21 "H8" "NET"    "UNIFORM"  "SHAD"    "MAY 21")
    ("fish"        22 "H9" "NUN"    "VICTOR"   "TANIN"   "MAY 28")
    ("fig"         23 "HT" "NAME"   "WHISKEY"  ""        "JUN 04")
    ("fife"        24 "HJ" "NERO"   "X-RAY"    ""        "JUN 11")
    ("fob"         25 "HQ" "NAIL"   "YANKEE"   ""        "JUN 18")
    ("bat"         26 "HK" "NICHE"  "ZULU"     ""        "JUN 25")

    ("back"        27 "CA" "tea"    "alpha"    "aryeh"   "JUL 02")
    ("beef"        28 "C2" "noah"   "baker"    "bayit"   "JUL 09")
    ("pipe"        29 "C3" "me"     "charlie"  "gamal"   "JUL 16")
    ("thesis"      30 "C4" "ray"    "delta"    "dalet"   "JUL 23")
    ("toast"       31 "C5" "law"    "echo"     "har"     "JUL 30")
    ("dozen"       32 "C6" "jaw"    "foxtrot"  "vered"   "AUG 06")
    ("twosome"     33 "C7" "key"    "golf"     "zahav"   "AUG 13")
    ("dowser"      34 "C8" "fee"    "hotel"    "chamor"  "AUG 20")
    ("diesel"      35 "C9" "pea"    "india"    "tayas"   "AUG 27")
    ("wood-sage"   36 "CT" "toes"   "juliet"   "yad"     "SEP 03")
    ("tusk"        37 "CJ" "tot"    "kilo"     "kiseh"   "SEP 10")
    ("adhesive"    38 "CQ" "tan"    "lima"     "lechem"  "SEP 17")
    ("teaspoon"    39 "CK" "tam"    "mike"     "mayim"   "SEP 24")

    ("tights"      40 "DA" "tar"    "november" "nachash" "OCT 01")
    ("teetotaler"  41 "D2" "tail"   "oscar"    "sefer"   "OCT 08")
    ("titan"       42 "D3" "tissue" "papa"     "ayin"    "OCT 15")
    ("totem"       43 "D4" "tack"   "quebec"   "panim"   "OCT 22")
    ("tatar"       44 "D5" "taffy"  "romeo"    "tzippor" "OCT 29")
    ("title"       45 "D6" "tap"    "sierra"   "quf"     "NOV 05")
    ("death-watch" 46 "D7" "nose"   "tango"    "rosh"    "NOV 12")
    ("hot-dog"     47 "D8" "net"    "uniform"  "shad"    "NOV 19")
    ("auto-da-fe"  48 "D9" "nun"    "victor"   "tanin"   "NOV 26")
    ("teddy-bear"  49 "DT" "name"   "whiskey"  ""        "DEC 03")
    ("dance"       50 "DJ" "nero"   "x-ray"    ""        "DEC 10")
    ("doughnut"    51 "DQ" "nail"   "yankee"   ""        "DEC 17")
    ("athenian"    52 "DK" "niche"  "zulu"     ""        "DEC 24")
    ))

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

(defun string-builder ()
  (make-array '(0) :element-type 'base-char
                   :fill-pointer 0 :adjustable t))

(print
 (with-output-to-string (sb nil)
   (do ((s 0 (1+ s)))
       ((<= +nsuits+ s))
     (format sb "~&")
     (do ((p 0 (1+ p)))
         ((<= +npips+ p))
       (format sb "~A~A " (aref *suits* s) (aref *pips* p))))))

(print
 (let ((str (string-builder)))
   (with-output-to-string (sb str)
     (do ((s 0 (1+ s)))
         ((<= +nsuits+ s))
       (format sb "~&")
       (do ((p 0 (1+ p)))
           ((<= +npips+ p))
         (format sb "~A~A " (aref *suits* s) (aref *pips* p))))
     str)))

(print
 (let ((sb (make-string-output-stream)))
   (do ((s 0 (1+ s)))
       ((<= +nsuits+ s))
     (format sb "~&")
     (do ((p 0 (1+ p)))
         ((<= +npips+ p))
       (format sb "~A~A " (aref *suits* s) (aref *pips* p))))
   (get-output-stream-string sb)))
