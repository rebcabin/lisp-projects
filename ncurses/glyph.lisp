(defconstant +regular-wall-glyph+ #\%)
(defconstant +regular-me-glyph+   #\@)

(defmethod basic-glypher ((c character))
  "Given a character, produces a function of position and direction that can
produce glyphs. The function signature of a glypher is x, y, and direction.
Direction will be :straight or :swapped. If :straight, then the caller has given
x and y in order. If :swapped, caller has given x and y reversed. Some
algorithms, like Bresenham's, like to swap x and y."
  (lambda (x y direction)
    (declare (ignorable x y direction))
    c))


