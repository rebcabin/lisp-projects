[This paper](https://www.lrde.epita.fr/~didier/research/verna.06.imecs.pdf) claims to make a certain Lisp program run faster than its C
equivalent. Trying to reproduce the results, I was able to get close (Lisp is
50% slower than C) but wanted to know if anyone knows ways to squeeze more
perf out of SBCL 1.3.1.

The target problem is adding a constant single float to every cell in an 800 x
800 array of single floats. The method is to write the program in C and in
Common Lisp and compare the times. Using this [portable timer](https://github.com/nclack/tictoc), The C code is as
follows:

    #include <stddef.h>
    #include <stdint.h>
    #include <stdio.h>
    #include <stdlib.h>
    #include <assert.h>
    #include <time.h>
    
    #include "./modules/tictoc/tictoc.h"
    
    const int HORZ = 800;
    const int VERT = 800;
    
    #define PERF_REPS 1000
    
    typedef float DATA_T;
    
    struct image_s {
        size_t n;
        size_t w, h;
        DATA_T * data;
    };
    typedef struct image_s image;
    
    image * make_image (size_t w, size_t h) {
        size_t n = w * h;
        DATA_T * data = (DATA_T *)malloc(sizeof(DATA_T) * n);
        assert (NULL != data);
        image * result = (image *)malloc(sizeof(image));
        assert (NULL != result);
        result->n = n;
        result->w = w;
        result->h = h;
        result->data = data;
        return result;
    }
    
    void free_image (image * it) {
        assert (NULL != it);
        assert (NULL != it->data);
        free (it->data);
        free (it);
    }
    
    image * init_to_value (image * it, DATA_T val) {
        assert (NULL != it);
        assert (NULL != it->data);
        size_t i;
        const size_t n = it->n;
        for (i = 0; i < n; ++i) {
            it->data[i] = val;
        }
        return it;
    }
    
    void add (image * to, image * from, DATA_T val) {
        assert (NULL != to);
        assert (NULL != to->data);
        assert (NULL != from);
        assert (NULL != from->data);
        size_t i;
        const size_t n = to->n;
        for (i = 0; i < n; ++i) {
            to->data[i] = from->data[i] + val;
        }
    }
    
    int main (int argc, char ** argv) {
        s_rand (42);
        image * from = init_to_value (make_image (HORZ, VERT), 0.0f);
        image * to = init_to_value (make_image (HORZ, VERT), 0.0f);
        TicTocTimer clock = tic();
        for (size_t i = 0; i < PERF_REPS; ++i)
            add (to, from, 42.0);
        printf("Elapsed time %f seconds.\n",toc(&clock));
        return 0;
    }

I compile and run the code as follows:

    gcc -O3 image-add.c ./modules/tictoc/libtictoc.a && ./a.out

A typical time on my mac book pro is about 0.178 seconds.  Pretty nice.

The equivalent Lisp code, using every option I was able to find in the Lisp
[hyperspec](http://www.lispworks.com/documentation/common-lisp.html), in the new book [Common Lisp Recipes](http://www.amazon.com/gp/product/1484211774/), and in the [SBCL user manual](http://www.sbcl.org/manual/), is
as follows.  The comments indicate some things I tried that did not make a
difference. 

    ;;; None of the commented-out declarations made any difference in speed. 
    
    (declaim (optimize speed (safety 0)))
    
    (defun image-add (to from val)
      (declare (type (simple-array single-float (*))
                     to from))
      (declare (type single-float val))
      (let ((size (array-dimension to 0)))
        ;(declare (type fixnum size))
        (dotimes (i size)
          ;(declare (type fixnum i))
          (setf (aref to i) (+ (aref from i) val)))))
    
    (defparameter HORZ 800)
    (defparameter VERT 800)
    
    (defparameter PERF-REPS 1000)
    
    (let ((to (make-array (* HORZ VERT) :element-type 'single-float))
          (fm (make-array (* HORZ VERT) :element-type 'single-float)))
      ;(declare (type fixnum HORZ))
      ;(declare (type fixnum VERT))
      (time (dotimes (_ PERF-REPS)
              ;(declare (type fixnum PERF-REPS))
              ;(declare (type fixnum _))
              ;(declare (inline image-add))
              (image-add to fm 42.0))))

I compile and run it as follows:

    sbcl --script image-perf.lisp

and typical run times are 0.276. Not bad, but I want it much better. Of course,
the whole point of the exercise is that the Lisp code is shorter, but does
anyone know a way to make it as fast or faster?
