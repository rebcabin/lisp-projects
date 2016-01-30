gcc -O3 image-add.c modules/tictoc/libtictoc.a
./a.out
sbcl --script image-add.lisp
