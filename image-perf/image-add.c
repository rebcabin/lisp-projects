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

void s_rand (unsigned seed) {
    if (0 == seed)
        srand (time (NULL));
    else
        srand (seed);
}

/* TODO: This function improperly 'knows' that DATA_T is a floating-point
   number. It should repsect a 'dispatch-on-type' discipline. */
DATA_T a_rand () {
    int r = rand ();
    DATA_T result = ((DATA_T) r) / ((DATA_T) RAND_MAX);
    return result;
}

image * init_to_rand (image * it) {
    assert (NULL != it);
    assert (NULL != it->data);
    size_t i;
    const size_t n = it->n;
    for (i = 0; i < n; ++i) {
        it->data[i] = a_rand ();
    }
    return it;
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
    free_image (from);
    free_image (to);
    return 0;
}
