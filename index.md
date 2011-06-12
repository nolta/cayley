---
title: Cayley
layout: default
---

[Cayley](http://github.com/nolta/cayley)
========================================

Cayley is an experimental superset of the C language.

Slices, a.k.a. multidimensional pointers
----------------------------------------

Slices are declared much like pointers:

    complex float $vector;  // 1d slice
    double $$matrix;        // 2d slice

but allow for multiple indicies:

    c = vector[k];
    d = matrix[i,j];

Example
-------

{% highlight c %}
//
// slice.cy
//

#include <stdio.h>
#include <stdlib.h>

void
print_matrix(dbl $$matrix)
{
    int n1 = __slice_dim1 matrix;
    int n2 = __slice_dim2 matrix;

    printf("print_matrix(d/[%d,%d])\n", n1, n2);

    for (int i = 0; i < n1; i++) {
        for (int j = 0; j != n2; j++) {
            printf("%5g", matrix[i,j]);
        }
        printf("\n");
    }
}

int
main(int argc, char *argv[])
{
    const int n1 = 3, n2 = 2;
    dbl *d, $$matrix;

    d = (dbl *) malloc(n1*n2*sizeof(dbl));
    for (int i = 0, e = n1*n2; i != e; ++i)
      d[i] = i;

    print_matrix(d/[n1,n2]);
    print_matrix(d/[n2,n1]);

    free(d);
}
{% endhighlight %}

Output:

    print_matrix(d/[3,2])
        0    1
        2    3
        4    5
    print_matrix(d/[2,3])
        0    1    2
        3    4    5

Contact
-------

Mike Nolta (mike@nolta.net)

Download
--------

    $ git clone git://github.com/nolta/cayley

