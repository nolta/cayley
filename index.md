---
title: Cayley
layout: default
---

[Cayley](http://github.com/nolta/cayley)
========================================

"C + multidimensional pointers"

Example
-------

{% highlight c %}
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

Syntax
------

Declaration:

    complex float $vector;  // 1d slice
    double $$matrix;        // 2d slice

Contact
-------

Mike Nolta (mike@nolta.net)

Download
--------

You can download this project in either
[zip](http://github.com/nolta/cayley/zipball/master) or
[tar](http://github.com/nolta/cayley/tarball/master) formats.

You can also clone the project with [Git](http://git-scm.com)
by running:

    $ git clone git://github.com/nolta/cayley

