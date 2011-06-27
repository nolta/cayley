---
title: Cayley
layout: default
---

[Cayley](http://github.com/nolta/cayley)
========================================

Cayley is an experimental set of extensions to the C language, intended to
simplify numerical programming.

Slices, a.k.a. multidimensional pointers
----------------------------------------

Slices are declared much like pointers:

    complex float $vector;  // 1d slice
    double $$matrix;        // 2d slice

but allow for multiple indicies:

    c = vector[k];
    d = matrix[i,j];

Examples
--------

A safer version of strcpy:

    char $
    slicecpy( char $ dst, const char $ src )
    {
        unsigned i;
        for (i = 0; i < __slice_dim1 dst - 1; ++i)
        {
            if (src[i] != '\0')
                dst[i] = src[i];
            else
                break;
        }
        dst[i] = '\0';
        return dst;
    }

A simple matrix multiplier:

    void
    simple_matmul( double $$c, const double $$a, const double $$b )
    {
        unsigned m = __slice_dim1 c;
        unsigned n = __slice_dim2 c;
        unsigned o = __slice_dim2 a;
        assert(m == __slice_dim1 a);
        assert(n == __slice_dim2 b);
        assert(o == __slice_dim1 b);

        for (unsigned i = 0; i < m; ++i)
        for (unsigned j = 0; j < n; ++j)
        {
            c[i,j] = 0.;
            for (unsigned k = 0; k < o; ++k)
                c[i,j] += a[i,k]*b[k,j];
        }
    }

Compiler
--------

I've hacked up a version of [Clang](http://http://clang.llvm.org/), the LLVM
C/C++/Obj-C compiler, with support for slices.

1. Checkout LLVM:

    $ svn co http://llvm.org/svn/llvm-project/llvm/trunk llvm

2. Checkout Clang+Cayley:

    $ cd llvm/tools
    $ git clone git://github.com/nolta/cayley clang

3. Build:

    $ mkdir /path/to/llvm-build
    $ cd /path/to/llvm-build
    $ /path/to/llvm/configure
    $ make

Cayley source files have the extension `.cy`.
