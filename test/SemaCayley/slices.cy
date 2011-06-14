// RUN: %clang_cc1 -fsyntax-only -verify -pedantic %s

double $$$$$$$$ x; // expected-error {{slice rank cannot exceed 7}}

float $y;
float f = y[0,0]; // expected-error {{number of subscripts does not match dimension of slice}}


