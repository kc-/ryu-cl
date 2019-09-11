Work-in-progress implementation of Adams, Ulf: "Ryu: Fast Float-to-String
Conversion.", in Common Lisp.

This code is a port of the 32- and 64-bit float printer, for 64-bit machines,  of [the C version of Ryu]. The only major difference in terms of output is the formatting of numbers in the range 10^-3 ... 10^7, as specified in [CLHS 22.1.3.1.3].

The main functions are float-to-string, single-float-to-string, and
double-float-to-string.

A bare minimum of tests are provided in the ryu-cl/test package (ql:quickload
:ryu-cl/test).

[the C version of Ryu]: https://github.com/ulfjack/ryu
[CLHS 22.1.3.1.3]: http://www.lispworks.com/documentation/HyperSpec/Body/22_acac.htm