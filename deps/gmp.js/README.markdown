gmp.js
======

gmp.js is a port of the GNU Multiple-Precision Library (GMP), a library
for arbitrary precision arithmetic, to JavaScript using Emscripten.

GMP website: http://gmplib.org/


Steps to build
--------------

* First run configure and make natively/normally. You will need some
  of the generated executables. Optionally, also build ``test.c`` and see
  that it works (see instructions inside ``test.c``).

* Run configure using something like

        EMCONFIGURE_JS=1 emconfigure ./configure ABI=longlong --build=none --host=none

* Edit ``config.h`` and disable ``HAVE_QUAD_T``, ``HAVE_OBSTACK_VPRINTF``

* Run make using something like

        make -j 2

* Run ``emscripten.sh`` which will build the main test file, link it, then
  compile to JavaScript using Emscripten

* Run the code using something like

        node complete.js 500

