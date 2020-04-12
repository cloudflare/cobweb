echo "bitcode ==> javascript"
~/Dev/emscripten/emcc -O2 test.c .libs/libgmp.a -o complete.js -s ASM_JS=1 -g --llvm-lto 1

