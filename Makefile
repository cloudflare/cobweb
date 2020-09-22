export DOCKER_BUILDKIT=0
DEPS_OUT = $(PWD)/dist
CFLAGS=-I$(DEPS_OUT)/include -L$(DEPS_OUT)/lib

all: $(DEPS_OUT)/lib/libgmp.a $(DEPS_OUT)/lib/libcob.a

$(DEPS_OUT)/lib/libgmp.a:
	cd ./deps/gmp.js && \
		emconfigure ./configure --disable-assembly --host none --build none \
		--enable-cxx \
		--prefix=$(DEPS_OUT)
	make -C ./deps/gmp.js
	make -C ./deps/gmp.js install

$(DEPS_OUT)/lib/libcob.a: $(DEPS_OUT)/lib/libgmp.a
	cd ./deps/gnucobol-2.2 && \
		emconfigure ./configure --host none --build none \
		--without-db CFLAGS="$(CFLAGS)" \
		--disable-assembly --with-gmp=$(DEPS_OUT) --prefix=$(DEPS_OUT)
	make -C ./deps/gnucobol-2.2 CFLAGS="$(CFLAGS)" libcob
	make -C ./deps/gnucobol-2.2/libcob install
	cp ./deps/gnucobol-2.2/libcob.h $(DEPS_OUT)/include

clean:
	make -C ./deps/gmp.js clean || true
	make -C ./deps/gnucobol-2.2 clean || true
	rm -rf deps/gnucobol-2.2/cobc/*.o
	rm -rf deps/gnucobol-2.2/libcob/.deps
	rm -rf deps/gnucobol-2.2/config.log

build-docker:
	docker build -t xtuc/cobweb .
