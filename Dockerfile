FROM ubuntu:18.04

SHELL ["/bin/bash", "-c"]

RUN apt-get update
RUN apt-get -y install git gcc libgmp-dev make python3 python3-distutils

WORKDIR /src
RUN git clone https://github.com/xtuc/cobaul.git .

# install cobc
WORKDIR deps/gnucobol-2.2/
RUN ./configure --without-db --prefix=/usr/
RUN make
RUN make install
RUN make clean

WORKDIR /src/deps/emsdk

# enable emscripten
RUN ./emsdk install latest
RUN ./emsdk activate latest

# source emsdk
ENV PATH /src/deps/emsdk:/src/deps/emsdk/node/12.9.1_64bit/bin:/src/deps/emsdk/upstream/emscripten:$PATH
ENV EMSDK /src/deps/emsdk
ENV EM_CONFIG /root/.emscripten

# build cobaul
WORKDIR /src
RUN make all

ENTRYPOINT ["./cobaul.sh"]
