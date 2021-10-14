#!/bin/sh

set -e

test `basename $PWD` != "c_src" && cd c_src

case "$1" in
  clean)
    rm -rf libbtc
    ;;

  *)
  	test -f libbtc/.libs/libbtc.so && exit 0

    (test -d libbtc || git clone https://github.com/libbtc/libbtc)

    (cd libbtc  &&  ./autogen.sh && ./configure --enable-module-recovery && make)
	#(cd libbtc &&  ./autogen.sh && ./configure --enable-module-recovery && make)
    ;;
esac
