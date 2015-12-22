#!/bin/sh

./configure --prefix=/usr --enable-shared=yes --enable-libccd --with-box-cylinder=libccd --disable-demos

make -j 
