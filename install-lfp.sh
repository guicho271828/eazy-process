#!/bin/bash

# this is a script for running the build testing on travis-ci.

git clone --depth=1 https://github.com/sionescu/libfixposix.git ~/lfp

cd ~/lfp/
autoreconf -i -f
mkdir build
cd build
../configure
make
sudo make install
sudo ldconfig
