#!/bin/bash

# this does not work actually, because the source code contains lots of 
# gcc extension and swig emit errors.

# I manually downloaded the source code of cgroup from sourceforge,
# removed all gcc extensions (mainly __thread) to get the output.

swig -I/usr/include -v -includeall -cpperraswarn -cffi libcgroup.swg
