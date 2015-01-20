#!/bin/bash

sbcl=sbcl
ccl=ccl
ecl=ecl
abcl=abcl
cmucl=cmucl

$sbcl --eval "(progn (ql:quickload :eazy-process.test)(quit))"

# all fails, sigh

# $ccl --eval "(progn (ql:quickload :eazy-process.test)(quit))"
# $ecl -eval "(progn (ql:quickload :eazy-process.test)(quit))"
# $abcl --eval '(progn (ql:quickload :eazy-process.test)(quit))'
# $cmucl "--eval \"(progn (ql:quickload :eazy-process.test)(quit))\""

