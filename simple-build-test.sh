#!/bin/bash

which sbcl && sbcl --eval "(progn (ql:quickload :eazy-process.test)(quit))"

# all fails, sigh

which ccl && ccl --eval "(progn (ql:quickload :eazy-process.test)(quit))"

# $ecl -eval "(progn (ql:quickload :eazy-process.test)(quit))"
# $abcl --eval '(progn (ql:quickload :eazy-process.test)(quit))'
# $cmucl "--eval \"(progn (ql:quickload :eazy-process.test)(quit))\""

