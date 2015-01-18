#!/bin/bash

sbcl=sbcl
ccl=ccl

$sbcl --eval "(progn (ql:quickload :eazy-process.test)(quit))"
$ccl --eval "(progn (ql:quickload :eazy-process.test)(quit))"
