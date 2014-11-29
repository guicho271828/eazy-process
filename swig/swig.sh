#!/bin/bash

locate-header (){
    echo "#include <$1>" | cpp -H -o /dev/null 2>&1 | head -n1 | cut -d ' ' -f 2
}

name=${1%.*}

mkdir -p $(dirname $name)
echo '%feature("export");' > $name.expanded
cat shared.h $(locate-header $name.h) | cpp -E >> $name.expanded
swig -cffi -module ${name##*/} $name.expanded 2> $name.err
# swig -cffi -o $name.lispbody -module ${name##*/} $name.expanded 2> $name.err
cat $name.err >&2

mv $name.lisp $name.lispbody
cat > $name.lisp <<EOF
(in-package :eazy-process.swig)
$(cat $name.lispbody)
EOF


showlines (){
    head -n $(($2-1)) $1 | tail -n 3
}

if [[ $(grep Error $name.err | wc -l | cut -d' ' -f 1) -gt 0 ]]
then
    showlines $name.expanded $(grep Error $name.err | cut -d: -f2)
    rm $name.lisp
else
# comment out if debug is required
    rm $name.expanded
    rm $name.err
fi
rm $name.lispbody

