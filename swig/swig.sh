#!/bin/bash

locate-header (){
    echo "#include <$1>" | cpp -H -o /dev/null 2>&1 | head -n1 | cut -d ' ' -f 2
}
q (){
    $@ &> /dev/null
}

name=${1%.*}

mkdir -p $(dirname $name)
cat > $name.expanded <<EOF
%feature("export");
EOF

for ig in $(cat ignored_list)
do
    echo "%ignore \"$ig\";" >> $name.expanded
done


    # grep -v "string.h" | \
    # grep -v "stdio.h" | \
    # grep -v "stddef.h" | \


cat shared.h $(locate-header $name.h) | \
    cpp -E | \
    grep -v "typedef long unsigned int size_t;" | \
    grep -v "typedef int wchar_t;" | \
    grep -v "strerror_r" | \
    grep -v "extern int fw\?scanf"  >> $name.expanded
swig -cffi -module ${name##*/} $name.expanded 2> $name.err
# swig -cffi -o $name.lispbody -module ${name##*/} $name.expanded 2> $name.err
cat $name.err >&2

q mv $name.lisp $name.lispbody
cat > $name.lisp <<EOF
(in-package :eazy-process.swig)
$(cat $name.lispbody)
EOF

showlines (){
    head -n $(($2+1)) $1 | tail -n 3
}

if [[ $(grep Error $name.err | wc -l | cut -d' ' -f 1) -gt 0 ]]
then
    showlines $name.expanded $(grep Error $name.err | cut -d: -f2)
    q rm $name.lisp
else
# comment out if debug is required
    q rm $name.err $name.expanded
fi
q rm $name.lispbody

