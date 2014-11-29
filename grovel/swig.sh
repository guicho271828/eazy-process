#!/bin/bash

locate-header (){
    echo "#include <$1>" | cpp -H -o /dev/null 2>&1 | head -n1 | cut -d ' ' -f 2
}

name=${1%.*}
echo $name

cat shared.h $(locate-header $name.h) | cpp -E > $name.expanded
swig -cffi -o $name.lisp -module $name $name.expanded
