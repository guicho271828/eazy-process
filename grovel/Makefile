
.SECONDLY: 
.PHONY: all clean

include_dir = /usr/include/

std 	= signal.h std*.h sys/*.h

headers = $(shell cd $(include_dir) ; ls $(std))

$(info $(headers))
# signal.h sys/types.h sys/signalfd.h

all: $(patsubst %.h,%.lisp,$(headers))

%.lisp: shared.h swig.sh
	-./swig.sh $@

clean:
	git clean -xf ./*
