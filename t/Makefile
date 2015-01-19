
.PHONY: clean
all: malloc spendtime exit0 exit1 dup dup2

%: %.c
	gcc $< -o $@

clean:
	git clean -xf
