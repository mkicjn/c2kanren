CFLAGS=-O3 -Wall -Wextra -pedantic

ALL=c2klisp

all: $(ALL)

c2klisp-min.c: c2klisp.c c2klisp-min.diff
	cp $< $@
	patch < c2klisp-min.diff

c2klisp-pretty.c: c2klisp.c c2klisp-pretty.diff
	cp $< $@
	patch < c2klisp-pretty.diff

.PHONY: clean
clean:
	rm -f $(ALL)
