CFLAGS=-Os -Wall -Wextra -pedantic
LDFLAGS=-lm

ALL=lisp lisp-small

all: $(ALL)

lisp: lisp.c
	$(CC) $(CFLAGS) $< -o $@

lisp-small: lisp-small.c
	$(CC) $(CFLAGS) $< -o $@

.PHONY: clean
clean:
	rm -f $(ALL)
