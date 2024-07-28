CFLAGS=-O3 -Wall -Wextra -pedantic
LDFLAGS=-lm

ALL=a.out

a.out: lisp.c
	$(CC) $(CFLAGS) $< -o $@

.PHONY: clean
clean:
	rm -f $(ALL)
