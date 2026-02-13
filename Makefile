CFLAGS=-O3 -Wall -Wextra -pedantic

ALL=c2klisp

all: $(ALL)


.PHONY: clean
clean:
	rm -f $(ALL)
