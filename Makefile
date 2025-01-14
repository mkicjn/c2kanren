CFLAGS=-Os -Wall -Wextra -pedantic
LDFLAGS=-lm

ALL=lisp lisp-small

all: $(ALL)

%: %.c
	$(CC) $(CFLAGS) $< -o $@

.PHONY: clean
clean:
	rm -f $(ALL)
