# -*- Makefile -*-
SOURCES ?= $(wildcard *.c)
OBJECTS = $(SOURCES:.c=.o)
CFLAGS = -fPIC

.PHONY: all
all: $(TARGET)

dll%.so: $(OBJECTS)
	ocamlmklib -o $* $+

.PHONY: clean
clean:
	rm -f *.o *.so *.a
