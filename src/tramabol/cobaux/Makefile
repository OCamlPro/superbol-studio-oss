# -*- Makefile -*-
SOURCES ?= $(wildcard *.c)
OBJECTS = $(SOURCES:.c=.o)
CFLAGS = -fPIC

.PHONY: all
all: $(TARGET)

dll%.so: $(OBJECTS)
	ocamlmklib -o $* $+

%.o: %.c
	$(CC) -c $< -o $@ $(CFLAGS) $(CPPFLAGS)

.PHONY: clean
clean:
	rm -f *.o *.so *.a
