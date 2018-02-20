# Makefile for Serial CVL
#
# Set TOPCC to compiler+flags to override definitions below

# default in case TOPCC isn't defined
TOPCC = gcc -O -fstrength-reduce

CC = $(TOPCC)
CFLAGS = -DUNROLL_FACTOR=4

# we should be in ROOT/cvl/src/serial
ROOT = ../..

INCLUDEDIR = $(ROOT)/include
INCLUDE = cvl.h
LIB = libcvl.a

SRC = elwise.c vprims.c vecops.c facilt.c library.c rank.c
OBJ = elwise.o vprims.o vecops.o facilt.o library.o rank.o

$(LIB): $(OBJ)
	-rm $(LIB)
	ar cr $(LIB) $(OBJ)
	-ranlib $(LIB)

.c.o:
	$(CC) -I$(INCLUDEDIR) $(CFLAGS) -c -o $@ $*.c

clean:
	-rm -f $(OBJ) $(LIB)

$(OBJ):	defins.h $(INCLUDEDIR)/$(INCLUDE)
