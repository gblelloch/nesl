# Makefile for CM-2 CVL
#
# Set TOPCC to compiler+flags to override definition below

# default in case TOPCC isn't defined
TOPCC =	gcc -O

CC = $(TOPCC)

INCLUDEDIR = ../../include
INCLUDE = cvl.h

CFLAGS = -I$(INCLUDEDIR)

LIB = libcvl.a
SRC = elwise.c facilt.c internal.c library.c permute.c reduce.c scan.c \
	vecscalar.c
OBJ = $(SRC:.c=.o)

$(LIB):	$(OBJ)
	-rm $(LIB)
	ar cr $(LIB) $(OBJ)
	ranlib $(LIB)

$(OBJ): $(INCLUDEDIR)/$(INCLUDE) cm2cvl.h
	$(CC) $(CFLAGS) -c $*.c

clean:
	-rm -f $(OBJ) $(LIB)
