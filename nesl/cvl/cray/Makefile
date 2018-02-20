# might need to hack include path to get cvl.h (1/21/94 -- mz)

CSRC = celwise.c scan.c cscan.c cvecops.c facilt.c vecops.c elwise.c vprims.c \
        rank.c sort.c library.c
FSRC = 
SSRC = selwise.s sscan.s svecops.s ssort.s
SRC = $(CSRC) $(FSRC) $(SSRC)

COBJ = $(CSRC:.c=.o)
FOBJ = $(FSRC:.f=.o)
SOBJ = $(SSRC:.s=.o)

LIB = libcvl.a
INCLUDEDIR = ../../include

CC = cc 
CFLAGS = -O -h intrinsics -hinline3,scalar3,vector3,task0

default: $(LIB)

cvecops.o cscan.o celwise.o:	defins.h $(INCLUDEDIR)/cvl.h

# The elwise file (and only the elwise file) needs to be compiled with
# ivdep to get the intrinsic loops (written in C) to vectorize
elwise.o:       elwise.c
	$(CC) -I$(INCLUDEDIR) $(CFLAGS) -h ivdep -c $*.c

.c.o:
	$(CC) -I$(INCLUDEDIR) $(CFLAGS) -c $*.c

$(LIB): $(COBJ) $(SOBJ) $(FOBJ)
	-rm -f $(LIB)
	ar cr $(LIB) $(COBJ) $(SOBJ) $(FOBJ)

clean: 
	-rm $(LIB) *.o

listfiles:
	@echo $(SRC) defins.h cray.h
