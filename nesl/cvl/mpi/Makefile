# This Makefile was designed for use with MPICH.  You'll need to modify
# it for use with any other MPI implementation.  

# Should be called with TOPCC and MPI_DIR set (Can also override e.g. AR for 
# the Paragon) 

# Add lfibrng6a.c to SRC and add -DFIBRNG to TOPCC if you are using
# the portable pseuodrandom number generator.

CC		= $(TOPCC)
AR		= ar
LIB		= libcvl.a
INCLUDES	= -I. -I../../include -I$(MPI_DIR)/include
CFLAGS		= -DNDEBUG -DBUF_SIZE=8192 -DRCV_EVERY=160 $(INCLUDES)
SRC		= elwise.c facilt.c library.c messages.c permute.c rank.c \
		reduce.c scan.c vecscalar.c 
OBJ		= $(SRC:.c=.o)

$(LIB):		$(OBJ)
		$(AR) crl $(LIB) $(OBJ)
		-ranlib $(LIB)

# On the CM-5, there's no obvious way to create a host/node library, so we
# use this rule instead.
cm5mpi:		$(OBJ)
		touch $(LIB)

$(OBJ):		mpicvl.h
		$(CC) $(CFLAGS) -c $*.c

clean:
		rm -f $(OBJ) $(LIB)
