# Makefile for CM-5 CVL
#
# Set TOPCC to compiler+flags to override definition below
#
# Note that this leaves the object files inplace and expects vcode/Makefile to
# link to them, since there's no obvious way to create a single host/node
# library on the CM-5.  It does create a dummy libcvl.a to keep things happy.

# Add lfibrng6a.c to NODE_SRC and add -DFIBRNG to CFLAGS if you are using
# the portable pseuodrandom number generator.

# default in case TOPCC isn't defined
TOPCC =		gcc -O

CC = 		$(TOPCC)

INCLUDEDIR =	../../include
INCLUDE = 	cvl.h

CFLAGS =	-DNDEBUG -I$(INCLUDEDIR)

HOST_FLAGS =	-DCP_CODE
NODE_FLAGS =	-DPE_CODE -Dpe_obj

NODE_SRC = 	n_elwise.c n_facilt.c n_internal.c n_library.c n_permute.c \
		n_scan.c n_vecscalar.c
HOST_SRC = 	h_elwise.c h_facilt.c h_internal.c h_library.c h_permute.c \
		h_scan.c h_vecscalar.c

NODE_OBJ = 	$(NODE_SRC:.c=.o)
HOST_OBJ = 	$(HOST_SRC:.c=.o)

##########################################################################

all:	$(NODE_OBJ) $(HOST_OBJ)
	touch libcvl.a

$(NODE_OBJ):	$(INCLUDEDIR)/$(INCLUDE) cm5cvl.h node.h
	$(CC) $(CFLAGS) $(NODE_FLAGS) -c $*.c

$(HOST_OBJ):	$(INCLUDEDIR)/$(INCLUDE) cm5cvl.h host.h
	$(CC) $(CFLAGS) $(HOST_FLAGS) -c $*.c

clean:
	rm -f $(NODE_OBJ) $(HOST_OBJ)
