TOPCC = gcc -O2

CC = $(TOPCC)
CFLAGS =
XLIBPATH = /usr/X11R6/lib
XLIBPATH2 = /usr/local/lib

SRC = xneslplot.c input.c
OBJS = $(SRC:.c=.o)

xneslplot: $(OBJS) xneslplot.h
	$(CC) $(CFLAGS) -o xneslplot $(OBJS) -L$(XLIBPATH) -L$(XLIBPATH2) -lX11

.c.o:
	$(CC) $(CFLAGS) -c $*.c

listfiles:
	@echo xneslplot.h xneslplot.c input.c

clean:
	-rm -f xneslplot $(OBJS)
