SHELL=/bin/sh
CC=xlc
FC=xlf
LIB=libbacio.a
INC=clib.h
FFLAGS= -O3 -qnosave -qarch=auto
AFLAGS= -X64
CFLAGS=-O3 -qarch=auto

$(LIB):	$(LIB)( bacio.v1.3.o baciof.o )

$(LIB)(bacio.v1.3.o):       bacio.v1.3.c $(INC)
	$(CC) -c $(CFLAGS) bacio.v1.3.c
	ar -rv $(AFLAGS) $(LIB) bacio.v1.3.o

$(LIB)(baciof.o):   baciof.f
	$(FC) -c $(FFLAGS) baciof.f
	ar -rv $(AFLAGS) $(LIB) baciof.o
	rm -f baciof.o

