SHELL=/bin/sh
#
SRCS=	gds2gdt.f pds2pdt.f pds2pdtens.f cnvgrib.f cnv12.f cnv22.f \
        cnv21.f gdt2gds.f makepds.f putgbexn.f makepdsens.f setbit.f
OBJS=	gds2gdt.o pds2pdt.o pds2pdtens.o cnvgrib.o cnv12.o cnv22.o \
        cnv21.o gdt2gds.o makepds.o putgbexn.o makepdsens.o setbit.o

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
#
#     G95 Fortran Compiler
#
FC = g95
#
#
#     Gfortran (GPL) Fortran Compiler
#
# FC = gfortran
#
#LDFLAGS = -p -bnoquiet -bloadmap:mug
#LDFLAGS = -pg
LDFLAGS = 
INC = -I /pub/share/ncoops/g2lib-1.2.2

LIBS = -L/pub/share/ncoops/g2lib-1.2.2 -lg2\
       -L/pub/share/ncoops/w3lib-2.0 -lw3\
       -L/pub/share/ncoops/jasper-1.900.1/src/libjasper/.libs -ljasper -lpng -lz

CMD =		cnvgrib
PROFLIB =	-lprof

# To perform the default compilation, use the first line
# To compile with flowtracing turned on, use the second line
# To compile giving profile additonal information, use the third line
# WARNING:  SIMULTANEOUSLY PROFILING AND FLOWTRACING IS NOT RECOMMENDED 
#FFLAGS =	-O3 -pg -qrealsize=4 $(INC)
FFLAGS  = -O3 -g $(INC)
#FFLAGS =	 -F
#FFLAGS =	 -Wf"-ez"

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

# Make the profiled version of the command and call it a.out.prof
#
$(CMD).prof:	$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(PROFLIB) $(LIBS)

clean:
	-rm -f $(OBJS)

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS) makefile
