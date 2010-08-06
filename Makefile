# Automated Makefile
FC = ifort
FFLAGS = -FR
COMPILE = $(FC) $(FFLAGS) -c
VASPLIB = ../vasp.5.lib
EXTOBJS = $(VASPLIB)/diolib.o $(VASPLIB)/dlexlib.o
CHGOBJS = $(EXTOBJS) mprec.o mio.o mposcar.o mchgcar.o chgcar.o
# Default
all: chgcar
# Program
chgcar: $(CHGOBJS)
	$(FC) -o chgcar $(CHGOBJS) $(LINK)
# Objects
%.o: %.f
	$(COMPILE) -o $@ $<
# Clean:
clean:
	rm *.o *.mod; touch *.f
