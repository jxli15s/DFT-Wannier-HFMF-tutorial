SUFFIX = .f90

FC = gfortran
FFLAGS = -O2 
#-DDEBUG

SOURCE = const.o
SOURCE += vector.o
SOURCE += gnu.o
SOURCE += utilities.o
SOURCE += dos.o
SOURCE += poscar.o
SOURCE += kpt.o
SOURCE += eigenval.o
SOURCE += procar.o

ppvasp: $(SOURCE) main.o
	@$(FC) $(FFLAGS) -o $@ $^ $(LIBS)
	@echo "Linking ..."
	@echo "Executable is $@"

%.o:%$(SUFFIX)
	@$(FC) $(FFLAGS) -c $<
	@echo "Compiling $^ ..."

clean:
	rm -f *.o *.mod; touch *$(SUFFIX)


