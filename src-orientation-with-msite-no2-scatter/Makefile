# FC = gfortran-mp-4.3
FC = gfortran

OBJ1 = \
common_data_mod.o \
reading_mod.o \
get_solute_mod.o \
binning_mod.o \
orientation.o

OP = -O3 -ffast-math -funroll-loops -ftree-vectorize -msse3 -ffree-form
#OP = -g -O0 -ffree-form

BIN1 = orientation.x

# =================================================================

clean:
	rm -f *.o *.mod *.x

# =================================================================

.f.o:
	$(FC) -c $(OP) $<

$(BIN1): $(OBJ1)
	$(FC) $(OBJ1) -o $(BIN1)
