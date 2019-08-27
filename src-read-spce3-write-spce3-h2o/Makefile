# module load  gcc/4.9.2
FC = gfortran

OBJ = \
common_data_mod.o \
reading_writing_mod.o \
coords_spce3.o

OP = -O3 -ffast-math -funroll-loops -ftree-vectorize -msse3 -ffree-form

BIN = coords_spce3.x

# =================================================================

clean:
	rm -f *.o *.mod *.x

# =================================================================

.f.o:
	$(FC) -c $(OP) $<

$(BIN): $(OBJ)
	$(FC) $(OBJ) -o $(BIN)
