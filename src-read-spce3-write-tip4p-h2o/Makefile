# module load gcc
FC = gfortran

OBJ = \
common_data_mod.o \
reading_writing_mod.o \
coords.o

OP = -O3 -ffast-math -funroll-loops -ftree-vectorize -msse3 -ffree-form

BIN = coords.x

# =================================================================

clean:
	rm -f *.o *.mod *.x

# =================================================================

.f.o:
	$(FC) -c $(OP) $<

$(BIN): $(OBJ)
	$(FC) $(OBJ) -o $(BIN)
