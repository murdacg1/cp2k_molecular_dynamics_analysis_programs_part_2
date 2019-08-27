# FC = gfortran-mp-4.3
FC = gfortran

OBJ = \
common_data_mod.o \
reading_writing_mod.o \
get_z_avg_mod.o \
residence_time.o

OP = -O3 -ffast-math -funroll-loops -ftree-vectorize -msse3 -ffree-form
#OP = -g -fbounds-check -Warray-bounds -O0 -ffree-form

BIN = residence_time.x

# =================================================================

clean:
	rm -f *.o *.mod *.x

# =================================================================

.f.o:
	$(FC) -c $(OP) $<

$(BIN): $(OBJ)
	$(FC) $(OBJ) -o $(BIN)
