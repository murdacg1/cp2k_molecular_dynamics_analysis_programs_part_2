# FC = gfortran-mp-4.3
FC = gfortran

OBJ1 = \
common_data_mod.o \
reading_mod.o \
get_alpha2_beta_mod.o \
alpha2_beta.o

OP = -O3 -ffast-math -funroll-loops -ftree-vectorize -march=native -msse3 -ffree-form

BIN1 = alpha2_beta.x

# =================================================================

clean:
	rm -f *.o *.mod *.x

# =================================================================

.f.o:
	$(FC) -c $(OP) $<

$(BIN1): $(OBJ1)
	$(FC) $(OBJ1) -o $(BIN1)
