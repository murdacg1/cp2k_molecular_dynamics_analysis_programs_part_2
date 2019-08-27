# FC = gfortran-mp-4.3
FC = gfortran

OBJ = \
common_data_mod.o \
reading_mod.o \
get_rdf_mod.o \
rdf.o

OP = -O3 -ffast-math -funroll-loops -ftree-vectorize -msse3 -ffree-form

BIN = rdf.x

# =================================================================

clean:
	rm -f *.o *.mod *.x

# =================================================================

.f.o:
	$(FC) -c $(OP) $<

$(BIN): $(OBJ)
	$(FC) $(OBJ) -o $(BIN)
