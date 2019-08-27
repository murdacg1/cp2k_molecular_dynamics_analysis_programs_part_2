# module load gcc
FC = gfortran

OBJ1 = \
common_data_mod.o \
reading_mod.o \
int2str_mod.o \
flt2str_mod.o \
piksrt2_mod.o \
get_number_contacts_mod.o \
number_contacts.o

#OP = -g -Wunused -O3 -ffast-math -funroll-loops -ftree-vectorize -march=native -msse3 -ffree-form
OP = -O3 -ffast-math -funroll-loops -ftree-vectorize -march=native -msse3 -ffree-form

BIN1 = number_contacts.x

# =================================================================

clean:
	rm -f *.o *.mod *.x

# =================================================================

.f.o:
	$(FC) -c $(OP) $<

$(BIN1): $(OBJ1)
	$(FC) $(OBJ1) -o $(BIN1)
