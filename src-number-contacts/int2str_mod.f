        MODULE int2str_mod

        USE common_data_mod

        IMPLICIT NONE

        CONTAINS

        CHARACTER(len=20) FUNCTION int2str(k)
! Convert an integer to string

        INTEGER, INTENT(in) :: k

        WRITE(int2str, *) k
        int2str = ADJUSTL(int2str)

        END FUNCTION int2str

        END MODULE int2str_mod
