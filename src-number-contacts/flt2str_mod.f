        MODULE flt2str_mod

        USE common_data_mod

        IMPLICIT NONE

        CONTAINS

        CHARACTER(len=20) FUNCTION flt2str(r)
! Convert a float to string

        REAL(KIND=dp), INTENT(in) :: r

        WRITE(flt2str,'(f10.2)') r
        flt2str = ADJUSTL(flt2str)

        END FUNCTION flt2str

        END MODULE flt2str_mod
