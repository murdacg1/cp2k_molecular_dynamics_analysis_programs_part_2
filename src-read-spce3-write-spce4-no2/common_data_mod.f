        MODULE common_data_mod

        INTEGER, PARAMETER :: dp=8
        INTEGER, PARAMETER :: d=3
        REAL(KIND=dp), PARAMETER :: zero=0.0_dp
        REAL(KIND=dp), PARAMETER :: bohr2ang=0.529177249_dp

        INTEGER :: m, a_old, a
        REAL(KIND=dp) ::  a_par, b_par
        REAL(KIND=dp), DIMENSION(d) :: &
          center_old = zero, center = zero, center_update = zero

        END MODULE common_data_mod
