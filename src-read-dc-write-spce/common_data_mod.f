        MODULE common_data_mod

        INTEGER, PARAMETER :: dp=8
        INTEGER, PARAMETER :: d=3
        REAL(KIND=dp), PARAMETER :: zero=0.0_dp
        REAL(KIND=dp), PARAMETER :: bohr2ang=0.529177249_dp

        INTEGER :: m_w, m_s, a_w_old, a_s_old, a_w, a_s 
        REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: mass_w, mass_s
        REAL(KIND=dp), DIMENSION(d) :: &
          center_old = zero, center = zero, center_update = zero

        END MODULE common_data_mod
