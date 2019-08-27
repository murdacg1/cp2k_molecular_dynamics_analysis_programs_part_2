        MODULE common_data_mod

        INTEGER, PARAMETER :: dp=8

        INTEGER, PARAMETER :: b_max=1e4

        INTEGER :: b
        REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: r, g, n, g_smooth

        REAL(KIND=dp), PARAMETER :: zero=0.0_dp
 
        END MODULE common_data_mod
