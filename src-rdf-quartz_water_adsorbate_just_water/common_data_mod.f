        MODULE common_data_mod

        INTEGER, PARAMETER :: dp=8

        INTEGER :: f_avg, f, m, a, b
        INTEGER, PARAMETER :: d=3
        REAL(KIND=dp) :: dr
        REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: cell
        REAL(KIND=dp), DIMENSION(:,:,:), ALLOCATABLE :: coor
        CHARACTER(len=1), DIMENSION(:), ALLOCATABLE :: symbol

        REAL(KIND=dp), PARAMETER :: zero=0.0_dp
 
        END MODULE common_data_mod
