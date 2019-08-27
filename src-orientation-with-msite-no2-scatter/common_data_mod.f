        MODULE common_data_mod

        INTEGER, PARAMETER :: dp=8

        INTEGER :: f_avg, f, m, a, c, f0 !, a_with_msite
        INTEGER, PARAMETER :: d=3
        REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: cell
        REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: tim, pot
        CHARACTER(len=1), DIMENSION(:), ALLOCATABLE :: symbol
        REAL(KIND=dp), DIMENSION(:,:,:,:), ALLOCATABLE :: &
          coor
        REAL(KIND=dp), DIMENSION(:,:,:), ALLOCATABLE :: &
          solute

        REAL(KIND=dp), PARAMETER :: zero=0.0_dp
        REAL(KIND=dp), PARAMETER :: eps=1.0e-12_dp
        REAL(KIND=dp), PARAMETER :: fs2ps=1.0e-3_dp

        END MODULE common_data_mod
