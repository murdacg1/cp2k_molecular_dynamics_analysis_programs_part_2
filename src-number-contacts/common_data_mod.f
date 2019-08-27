        MODULE common_data_mod

        INTEGER, PARAMETER :: dp=8

        INTEGER :: f_avg, f, &
          a, &
          m_wat, a_wat, &
          m_sol, a_sol, &
          n_my_atom_1
        INTEGER, PARAMETER :: d=3
        REAL(KIND=dp) :: dt, temperature
        REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: cell
        REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: tim, pot
        CHARACTER(len=1), DIMENSION(:), ALLOCATABLE :: symbol 
        REAL(KIND=dp), DIMENSION(:,:,:), ALLOCATABLE :: coor
        INTEGER, DIMENSION(:), ALLOCATABLE :: my_atom_1_array
        CHARACTER(len=1), DIMENSION(:), ALLOCATABLE :: my_symbol_1_array

        REAL(KIND=dp), PARAMETER :: zero=0.0_dp
        REAL(KIND=dp), PARAMETER :: eps=1.0e-12_dp, eps2=1.0e-6_dp
        REAL(KIND=dp), PARAMETER :: bohr2ang=0.529177249_dp
        REAL(KIND=dp), PARAMETER :: au2debye=1.0_dp/0.39343029_dp
        REAL(KIND=dp), PARAMETER :: hart2kcal=627.51_dp, hart2cm=219474.63_dp, hart2kelvin=315773.3_dp
        REAL(KIND=dp), PARAMETER :: fs2ps=1.0e-3_dp, autime2fs=2.41888432650478e-2_dp

        END MODULE common_data_mod
