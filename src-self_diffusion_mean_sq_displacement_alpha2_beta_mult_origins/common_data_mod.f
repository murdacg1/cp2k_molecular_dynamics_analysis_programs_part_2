        MODULE common_data_mod

        INTEGER, PARAMETER :: dp=8

        INTEGER :: f_avg, f, df, m, a
        INTEGER :: f_offset
        INTEGER, PARAMETER :: d=3
        REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: cell
        REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: tim, pot
        CHARACTER(len=1), DIMENSION(:), ALLOCATABLE :: symbol
        REAL(KIND=dp), DIMENSION(:,:,:,:), ALLOCATABLE :: coor
        REAL(KIND=dp), DIMENSION(:,:,:), ALLOCATABLE :: coor_mol
        REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: &
          msd_mol, mqd_mol, alpha2_mol, &
          beta_mol_a, beta_mol_b, beta_mol_c

        REAL(KIND=dp), DIMENSION(:,:), ALLOCATABLE :: &
          sd_mol

        REAL(KIND=dp), PARAMETER :: zero=0.0_dp
        REAL(KIND=dp), PARAMETER :: bohr2ang=0.529177249_dp
        REAL(KIND=dp), PARAMETER :: fs2ps=1.0e-3_dp

        END MODULE common_data_mod
