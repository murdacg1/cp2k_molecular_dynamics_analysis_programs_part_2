        MODULE common_data_mod

        INTEGER, PARAMETER :: dp=8
        INTEGER, PARAMETER :: d=3
        REAL(KIND=dp), PARAMETER :: zero = 0.0_dp, one =  1.0_dp, two = 2.0_dp, five = 5.0_dp, ten = 10.0_dp, half = 0.5_dp
        REAL(KIND=dp), PARAMETER :: bohr2ang = 0.529177249_dp
        REAL(KIND=dp), PARAMETER :: eps = 1.0e-12_dp

        INTEGER :: f_avg, f, f_width, b, m_w, m_s, m, a_w, a_s, a, n        
        REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: cell, cell_half
        REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: mass_w, mass_s
        CHARACTER(len=80), DIMENSION(:), ALLOCATABLE :: dir
        REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: &
          zn, &
          zc
        REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: &
          tim, pot
	REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: &
          tim_avg, &
          vac_avg_n, sigma_avg_n, rho_avg_n, &
          vac_avg_c, sigma_avg_c, rho_avg_c
        CHARACTER(len=2), DIMENSION(:,:), ALLOCATABLE :: symbol
        REAL(KIND=dp), DIMENSION(:,:,:,:), ALLOCATABLE :: coor
        REAL(KIND=dp) :: z_gds, delta, delta_2
        REAL(KIND=dp), DIMENSION(d) :: &
          center_old = zero, center = zero, center_update = zero
        REAL(KIND=dp), DIMENSION(:,:), ALLOCATABLE :: &
          zn_avg, zn_sd, &
          zc_avg, zc_sd
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: &
          rescattered, &
          absorbed
 
        END MODULE common_data_mod
