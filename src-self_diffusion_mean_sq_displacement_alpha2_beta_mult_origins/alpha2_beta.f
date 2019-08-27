        PROGRAM alpha2_beta

        USE common_data_mod
        USE reading_mod
        USE get_alpha2_beta_mod

! Purpose: Determine the msd and the alpha2 and beta coefficients of a CP2K simulation of (H20)n
! Program is run as:
!       alpha2_beta.exe < alpha2_beta.in >& alpha2_beta.out &
!
! It is assumed that the files run-01.xyz have the atoms organized as O,H,H; O,H,H; ...
!
! Program reads first the file alpha2_beta.in:
! 10000         f_avg (the final f frames are used for averaging) (avg calculated from last 5ps of the run)
! 20000         f (total number of frames, not including the zeroth frame) (total run=0.5fs*20k=10ps)
! 64            m (number of molecules per frame or cell)
! 3             a (number of atoms per molecule)
! 12.4138 12.4138 12.4138  cell ABC values [Angstrom]
! 200           df (the step size in number of frames for the mutiple time origins)
!               (with dt=0.5fs, dtau=df*dt=100fs same as in Lee and Tuckerman dynamics of water 2007 JCP article; note that Kuo JCTC used 500fs)
! 200           f_offset (the offset for calculating beta)
!               (with dt=0.5fs, t_offset=f_offset*dt=100fs 1/5 that of Kuo JCTC article)

        IMPLICIT NONE

        INTEGER :: jf, jm, jd
        CHARACTER(len=80) :: my_file

        WRITE(6,'(a)') '# PROGRAM = alpha2_beta'
        CALL FLUSH(6)

        ALLOCATE( cell(d) )

        READ(5,*) f_avg
        READ(5,*) f
        READ(5,*) m
        READ(5,*) a
        READ(5,*) cell(:)
        READ(5,*) df
        READ(5,*) f_offset

        WRITE(6,100) '# f_avg = ', f_avg
        WRITE(6,100) '# f = ', f
        WRITE(6,100) '# m = ', m
        WRITE(6,100) '# a = ', a
        WRITE(6,100) '# d = ', d
        WRITE(6,300) '# cell = ', cell(:)
        WRITE(6,100) '# df = ', df
        WRITE(6,100) '# f_offset = ', f_offset

        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

        ALLOCATE( tim(0:f) )
        ALLOCATE( pot(0:f) )
        ALLOCATE( symbol(a) )

!   Trajectory
        my_file='../ANALYZE/run-01.xyz'
        ALLOCATE( coor(0:f,m,a,d) )
        CALL reading(d, coor, &
          my_file)

        ALLOCATE( coor_mol(0:f,m,d) ) ! use the oxygen atom coordinate instead of molecular center of mass
        coor_mol = zero
        DO jf = 0, f
          DO jm = 1, m
            DO jd = 1, d
              coor_mol(jf,jm,jd) = coor(jf,jm,1,jd)
            END DO
          END DO
        END DO

! msd_mol of molecule=oxygen
        my_file='alpha2_beta_mol.dat'
        ALLOCATE ( msd_mol(0:f) )
        ALLOCATE ( mqd_mol(0:f) )
        ALLOCATE ( alpha2_mol(0:f) )
        ALLOCATE ( beta_mol_a(0:f) )
        ALLOCATE ( beta_mol_b(0:f) )
        ALLOCATE ( beta_mol_c(0:f) )
        ALLOCATE ( sd_mol(0:f,m) )
        CALL get_alpha2_beta(my_file)

        DEALLOCATE( coor_mol )

        DEALLOCATE( msd_mol )
        DEALLOCATE( mqd_mol )
        DEALLOCATE( alpha2_mol )
        DEALLOCATE( beta_mol_a )
        DEALLOCATE( beta_mol_b )
        DEALLOCATE ( beta_mol_c )
        DEALLOCATE ( sd_mol )
 
        DEALLOCATE( cell )
        DEALLOCATE( coor )

        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

        DEALLOCATE( pot )
        DEALLOCATE( tim )
        DEALLOCATE( symbol )

100     FORMAT(1x,a40,1(1x,i10))
200     FORMAT(1x,a40,1(1x,e20.10))
300     FORMAT(1x,a40,3(1x,e20.10))

        END PROGRAM alpha2_beta
