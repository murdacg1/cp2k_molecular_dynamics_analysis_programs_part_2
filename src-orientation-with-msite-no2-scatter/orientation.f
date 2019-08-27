        PROGRAM orientation

        USE common_data_mod
        USE reading_mod
        USE get_solute_mod
        USE binning_mod

! Purpose: Get orientation distribution (of cos theta) from a CP2K simulation of (H20)n-NO2 scattering simulation
! Program is run as:
!       orientation.exe < orientation.in >& orientation.out &
!
! It is assumed that the file run-01.xyz has the atoms organized as O,H,H,Li; ... N2,O2,O2,Be
!
! Program reads first the file orientation.in:
! 5000             f_avg (the final f frames are used for averaging) (avg calculated from last 50ps of the run)
! 9000             f (total number of frames, not including the zeroth frame) (total run=1fs*9000=90ps since every 10th printed)
! 217              m (number of molecules per frame or cell)
! 4                a (number of atoms per molecule)
! 15.0 15.0 71.44  cell ABC values [Angstrom]
! 101              c (the number of bins = c + 1)

        IMPLICIT NONE

        INTEGER :: &
          my_m_solute, my_m, &
          my_d_solute, my_d
        CHARACTER(len=80) my_file

        WRITE(6,'(a)') '# PROGRAM = orientation'
        CALL FLUSH(6)

        READ(5,*) f_avg
        READ(5,*) f
        f0 = f - f_avg
        READ(5,*) m
        READ(5,*) a
        ALLOCATE( cell(d) )
        cell = zero
        READ(5,*) cell(:)
        READ(5,*) c

        WRITE(6,100) '# f_avg = ', f_avg
        WRITE(6,100) '# f = ', f
        WRITE(6,100) '# f0+1 = ', f0+1
        WRITE(6,100) '# m = ', m
        WRITE(6,100) '# a = ', a
        WRITE(6,100) '# d = ', d
        WRITE(6,300) '# cell = ', cell(:)
        WRITE(6,100) '# number of bins c = ', c
        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

        ALLOCATE( tim(0:f) )
        ALLOCATE( pot(0:f) )
        ALLOCATE( symbol(a) )

!   Trajectory: read in (no need to subtract COM position in each frame since calculating static quantity=NVT or re-thermalized=NVE )

        my_file='../run-01.xyz'
        my_d=d
        ALLOCATE( coor(0:f,m,a,my_d) )
        CALL reading(my_d, coor, &
          my_file)

! orientation of solute: angles and cosines
        my_file='solute.dat'
        my_m_solute=1
        my_m=my_m_solute
        my_d_solute=6
        my_d=my_d_solute
        ALLOCATE( solute(0:f,my_m,my_d) )
        CALL get_solute(my_m,my_d, &
          my_file)

! bin solute angles and cosines

        m = my_m

        my_d=1 
        my_file='angleX.bin'
        CALL binning(my_d, solute, &
          my_file) 

        my_d=2 
        my_file='angleY.bin'
        CALL binning(my_d, solute, &
          my_file) 

        my_d=3 
        my_file='angleZ.bin'
        CALL binning(my_d, solute, &
          my_file) 

        my_d=4 
        my_file='COSangleX.bin'
        CALL binning(my_d, solute, &
          my_file) 

        my_d=5 
        my_file='COSangleY.bin'
        CALL binning(my_d, solute, &
          my_file) 

        my_d=6 
        my_file='COSangleZ.bin'
        CALL binning(my_d, solute, &
          my_file) 

        DEALLOCATE( solute )

100     FORMAT(1x,a40,1(1x,i10))
200     FORMAT(1x,a40,1(1x,e20.10))
300     FORMAT(1x,a40,3(1x,e20.10))
410     FORMAT(1x,a40,4(1x,e20.10))

        END PROGRAM orientation
