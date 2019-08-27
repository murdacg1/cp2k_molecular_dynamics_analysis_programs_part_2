        PROGRAM rdf

        USE common_data_mod
        USE reading_mod
        USE get_rdf_mod

! Purpose: rdf for CP2K simulation of (H20)n
! Program is run as:
!       ../rdf.exe < rdf.in >& rdf.out &
!
! It is assumed that the files run-01*.xyz have the atoms organized as O,H,H; O,H,H; ...
!
! Program reads first the file analyze_run.in:
! 500           f_avg (the final f frames are used for averaging) (avg calculated from last 50ps of the run)
! 900           f (total number of frames, not including the zeroth frame) (total run=1fs*90000=90ps)
! 217           m (number of molecules per frame or cell)
! 4             apm (number of atoms per molecule)
! 15.0 15.0 71.44  cell ABC values [Angstrom]
! 0.0 6.0 0.025    r1 r2 dr [Angstrom]

        IMPLICIT NONE

        REAL(KIND=dp) :: r1, r2
        CHARACTER(len=2) name1, name2
        CHARACTER(len=80) my_file

        WRITE(6,'(a)') '# PROGRAM = rdf'
        CALL FLUSH(6)

        ALLOCATE( cell(d) )

        READ(5,*) f_avg
        READ(5,*) f
        READ(5,*) m
        READ(5,*) apm
        READ(5,*) cell(:)
        READ(5,*) r1, r2, dr

        a = m*apm
        b = ANINT((r2 - r1)/dr)

        WRITE(6,200) '# f_avg, f = ', f_avg, f
        WRITE(6,200) '# m, apm = ', m, apm
        WRITE(6,100) '# a = ', a
        WRITE(6,310) '# cell = ', cell(:)
        WRITE(6,310) '# r1, r2, dr = ', r1, r2, dr
        WRITE(6,100) '# b = ', b

        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

        ALLOCATE( coor(0:f,a,d) )
        ALLOCATE( symbol(a) )

        my_file='../run-01.xyz'
        CALL reading(my_file)

!       GOTO 901

        name1='O1'
        name2='O1'
        my_file='go1o1_mine.dat'
        CALL get_rdf(name1, name2, my_file)

        name1='H1'
        name2='H1'
        my_file='gh1h1_mine.dat'
        CALL get_rdf(name1, name2, my_file)

        name1='O1'
        name2='H1'
        my_file='go1h1_mine.dat'
        CALL get_rdf(name1, name2, my_file)

901     CONTINUE

        name1='O1'
        name2='N2'
        my_file='go1n2_mine.dat'
        CALL get_rdf(name1, name2, my_file)

!       GOTO 902

        name1='H1'
        name2='N2'
        my_file='gh1n2_mine.dat'
        CALL get_rdf(name1, name2, my_file)

        name1='O1'
        name2='O2'
        my_file='go1o2_mine.dat'
        CALL get_rdf(name1, name2, my_file)

        name1='H1'
        name2='O2'
        my_file='gh1o2_mine.dat'
        CALL get_rdf(name1, name2, my_file)

902     CONTINUE

        DEALLOCATE( cell )

        DEALLOCATE( coor )
        DEALLOCATE( symbol )

100     FORMAT(1x,a40,1(1x,i10))
200     FORMAT(1x,a40,2(1x,i10))
110     FORMAT(1x,a40,1(1x,e20.10))
310     FORMAT(1x,a40,3(1x,e20.10))

        END PROGRAM rdf
