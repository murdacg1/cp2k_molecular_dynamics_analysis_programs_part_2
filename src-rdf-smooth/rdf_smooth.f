        PROGRAM rdf_smooth

        USE common_data_mod
        USE reading_mod
        USE get_rdf_smooth_mod

! Purpose: smooth an rdf curve using 3rd degree 5 point smoothing (Allen & Tildesly, pp. 203-4)
! Program is run as:
!       ../rdf_smooth.x 241 < goo_mine.dat > goo_mine_smooth.dat &
! (for example)
!

        IMPLICIT NONE

!       WRITE(6,'(a)') '# PROGRAM = rdf_smooth'
!       CALL FLUSH(6)

        ALLOCATE( r(0:b_max) )
        ALLOCATE( g(0:b_max) )
        ALLOCATE( n(0:b_max) )
        ALLOCATE( g_smooth(0:b_max) )

        r = zero
        g = zero
        n = zero
        g_smooth = zero

        CALL reading

        CALL get_rdf_smooth

        DEALLOCATE( r )
        DEALLOCATE( g )
        DEALLOCATE( n )
        DEALLOCATE( g_smooth )

        END PROGRAM rdf_smooth
