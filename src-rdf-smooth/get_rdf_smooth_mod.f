        MODULE get_rdf_smooth_mod

        USE common_data_mod

        IMPLICIT NONE

        CONTAINS

        SUBROUTINE get_rdf_smooth

        INTEGER :: jb

!       WRITE(6,'(a)') '# SUBROUTINE = get_rdf_smooth'
!       CALL FLUSH(6)

        jb = 2
        g_smooth(jb-2) = (  &
            69.0_dp*g(jb-2) &
          +  4.0_dp*g(jb-1) &
          -  6.0_dp*g(jb)   &
          +  4.0_dp*g(jb+1) &
          -  1.0_dp*g(jb+2) &
          ) / 70.0_dp

        jb = 2
        g_smooth(jb-1) = (  &
             2.0_dp*g(jb-2) &
          + 27.0_dp*g(jb-1) &
          + 12.0_dp*g(jb)   &
          -  8.0_dp*g(jb+1) &
          +  2.0_dp*g(jb+2) &
          ) / 35.0_dp

        DO jb = 2, b - 2
          g_smooth(jb) = (    &
            -  3.0_dp*g(jb-2) &
            + 12.0_dp*g(jb-1) &
            + 17.0_dp*g(jb)   &
            + 12.0_dp*g(jb+1) &
            -  3.0_dp*g(jb+2) &
            ) / 35.0_dp
        END DO

        jb = b - 2
        g_smooth(jb+1) = (  &
             2.0_dp*g(jb+2) &
          + 27.0_dp*g(jb+1) &
          + 12.0_dp*g(jb)   &
          -  8.0_dp*g(jb-1) &
          +  2.0_dp*g(jb-2) &
          ) / 35.0_dp

        jb = b - 2
        g_smooth(jb+2) = (  &
            69.0_dp*g(jb+2) &
          +  4.0_dp*g(jb+1) &
          -  6.0_dp*g(jb)   &
          +  4.0_dp*g(jb-1) &
          -  1.0_dp*g(jb-2) &
          ) / 70.0_dp

        DO jb = 0, b
          WRITE(6,320) r(jb), g_smooth(jb), n(jb)
        END DO

320     FORMAT(3(1x,e20.10))

        END SUBROUTINE get_rdf_smooth

        END MODULE get_rdf_smooth_mod
