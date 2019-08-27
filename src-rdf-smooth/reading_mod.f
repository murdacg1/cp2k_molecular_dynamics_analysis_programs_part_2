        MODULE reading_mod

        USE common_data_mod

        IMPLICIT NONE

        CONTAINS

        SUBROUTINE reading

        INTEGER :: jb, counter

!       WRITE(6,'(a)') '# SUBROUTINE = reading'
!       CALL FLUSH(6)

        counter = 0
        DO jb = 0, b_max
          READ(5,*,END=900) r(jb), g(jb), n(jb)
          counter = counter + 1
        END DO
!       WRITE(6,100) '# counter = ', counter
900     CONTINUE
        b = counter - 1

!       WRITE(6,100) '# counter = ', counter
!       CALL FLUSH(6)

!00     FORMAT(1x,a40,1(1x,i10))

        END SUBROUTINE reading

        END MODULE reading_mod
