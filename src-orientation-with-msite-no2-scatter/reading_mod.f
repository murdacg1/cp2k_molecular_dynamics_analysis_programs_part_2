        MODULE reading_mod

        USE common_data_mod

        IMPLICIT NONE

        CONTAINS

        SUBROUTINE reading(my_d, my_array, &
          my_file)

        INTEGER :: my_d
        REAL(KIND=dp), DIMENSION(0:f,m,a,my_d) :: my_array
        CHARACTER(len=80) my_file

        INTEGER :: jf, jm, ja, jd
        INTEGER :: atot_temp
        INTEGER :: jf_temp
        REAL(KIND=dp) :: tim_temp, pot_temp
        CHARACTER(len=3) char3_temp
        CHARACTER(len=8) char8_temp
        CHARACTER(len=5) char5_temp

        WRITE(6,'(2a)') '# SUBROUTINE = reading; filename = ', &
          TRIM(my_file)
        CALL FLUSH(6)

        tim = zero
        pot = zero
        symbol = ' '
        my_array = zero

        OPEN(3,file=TRIM(my_file),status='unknown')

        DO jf = 0, f
          READ(3,*) atot_temp
          READ(3,200) &
            char3_temp, jf_temp, &
            char8_temp, tim_temp, &
            char5_temp, pot_temp
          tim(jf) = tim_temp*fs2ps
          pot(jf) = pot_temp
!         WRITE(6,200) &
!           char3_temp, jf_temp, &
!           char8_temp, tim(jf)/fs2ps, &
!           char5_temp, pot(jf)
!         CALL FLUSH(6)
          DO jm = 1, m
            DO ja = 1, a
              READ(3,*)  symbol(ja), (my_array(jf,jm,ja,jd),jd=1,my_d)
!             WRITE(6,*) symbol(ja), (my_array(jf,jm,ja,jd),jd=1,my_d)
!             CALL FLUSH(6)
            END DO
          END DO
        END DO

        CLOSE(3)

        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

200     FORMAT(1x, &
          a3,1x,i8, &
          a8,1x,f12.3, &
          a5,1x,f20.10 &
          )

        END SUBROUTINE reading

        END MODULE reading_mod
