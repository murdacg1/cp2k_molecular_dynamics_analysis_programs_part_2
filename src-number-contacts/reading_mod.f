        MODULE reading_mod

        USE common_data_mod

        IMPLICIT NONE

        CONTAINS

        SUBROUTINE reading(my_d, total, my_file)

        INTEGER :: my_d
        REAL(KIND=dp), DIMENSION(0:f,0:a-1,my_d) :: total
        REAL(KIND=dp), DIMENSION(0:f,m_wat,a_wat,my_d) :: wat
        REAL(KIND=dp), DIMENSION(0:f,m_sol,a_sol,my_d) :: sol
        CHARACTER(len=80) my_file

        INTEGER :: jf, ja, jd
        INTEGER :: a_temp
        INTEGER :: jf_temp
        REAL(KIND=dp) :: tim_temp, pot_temp
        CHARACTER(len=3) char3_temp
        CHARACTER(len=8) char8_temp
        CHARACTER(len=5) char5_temp
        CHARACTER(len=1) symbol_temp

        WRITE(6,'(2a)') '# SUBROUTINE = reading; filename = ', &
          TRIM(my_file)
        CALL FLUSH(6)

        tim = zero
        pot = zero
        symbol = '  '
        total = zero

!       WRITE(6,*)
!       WRITE(6,*)
        OPEN(3,file=TRIM(my_file),status='unknown')
        DO jf = 0, f
          READ(3,*) a_temp
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
          DO ja = 0, a-1
            READ(3,300)  symbol_temp, (total(jf,ja,jd),jd=1,my_d)
            symbol(ja) = symbol_temp
!           WRITE(6,300) symbol(ja), (total(jf,ja,jd),jd=1,my_d)
!           CALL FLUSH(6)
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
300     FORMAT(2x,a,1x,3(f20.10))
500     FORMAT(1(1x,i10),1(1x,f20.10),4(1x,f20.10))

        END SUBROUTINE reading

        END MODULE reading_mod
