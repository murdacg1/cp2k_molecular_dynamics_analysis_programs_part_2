        MODULE reading_mod

        USE common_data_mod

        IMPLICIT NONE

        CONTAINS

        SUBROUTINE reading(my_file)

        CHARACTER(len=80) my_file
 
        INTEGER :: jf, ja, jm, japm, jd
        INTEGER :: a_max_temp
        INTEGER :: jf_temp
        REAL(KIND=dp) :: tim_temp, pot_temp
        CHARACTER(len=3) char3_temp
        CHARACTER(len=8) char8_temp
        CHARACTER(len=5) char5_temp

        WRITE(6,'(2a)') '# SUBROUTINE = reading; filename = ', &
          TRIM(my_file)
        CALL FLUSH(6)

        coor = zero
        symbol = ' '

        OPEN(3,file=TRIM(my_file),status='unknown')

        DO jf = 0, f

          READ(3,*) a_max_temp
          READ(3,120) &
            char3_temp, jf_temp, &
            char8_temp, tim_temp, &
            char5_temp, pot_temp
!         WRITE(9,*) a_max_temp
!         WRITE(9,120) &
!           char3_temp, jf_temp, &
!           char8_temp, tim_temp, &
!           char5_temp, pot_temp

          DO ja = 1, (m-1)*apm
            READ(3,*) symbol(ja), &
              (coor(jf,ja,jd),jd=1,d)
              IF (symbol(ja) == 'O') THEN
                symbol(ja) = 'O1'
              ELSE IF (symbol(ja) == 'H') THEN
                symbol(ja) = 'H1'
              ELSE IF (symbol(ja) == 'Li') THEN
                symbol(ja) = 'M1'
              END IF
!           WRITE(9,*) symbol(ja), &
!             (coor(jf,ja,jd),jd=1,d)
          END DO

          DO ja = (m-1)*apm+1, a
            READ(3,*) symbol(ja), &
              (coor(jf,ja,jd),jd=1,d)
              IF (symbol(ja) == 'N') THEN
                symbol(ja) = 'N2'
              ELSE IF (symbol(ja) == 'O') THEN
                symbol(ja) = 'O2'
              ELSE IF (symbol(ja) == 'Be') THEN
                symbol(ja) = 'M2'
              END IF
!           WRITE(9,*) symbol(ja), &
!             (coor(jf,ja,jd),jd=1,d)
          END DO

        END DO

        CLOSE(3)

        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

!       CALL FLUSH(9)

120     FORMAT(1x, &
          a3,1x,i8, &
          a8,1x,f12.3, &
          a5,1x,f20.10 &
          )

        END SUBROUTINE reading

        END MODULE reading_mod
