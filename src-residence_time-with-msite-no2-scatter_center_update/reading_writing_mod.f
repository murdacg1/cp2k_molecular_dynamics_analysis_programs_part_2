        MODULE reading_writing_mod

        USE common_data_mod

        IMPLICIT NONE

        CONTAINS

        SUBROUTINE reading_writing(file_1a, file_1b, file_2a, file_2b)

        CHARACTER(len=160) file_1a, file_1b, file_2a, file_2b

        INTEGER :: jf, jm, ja, jd
        INTEGER :: atot_temp
        INTEGER :: jf_temp
        REAL(KIND=dp) :: tim_temp, pot_temp
        CHARACTER(len=3) char3_temp
        CHARACTER(len=8) char8_temp
        CHARACTER(len=5) char5_temp
        REAL(KIND=dp), DIMENSION(d) :: coor_temp, com

        WRITE(6,*)
        WRITE(6,*)
        WRITE(6,'(5a)') '# SUBROUTINE = reading_writing; filenames = ', &
          TRIM(file_1a), TRIM(file_1b), TRIM(file_2a), TRIM(file_2b)
        CALL FLUSH(6)

        tim = zero
        pot = zero
        symbol = ''
        coor = zero
        zn = zero
        zc = zero 
        coor_temp = zero

        OPEN(3,file=TRIM(file_1a),status='unknown')
!       OPEN(4,file=TRIM(file_1b),status='unknown')
        OPEN(7,file=TRIM(file_2a),status='unknown')
        OPEN(8,file=TRIM(file_2b),status='unknown')

        DO jf = 0, f
          READ(3,*) atot_temp
!         WRITE(4,100) atot_temp+1
          READ(3,200) &
            char3_temp, jf_temp, &
            char8_temp, tim_temp, &
            char5_temp, pot_temp
!         WRITE(4,200) &
!           char3_temp, jf_temp, &
!           char8_temp, tim_temp, &
!           char5_temp, pot_temp
          tim(jf) = tim_temp
          pot(jf) = pot_temp
          DO jm = 1, m_w
            DO ja = 1, a_w
              READ(3,*) &
                symbol(jm,ja), coor_temp(:)
              coor_temp = coor_temp + center_update
              coor(jf,jm,ja,:) = coor_temp(:)
!             WRITE(4,300) &
!               symbol(jm,ja), coor(jf,jm,ja,:)
            END DO
          END DO
          DO jm = m_w+1, m ! m_s is always 1
            com = zero
            DO ja = 1, a_s-1
              READ(3,*) &
                symbol(jm,ja), coor_temp(:)
              coor_temp = coor_temp + center_update
              coor(jf,jm,ja,:) = coor_temp(:)
              com(:) = com(:) + mass_s(ja)*coor(jf,jm,ja,:)
            END DO
            ja = 1 ! N of NO2
            WRITE(7,300) &
              symbol(jm,ja), coor(jf,jm,ja,:)
            zn(jf) = coor(jf,jm,ja,d)
            ja = a_s ! COM of NO2 (write as M3 or, better, as Mg)
            symbol(jm,ja) = 'Mg'
            coor(jf,jm,ja,:) = com(:) / SUM( mass_s )
            WRITE(8,300) &
              symbol(jm,ja), coor(jf,jm,ja,:)
            zc(jf) = coor(jf,jm,ja,d)
!           DO ja = 1, a_s
!             WRITE(4,300) &
!               symbol(jm,ja), coor(jf,jm,ja,:)
!           END DO
          END DO
        END DO

        CLOSE(3)
!       CLOSE(4)
        CLOSE(7)
        CLOSE(8)

100     FORMAT(i8)
200     FORMAT(1x,a3,1x,i8,a8,1x,f12.3,a5,1x,f20.10)
300     FORMAT(1x,a3,3(2x,f18.10))

        END SUBROUTINE reading_writing

        END MODULE reading_writing_mod
