        MODULE reading_writing_mod

        USE common_data_mod

        IMPLICIT NONE

        CONTAINS

        SUBROUTINE reading_writing(file_old,file_new)

        CHARACTER(len=80) file_old, file_new

        INTEGER :: jm, ja
        INTEGER :: atot_temp
        INTEGER :: jf_temp
        REAL(KIND=dp) :: tim_temp, pot_temp
        CHARACTER(len=3) char3_temp
        CHARACTER(len=8) char8_temp
        CHARACTER(len=5) char5_temp
        REAL(KIND=dp), DIMENSION(d) :: coor_temp, com_temp
        REAL(KIND=dp), DIMENSION(:,:,:), ALLOCATABLE :: coor
        CHARACTER(len=2), DIMENSION(:,:), ALLOCATABLE :: symbol

        WRITE(6,'(4a)') '# SUBROUTINE = reading_writing; filenames = ', &
          TRIM(file_old), '   ', TRIM(file_new)
        CALL FLUSH(6)

        OPEN(3,file=TRIM(file_old),status='unknown')
        OPEN(4,file=TRIM(file_new),status='unknown')

        READ(3,*) atot_temp
        WRITE(4,100) m*a
        READ(3,200) &
          char3_temp, jf_temp, &
          char8_temp, tim_temp, &
          char5_temp, pot_temp
        WRITE(4,200) &
          char3_temp, jf_temp, &
          char8_temp, tim_temp, &
          char5_temp, pot_temp

        ALLOCATE ( coor(m,a,d) )
        coor = zero
        ALLOCATE ( symbol(m,a) )
        symbol = ''
        coor_temp = zero

        DO jm = 1, m
          DO ja = 1, a
            READ(3,*) &
              symbol(jm,ja), coor_temp(:)
            coor_temp = coor_temp + center_update
            coor(jm,ja,:) = coor_temp(:)
          END DO
        END DO

        DO jm = 1, m
          DO ja = 1, a
            WRITE(4,300) symbol(jm,ja), coor(jm,ja,:), 'H2O', jm
          END DO
        END DO

        DEALLOCATE ( coor )
        DEALLOCATE ( symbol )

        CLOSE(3)
        CLOSE(4)

        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

100     FORMAT(i8)
200     FORMAT(1x,a3,1x,i8,a8,1x,f12.3,a5,1x,f20.10)
300     FORMAT(1x,a3,3(2x,f18.10),1x,a3,1x,i3)

        END SUBROUTINE reading_writing

        END MODULE reading_writing_mod
