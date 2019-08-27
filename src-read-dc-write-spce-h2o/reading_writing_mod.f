        MODULE reading_writing_mod

        USE common_data_mod

        IMPLICIT NONE

        CONTAINS

        SUBROUTINE reading_writing(file_old,file_new)

        CHARACTER(len=80) file_old, file_new

        INTEGER :: jm, ja
        INTEGER :: a_max_temp
        REAL(KIND=dp) :: temp
        CHARACTER(len=80) :: string80
        REAL(KIND=dp), DIMENSION(d) :: coor_temp, com_temp
        REAL(KIND=dp), DIMENSION(:,:,:), ALLOCATABLE :: coor
        CHARACTER(len=2), DIMENSION(:,:), ALLOCATABLE :: symbol

        WRITE(6,'(3a)') '# SUBROUTINE = reading_writing; filenames = ', &
          TRIM(file_old), TRIM(file_new)
        CALL FLUSH(6)

        OPEN(3,file=TRIM(file_old),status='unknown')
        OPEN(4,file=TRIM(file_new),status='unknown')

        READ(3,*) a_max_temp
        READ(3,*) string80
        WRITE(4,*) a_max_temp
        WRITE(4,*) string80


        ALLOCATE ( coor(m_w,a_w_old,d) )
        coor = zero
        ALLOCATE ( symbol(m_w,a_w_old) )
        symbol = ''
        coor_temp = zero

        DO jm = 1, m_w
          DO ja = 1, a_w_old
            READ(3,*) &
              symbol(jm,ja), coor_temp(:)
              coor_temp = coor_temp + center_update
              coor(jm,ja,:) = coor_temp(:)
          END DO
        END DO

        DO jm = 1, m_w
          DO ja = 1, a_w
            WRITE(4,*) symbol(jm,ja), coor(jm,ja,:), ' H2O ', jm
          END DO
        END DO

        DEALLOCATE ( coor )
        DEALLOCATE ( symbol )


        ALLOCATE ( coor(m_s,a_s,d) )
        coor = zero
        ALLOCATE ( symbol(m_s,a_s) )
        symbol = ''
        coor_temp = zero

        DO jm = 1, m_s
          com_temp = zero
          DO ja = 1, a_s_old
            READ(3,*) & 
              symbol(jm,ja), coor_temp(:)
              coor_temp = coor_temp + center_update
              coor(jm,ja,:) = coor_temp(:)
              com_temp(:) = com_temp(:) + mass_s(ja)*coor(jm,ja,:)
              IF ( symbol(jm,ja) == 'N' ) THEN
                symbol(jm,ja) = 'O'
              ELSE IF ( symbol(jm,ja) == 'O' ) THEN
                symbol(jm,ja) = 'H'
              END IF
          END DO
          com_temp(:) = com_temp(:) / SUM( mass_s )
          coor(jm,a_s,:) = com_temp(:)
          symbol(jm,a_s) = 'M3'
        END DO

        DO jm = 1, m_s
          DO ja = 1, a_s
            WRITE(4,*) symbol(jm,ja), coor(jm,ja,:), ' H2Os ', m_w+jm
          END DO
        END DO

        DEALLOCATE ( coor )
        DEALLOCATE ( symbol )


        CLOSE(3)
        CLOSE(4)

        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

        END SUBROUTINE reading_writing

        END MODULE reading_writing_mod
