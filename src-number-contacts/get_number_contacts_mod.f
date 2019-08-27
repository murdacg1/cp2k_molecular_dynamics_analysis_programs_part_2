        MODULE get_number_contacts_mod

        USE common_data_mod
        USE int2str_mod
        USE piksrt2_mod

        IMPLICIT NONE

        CONTAINS

        SUBROUTINE get_number_contacts(my_symbol_1, my_atom_1, my_symbol_2, &
          rc, p, q, my_file)

        CHARACTER(len=1) my_symbol_1, my_symbol_2
        INTEGER :: my_atom_1, p, q
        REAL(KIND=dp) :: rc
        CHARACTER(len=80) my_file

        INTEGER :: jf, ja2
        REAL(KIND=dp), DIMENSION(d) :: r_vec
        REAL(KIND=dp) :: r, na, increment
        INTEGER, DIMENSION(4) :: my_atom_2

	REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: distance_array
        INTEGER, DIMENSION(:), ALLOCATABLE :: index_array
        INTEGER :: c, jc

        WRITE(6,'(2a)') &
          '# SUBROUTINE = get_number_contacts; filename = ', &
          TRIM(my_file)
        WRITE(6,*) 'my_symbol_1, my_atom_1, my_symbol_2, rc, p, q = ', &
          my_symbol_1, my_atom_1, my_symbol_2, rc, p, q
        CALL FLUSH(6)

        OPEN(3,file=TRIM(my_file),status='unknown')

        WRITE(3,'(3a)') &
          '#          1f         2tim   3na', &
          '           my_symbol_2//my_atom_2(1)', &
          ' (2)           (3)          (4)'
        CALL FLUSH(3)

! find size of desired neigbor array
        jc = 0 ! counter
        DO ja2 = 0, a-1 
          IF ( (ja2 == my_atom_1) .OR. &
               (symbol(ja2) /= my_symbol_2) ) CYCLE
          jc = jc + 1
        END DO
        c = jc
        WRITE(6,*) 'counter c = ', c
        ALLOCATE( distance_array(c), index_array(c) )

        DO jf = 0, f

          jc = 0
          distance_array = zero
          index_array = 0
          na = 0
 
          DO ja2 = 0, a-1

            IF ( (ja2 == my_atom_1) .OR. &
                 (TRIM(ADJUSTL(symbol(ja2))) /= TRIM(ADJUSTL(my_symbol_2))) ) CYCLE

            r_vec = coor(jf,ja2,:) - coor(jf,my_atom_1,:)
            r_vec(:) = r_vec(:) - cell(:)*ANINT(r_vec(:)/cell(:))
            r = SQRT(DOT_PRODUCT(r_vec, r_vec))
            jc = jc + 1
            distance_array(jc) = r
            index_array(jc) = ja2

            increment = (1.0_dp - (r/rc)**p) / &
                        (1.0_dp - (r/rc)**q)

            na = na + increment

          END DO

          CALL piksrt2(c,distance_array,index_array)

          WRITE(3,100) &
            jf, tim(jf), na, &
            ( TRIM(ADJUSTL(symbol(index_array(jc)))) // &
              TRIM(ADJUSTL(int2str((index_array(jc))))), &
            distance_array(jc), &
            jc=1,4 )

          CALL FLUSH(3)

        END DO

        CLOSE(3)


        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

100     FORMAT(1(1x,i12),1(1x,f12.6),1(1x,f4.1),4(4x,a10,1x,f10.4))

        END SUBROUTINE get_number_contacts

        END MODULE get_number_contacts_mod
