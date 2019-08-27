        PROGRAM number_contacts

        USE common_data_mod
        USE reading_mod
        USE int2str_mod ! integer to string conversion
        USE flt2str_mod ! float to string conversion
        USE get_number_contacts_mod

! Purpose: Analyze a CP2K simulation of [(H20)n) slab] + [single adsorbate = sol = solute = fa = trans formic acid here]
! Program is run as:
!       number_contacts.exe < number_contacts.in >& number_contacts.out &
!
! It is assumed that the file slab-pos-1.xyz has the atoms
!
! Program reads first the file number_contacts.in:
! 25000         f_avg   final f frames are used for averaging (avg calculated from last 5ps of the run)
! 25000         f       total number of frames, not including the zeroth frame (total run=0.4fs*25k=10ps)
! 0.5           dt      time step [fs]
! 72            m_wat   number of water molecules
! 3             a_wat   number of atoms per water molecule (increase if have WCs)
! 1             m_sol   number of solute molecules
! 5             a_sol   number of atoms per solute molecule (increase if have WCs)
! 13.4724 15.5566 40.0              cell(d)   ABC values [Angstrom]
! 300.0                             temperature [Kelvin]
! 1.27 (or 1.60)                    rc
! 5                                 n_my_atom_1
! O 217                             my_symbol_1_array(jn), my_atom_1_array(jn)  atom numbering based on vmd convention: 0, 1, ...
! O 219
! C 216
! H 218
! H 220

        IMPLICIT NONE

        REAL(KIND=dp) :: rc

        INTEGER :: &
          my_m, &
          my_a, &
          my_d, &
          jn, &
          p, q
        CHARACTER(len=1) my_symbol_2
        CHARACTER(len=80) my_file

        WRITE(6,'(a)') '# PROGRAM = analyze'
        CALL FLUSH(6)

        READ(5,*) f_avg
        READ(5,*) f
        READ(5,*) dt
        READ(5,*) m_wat
        READ(5,*) a_wat
        READ(5,*) m_sol
        READ(5,*) a_sol
        a = m_wat*a_wat + m_sol*a_sol ! total number atoms
        ALLOCATE( cell(d) )
        cell = zero
        READ(5,*) cell(:)
        READ(5,*) temperature
        READ(5,*) rc
        READ(5,*) n_my_atom_1
        ALLOCATE( my_symbol_1_array(n_my_atom_1), my_atom_1_array(n_my_atom_1) )
        DO jn = 1, n_my_atom_1
          READ(5,*) my_symbol_1_array(jn), my_atom_1_array(jn)
        END DO

        WRITE(6,100) '# f_avg = ', f_avg
        WRITE(6,100) '# f = ', f
        WRITE(6,200) '# dt = ', dt
        WRITE(6,100) '# a = ', a
        WRITE(6,100) '# m_wat = ', m_wat
        WRITE(6,100) '# a_wat = ', a_wat
        WRITE(6,100) '# m_sol = ', m_sol
        WRITE(6,100) '# a_sol = ', a_sol
        WRITE(6,100) '# d = ', d
        WRITE(6,300) '# cell = ', cell(:)
        WRITE(6,200) '# temperature [Kelvin] = ', temperature
        WRITE(6,200) '# rc = ', rc
        WRITE(6,100) '# n_my_atom_1 = ', n_my_atom_1
        WRITE(6,'(a)') &
          '# my_symbol_1_array(jn), my_atom_1_array(jn):'
        DO jn = 1, n_my_atom_1
          WRITE(6,*) &
            my_symbol_1_array(jn), my_atom_1_array(jn)
        END DO
        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

        ALLOCATE( tim(0:f), pot(0:f) )
        ALLOCATE( symbol(0:a-1) )

!   Trajectory: read in each frame

        my_file='../slab-pos-1.xyz'
        my_d=d
        ALLOCATE( coor(0:f,0:a-1,my_d) )
        CALL reading(my_d, coor, my_file)

        p = 6
        q = 12

        DO jn = 1, n_my_atom_1

          SELECT CASE (TRIM(ADJUSTL(my_symbol_1_array(jn))))
          CASE ('C')
            my_symbol_2 = 'H'
          CASE ('O')
            my_symbol_2 = 'H'
          CASE ('H')
            my_symbol_2 = 'O'
          END SELECT

          WRITE(6,*) 'jn, my_symbol_1_array(jn), my_symbol_2 = ', &
                      jn, my_symbol_1_array(jn), my_symbol_2

          my_file =  ( TRIM(ADJUSTL(my_symbol_1_array(jn))) // &
          TRIM(ADJUSTL(int2str(my_atom_1_array(jn)))) // &
          '_' // &
          TRIM(ADJUSTL(my_symbol_2)) // '_contacts_' // &
          TRIM(ADJUSTL(flt2str(rc))) // '.dat' )

          WRITE(6,*) 'my_file = ', my_file
          WRITE(6,*)

!         CYCLE

          CALL get_number_contacts(my_symbol_1_array(jn), my_atom_1_array(jn), my_symbol_2, &
            rc, p, q, my_file)

        END DO

        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

        DEALLOCATE( cell )
        DEALLOCATE( tim, pot )
        DEALLOCATE( symbol )
        DEALLOCATE( my_symbol_1_array, my_atom_1_array )
        DEALLOCATE( coor )

100     FORMAT(1x,a40,1(1x,i10))
200     FORMAT(1x,a40,1(1x,e20.10))
300     FORMAT(1x,a40,3(1x,e20.10))

        END PROGRAM number_contacts
