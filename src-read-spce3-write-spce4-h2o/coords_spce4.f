        PROGRAM coords_spce4

        USE common_data_mod
        USE reading_writing_mod

! Purpose: Extract final frame from a CP2K simulation of (H20)n slab:
! read spce3; write spce4

! Program is run as:
!       ../coords_spce4.exe < coords_spce4.in >& coords_spce4.out &
!
! It is assumed that the file run-01.xyz have the atoms organized as O,H,H

! Program reads first the file coords_spce4.in:

! 216           m (number of water molecules per frame or cell)
! 3             a_old (number of atoms per water molecule, last one is the M site if more than 3)
! 4             a (number of atoms per water molecule, last one is the M site if more than 3)
! 0.18348396 0.18348396 a_par b_par parameters (so that r_OM=0.212A (r_OH=1.0A, q_HOH=109.47deg)
! 15.1065589017861 14.2580626814867 67.7442647138845                       center_old
! 15.1065589017861 14.2580626814867 67.7442647138845                       center
!!14.17294491434192402326 14.17294491434192402326 67.50101231203913681481  center

        IMPLICIT NONE

        CHARACTER(len=80) file_old, file_new

        WRITE(6,'(a)') '# PROGRAM = coords_spce4'
        CALL FLUSH(6)

        READ(5,*) m
        READ(5,*) a_old
        READ(5,*) a
        READ(5,*) a_par, b_par
        READ(5,*) center_old(:)
        READ(5,*) center(:)
        WRITE(6,*) 'center_old, center [bohr] = ', &
                    center_old(:) , center(:)
        center_update = (center - center_old)*bohr2ang
        WRITE(6,*) 'center_update [A] = ', center_update(:)

        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

        file_old = 'run-01.xyz_final_old'
        file_old = TRIM(file_old)
        file_new = 'run-01.xyz_final_new'
        file_new = TRIM(file_new)

        CALL reading_writing(file_old, file_new)

        END PROGRAM coords_spce4
