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
! 4             a (number of atoms per water molecule, last one is the M site if more than 3)

        IMPLICIT NONE

        CHARACTER(len=80) file_old, file_new

        WRITE(6,'(a)') '# PROGRAM = coords_spce4'
        CALL FLUSH(6)

        READ(5,*) m
        READ(5,*) a
        WRITE(6,*) 'm, a = ', m, a

        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

        file_old = 'run-01.xyz_final_old'
        file_old = TRIM(file_old)
        file_new = 'run-01.xyz_final_new'
        file_new = TRIM(file_new)

        CALL reading_writing(file_old, file_new)

        END PROGRAM coords_spce4
