        PROGRAM coords

        USE common_data_mod
        USE reading_writing_mod

! Purpose: Extract final frame from a CP2K simulation of (H20)n slab:
! read tip4p; write tip4p

! Program is run as:
!       ../coords.exe < coords.in >& coords.out &
!
! It is assumed that the file run-01.xyz have the atoms organized as O,H,H

! Program reads first the file coords.in:

! 216           m (number of water molecules per frame or cell)
! 4             a (number of atoms per water molecule, last one is the M site if more than 3)
! 14.17294599664232300417 14.17294599664232300417 67.50101746667517036119  center_old
! 14.17294599664232300417 14.17294599664232300417 67.50101746667517036119  center

        IMPLICIT NONE

        CHARACTER(len=80) file_old, file_new

        WRITE(6,'(a)') '# PROGRAM = coords'
        CALL FLUSH(6)

        READ(5,*) m
        READ(5,*) a
        READ(5,*) center_old(:)
        READ(5,*) center(:)
        WRITE(6,*) 'center_old, center [bohr] = ', &
                    center_old(:) , center(:)
        center_update = (center - center_old)*bohr2ang
        WRITE(6,*) 'center_update [A] = ', center_update(:)

        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

        file_old = 'geometry_old.xyz'
        file_old = TRIM(file_old)
        file_new = 'geometry.xyz'
        file_new = TRIM(file_new)

        CALL reading_writing(file_old, file_new)

        END PROGRAM coords
