        PROGRAM coords_spce3

        USE common_data_mod
        USE reading_writing_mod

! Purpose: Extract final frame from a CP2K simulation of (H20)n slab:
! read spce3; write spce3

! Program is run as:
!       ../coords_spce3.x < coords_spce3.in >& coords_spce3.out &
!
! It is assumed that the file geometry.xyz_final_old have the atoms organized as O,H,H

! Program reads first the file coords_spce3.in:

! 216           m (number of water molecules per frame or cell)
! 3             a (number of atoms per water molecule, last one is the M site if more than 3)
! 15.1065589017861 14.2580626814867 67.7442647138845                       center_old
! 15.1065589017861 14.2580626814867 67.7442647138845                       center
!!14.17294491434192402326 14.17294491434192402326 67.50101231203913681481  center

        IMPLICIT NONE

        CHARACTER(len=80) file_old, file_new

        WRITE(6,'(a)') '# PROGRAM = coords_spce3'
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

        file_old = 'geometry.xyz_final_old'
        file_old = TRIM(file_old)
        file_new = 'geometry.xyz_final_new'
        file_new = TRIM(file_new)

        CALL reading_writing(file_old, file_new)

        END PROGRAM coords_spce3
