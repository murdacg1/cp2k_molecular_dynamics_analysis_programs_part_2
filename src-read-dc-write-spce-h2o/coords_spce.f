        PROGRAM coords_spce

        USE common_data_mod
        USE reading_writing_mod

! Purpose: Extract final frame from a CP2K simulation of (H20)n slab with a single solute molecule (last molecule in the xyz file):
! read dc/spackman; write spce/spackman+com site

! Program is run as:
!       ../coords_spce.exe < coords_spce.in >& coords_spce.out &
!
! It is assumed that the file run-01.xyz have the atoms organized as O,H,H(,M) ; O,H,H(,M); ...; N,O,O(,M)

! Program reads first the file coords_spce.in:

! 215           m_w (number of water molecules per frame or cell)
! 1             m_s (number of solute molecules per frame or cell)
! 4             a_w_old (number of atoms per water molecule, last one is the M site if more than 3)
! 3             a_s_old (number of atoms per solute molecule, last one is the M site if more than 3)
! 3             a_w (number of atoms per water molecule, last one is the M site if more than 3)
! 4             a_s (number of atoms per solute molecule, last one is the M site if more than 3)
! 15.1065589017861 14.2580626814867 67.7442647138845                       center_old
! 14.17294491434192402326 14.17294491434192402326 67.50101231203913681481  center
! 15.99491463     1.0078250321  1.0078250321  0.0  mass_w(:)
! 15.99491463     1.0078250321  1.0078250321  0.0  mass_s(:)

        IMPLICIT NONE

        CHARACTER(len=80) file_old, file_new

        WRITE(6,'(a)') '# PROGRAM = coords_spce'
        CALL FLUSH(6)

        READ(5,*) m_w
        READ(5,*) m_s
        READ(5,*) a_w_old
        READ(5,*) a_s_old
        READ(5,*) a_w
        READ(5,*) a_s
        READ(5,*) center_old(:)
        READ(5,*) center(:)
        WRITE(6,*) 'center_old, center [bohr] = ', center_old(:) , center(:)
        center_update = (center - center_old)*bohr2ang
        WRITE(6,*) 'center_update [A] = ', center_update(:)
        ALLOCATE( mass_w(4) )
        mass_w = zero
        READ(5,*) mass_w(:)
        ALLOCATE( mass_s(4) )
        mass_s = zero
        READ(5,*) mass_s(:)

        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

        file_old = 'run-01.xyz_final_old'
        file_old = TRIM(file_old)
        file_new = 'run-01.xyz_final_new'
        file_new = TRIM(file_new)

        CALL reading_writing(file_old, file_new)

        DEALLOCATE( mass_w )
        DEALLOCATE( mass_s )

        END PROGRAM coords_spce
