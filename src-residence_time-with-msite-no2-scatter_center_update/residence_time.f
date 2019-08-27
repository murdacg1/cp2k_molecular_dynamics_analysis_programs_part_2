        PROGRAM residence_time

        USE common_data_mod
        USE reading_writing_mod
        USE get_z_avg_mod

! Purpose: density for CP2K simulation of (H20)n

! Program is run as:
!       ../residence_time.x < residence_time.in >& residence_time.out &
!
! It is assumed that the file run-01.xyz is read

! Program reads first the file residence_time.in:

! 800           f_avg (the final f_avg frames are used) (avg calculated from last 80ps of the run)
! 900           f     (total number of frames, not including the zeroth frame) (total run=1fs*90000=90ps, every 100th)
! 100           f_width (number of frames to average over to get z_avg)
! 216           m_w (number of water molecules per frame or cell)
! 1             m_s (number of solute molecules per frame or cell)
! 4             a_w (number of atoms per water molecule, last one is the M site if more than 3)
! 5             a_s (number of atoms per solute molecule, last one is the M site if more than 3)
! 15.1065589017861 14.2580626814867 67.7442647138845                       center_old
! 14.17294491434192402326 14.17294491434192402326 67.50101231203913681481  center
! 15.0 15.0 71.44  cell ABC values [Angstrom]
! 15.99491463     1.0078250321  1.0078250321  0.0       mass_w(:)
! 14.0030740052  15.99491463   15.99491463    0.0  0.0  mass_s(:)
! 14.9077       z_gds (z of gibbs dividing surface, Ang) (from tanh fit in gnuplot)
! 1.0069        delta (width of the interfacial region, Ang) (from tanh fit in gnuplot)
! 160           n (total number of trajectories to read in) n directories follow, 1 per line:
! NO2_SCATTER_spce_spackman_two_add_sites_final_fit_3/dir_mons_reversed-1
! ...

        IMPLICIT NONE

        INTEGER :: jn, jm, ja, f0, jb
         REAL(KIND=dp) :: rn
        CHARACTER(len=80) my_dir, file_4a, file_4b
        CHARACTER(len=160) &
          file_1a, file_1b, file_2a, file_2b, file_3a, file_3b

        WRITE(6,*)
        WRITE(6,*)
        WRITE(6,'(a)') '# PROGRAM = residence_time'
        CALL FLUSH(6)

        READ(5,*) f_avg
        READ(5,*) f
        f0 = f - f_avg
        READ(5,*) f_width
        b = f_avg / f_width
        READ(5,*) m_w
        READ(5,*) m_s
        m = m_w + m_s
        READ(5,*) a_w
        READ(5,*) a_s
        a = MAX(a_w, a_s)
        READ(5,*) center_old(:)
        READ(5,*) center(:)
        center_old = center_old*bohr2ang
        center = center*bohr2ang
        center_update = center - center_old
        ALLOCATE( cell(d) )
        cell = zero
        ALLOCATE( cell_half(d) )
        cell_half = zero
        READ(5,*) cell(:)
        cell_half = half*cell
        ALLOCATE( mass_w(a_w) )
        mass_w = zero
        READ(5,*) mass_w(:)
        ALLOCATE( mass_s(a_s) )
        mass_s = zero
        READ(5,*) mass_s(:)
        READ(5,*) z_gds
        READ(5,*) delta
        delta_2 = two*delta

        READ(5,*) n
        rn = REAL(n,dp)
        ALLOCATE( dir(n) )
        dir = ''
        DO jn = 1, n
          READ(5,'(a)') dir(jn)
          WRITE(6,'(a)') dir(jn)
        END DO

        WRITE(6,150) '# f_avg, f, f0+1, f_width, b = ', &
                        f_avg, f, f0+1, f_width, b
        WRITE(6,130) '# m_w, m_s, m = ', m_w, m_s, m
        WRITE(6,130) '# a_w, a_s, a = ', a_w, a_s, a
        WRITE(6,230) '# center_old [A] = ', center_old(:)
        WRITE(6,230) '# center [A] = ', center(:)
        WRITE(6,230) '# center_update [A] = ', center_update(:)
        WRITE(6,230) '# cell = ', cell(:)
        WRITE(6,230) '# cell_half = ', cell_half(:)
        WRITE(6,250) '# mass_w, SUM(mass_w) = ', &
                        mass_w, SUM(mass_w)
        WRITE(6,260) '# mass_s, SUM(mass_s) = ', &
                        mass_s, SUM(mass_s)
        WRITE(6,230) '# z_gds, delta, delta_2 = ', &
                        z_gds, delta, delta_2
        WRITE(6,110) '# n = ', n
        CALL FLUSH(6)

        ALLOCATE( rescattered(n,b) )
        ALLOCATE( absorbed(n,b) )
        ALLOCATE( zn_avg(n,b) )
        ALLOCATE( zn_sd(n,b) )
        ALLOCATE( zc_avg(n,b) )
        ALLOCATE( zc_sd(n,b) )
        rescattered = 0
        absorbed = 0
        zn_avg = zero
        zn_sd = zero
        zc_avg = zero
        zc_sd = zero

        ALLOCATE( tim_avg(b) )
        tim_avg = zero
        DO jb = 1, b
          tim_avg(jb) = ten*REAL(jb,dp)+five
        END DO

        ALLOCATE( vac_avg_n(b) )
        ALLOCATE( sigma_avg_n(b) )
        ALLOCATE( rho_avg_n(b) )
        ALLOCATE( vac_avg_c(b) )
        ALLOCATE( sigma_avg_c(b) )
        ALLOCATE( rho_avg_c(b) )
        vac_avg_n = zero
        sigma_avg_n = zero
        rho_avg_n = zero
        vac_avg_c = zero
        sigma_avg_c = zero
        rho_avg_c = zero
 
        ALLOCATE( tim(0:f) )
        ALLOCATE( pot(0:f) )
        ALLOCATE( symbol(m,a) )
        ALLOCATE( coor(0:f,m,a,d) )
        ALLOCATE( zn(0:f) )
        ALLOCATE( zc(0:f) )

        DO jn = 1, n
          my_dir = dir(jn)
          file_1a = TRIM(my_dir) // 'run-01.xyz'
          file_1b = TRIM(my_dir) // 'run-01_COM_center_update.xyz'
          file_2a = TRIM(my_dir) // 'N_center_update' ! NITROGEN of the NO2
          file_2b = TRIM(my_dir) // 'C_center_update' ! COM of the NO2
          file_3a = TRIM(my_dir) // 'ZN_AVG_center_update'
          file_3b = TRIM(my_dir) // 'ZC_AVG_center_update'
          file_1a = TRIM(file_1a)
          file_1b = TRIM(file_1b)
          file_2a = TRIM(file_2a)
          file_2b = TRIM(file_2b)
          file_3a = TRIM(file_3a)
          file_3b = TRIM(file_3b)
          WRITE(6,'(a)') '# file_1a = ', TRIM(file_1a)
          WRITE(6,'(a)') '# file_1b = ', TRIM(file_1b)
          WRITE(6,'(a)') '# file_2a = ', TRIM(file_2a)
          WRITE(6,'(a)') '# file_2b = ', TRIM(file_2b)
          WRITE(6,'(a)') '# file_3a = ', TRIM(file_3a)
          WRITE(6,'(a)') '# file_3b = ', TRIM(file_3b)
          CALL FLUSH(6)
          CALL reading_writing(file_1a, file_1b, file_2a, file_2b) ! reading_writing and getting COM of NO2 for each frame
          CALL get_z_avg(jn,file_3a, file_3b)
        END DO

        file_4a = 'AVG_NUM_N'
        file_4b = 'AVG_NUM_C'
        file_4a = TRIM(file_4a)
        file_4b = TRIM(file_4b)
        WRITE(6,*)
        WRITE(6,*)
        WRITE(6,'(a)') '# file_4a = ', TRIM(file_4a)
        WRITE(6,'(a)') '# file_4b = ', TRIM(file_4b)
        CALL FLUSH(6)

        OPEN(11,file=TRIM(file_4a),status='unknown')
        WRITE(11,*) &
        '# jb, tim_avg, vac_avg_n, sigma_avg_n, rho_avg_n, check:'
        DO jb = 1, b
          WRITE(11,300) &
            jb, tim_avg(jb), &
            vac_avg_n(jb), sigma_avg_n(jb), rho_avg_n(jb), &
            vac_avg_n(jb)+sigma_avg_n(jb)+rho_avg_n(jb)-rn
          CALL FLUSH(11)
        END DO
        CLOSE(11)

        OPEN(12,file=TRIM(file_4b),status='unknown')
        WRITE(12,*) &
        '# jb, tim_avg, vac_avg_c, sigma_avg_c, rho_avg_c, check:'
        DO jb = 1, b
          WRITE(12,300) &
            jb, tim_avg(jb), &
            vac_avg_c(jb), sigma_avg_c(jb), rho_avg_c(jb), &
            vac_avg_c(jb)+sigma_avg_c(jb)+rho_avg_c(jb)-rn
          CALL FLUSH(12)
        END DO
        CLOSE(12)

        DEALLOCATE( cell )
        DEALLOCATE( cell_half )
        DEALLOCATE( mass_w )
        DEALLOCATE( mass_s )
        DEALLOCATE( dir )

        DEALLOCATE( tim_avg )
        DEALLOCATE( vac_avg_n )
        DEALLOCATE( sigma_avg_n )
        DEALLOCATE( rho_avg_n )
        DEALLOCATE( vac_avg_c )
        DEALLOCATE( sigma_avg_c )
        DEALLOCATE( rho_avg_c )
        DEALLOCATE( rescattered )
        DEALLOCATE( absorbed )
        DEALLOCATE( zn_avg )
        DEALLOCATE( zn_sd )
        DEALLOCATE( zc_avg )
        DEALLOCATE( zc_sd )

        DEALLOCATE( tim )
        DEALLOCATE( pot )
        DEALLOCATE( symbol )
        DEALLOCATE( coor )
        DEALLOCATE( zn )
        DEALLOCATE( zc )

110     FORMAT(1x,a40,1(1x,i10))
130     FORMAT(1x,a40,3(1x,i10))
150     FORMAT(1x,a40,5(1x,i10))
220     FORMAT(1x,a40,2(1x,e20.10))
230     FORMAT(1x,a40,3(1x,e20.10))
250     FORMAT(1x,a40,5(1x,e20.10))
260     FORMAT(1x,a40,6(1x,e20.10))
300     FORMAT(1(1x,i10),5(1x,f10.2))

        END PROGRAM residence_time
