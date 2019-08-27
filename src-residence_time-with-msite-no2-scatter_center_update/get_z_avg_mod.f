        MODULE get_z_avg_mod

        USE common_data_mod

        IMPLICIT NONE

        CONTAINS

        SUBROUTINE get_z_avg(jn, file_3a, file_3b)

        CHARACTER(len=160) file_3a, file_3b

        INTEGER :: jn, f0, jf, jb, jb_r, jb_a
        REAL(KIND=dp) :: z_temp

        WRITE(6,*)
        WRITE(6,*)
        WRITE(6,*) '# SUBROUTINE = get_z_avg; jn, file_3a, file_3b = ', &
          jn, TRIM(file_3a), TRIM(file_3b)
        CALL FLUSH(6)

        OPEN(9,file=TRIM(file_3a),status='unknown')
        OPEN(10,file=TRIM(file_3b),status='unknown')

        f0 = f - f_avg

        jb_r = 0
        jb_a = 0

        DO jf = f0+1, f

          jb = jf / f_width
          IF ( MOD(jf,f_width) == 0 ) jb = jb - 1

          zn_avg(jn,jb) = zn_avg(jn,jb) + zn(jf)
          zc_avg(jn,jb) = zc_avg(jn,jb) + zc(jf)

          z_temp = half*(zn(jf)+zc(jf)) ! avg of N and COM ! use both surfaces? tests for rescattering and absorption
          IF ( z_temp <= cell_half(d) ) THEN
            z_temp = -z_temp+cell_half(d)
          ELSE
            z_temp =  z_temp-cell_half(d)
          END IF
          IF ( (jb_r == 0) .AND. (jb_a == 0) ) THEN
            IF ( & ! test for rescattering
                 (z_temp >= cell(d)-z_gds-delta_2) &
               ) THEN
              jb_r = jb
            END IF
            IF ( & ! test for absorption
                 (z_temp <= -z_gds+delta_2) &
               ) THEN
              jb_a = jb
            END IF
          END IF

        END DO

        IF (jb_r /= 0) THEN
          DO jb = jb_r, b ! consider it rescattered for all future times and place in vacuum (see below)
            rescattered(jn,jb) = 1
          END DO
        END IF
        IF (jb_a /= 0) THEN
          DO jb = jb_r, b ! consider it absorbed for all future times and place in bulk (see below)
            absorbed(jn,jb) = 1
          END DO
        END IF

        DO jb = 1, b
          zn_avg(jn,jb) = zn_avg(jn,jb) / REAL(f_width,dp)
          zc_avg(jn,jb) = zc_avg(jn,jb) / REAL(f_width,dp)
        END DO

        DO jf = f0+1, f
          jb = jf / f_width
          IF ( MOD(jf,f_width) == 0 ) jb = jb - 1
          zn_sd(jn,jb) = zn_sd(jn,jb) + ( zn_avg(jn,jb) - zn(jf) )**2
          zc_sd(jn,jb) = zc_sd(jn,jb) + ( zc_avg(jn,jb) - zc(jf) )**2
        END DO
        DO jb = 1, b
          zn_sd(jn,jb) = SQRT( zn_sd(jn,jb) / REAL(f_width,dp) )
          zc_sd(jn,jb) = SQRT( zc_sd(jn,jb) / REAL(f_width,dp) )
        END DO

        WRITE(9,*) &
          '# jn, jb, tim_avg, zn_avg, zn_sd, rescattered, absorbed'
        WRITE(10,*) &
          '# jn, jb, tim_avg, zc_avg, zc_sd, rescattered, absorbed'
        DO jb = 1, b
          WRITE(9,*) jb, tim_avg(jb), zn_avg(jn,jb), zn_sd(jn,jb), &
                     rescattered(jn,jb), absorbed(jn,jb)
          WRITE(10,*) jb, tim_avg(jb), zc_avg(jn,jb), zc_sd(jn,jb), &
                     rescattered(jn,jb), absorbed(jn,jb)
        END DO

        DO jb = 1, b

          IF (rescattered(jn,jb) == 1) THEN
            vac_avg_n(jb) = vac_avg_n(jb) + one
            vac_avg_c(jb) = vac_avg_c(jb) + one
            CYCLE
          END IF
          IF (absorbed(jn,jb) == 1) THEN
            rho_avg_n(jb) = rho_avg_n(jb) + one
            rho_avg_c(jb) = rho_avg_c(jb) + one
            CYCLE
          END IF

          z_temp = zn_avg(jn,jb) ! N ! use both surfaces -- this should not cause a problem
          IF ( z_temp <= cell_half(d) ) THEN
            z_temp = -z_temp+cell_half(d)
          ELSE
            z_temp =  z_temp-cell_half(d)
          END IF

          IF ( &
               (z_temp >= z_gds-delta_2) .AND. &
               (z_temp <= z_gds+delta_2) &
             ) THEN
            sigma_avg_n(jb) = sigma_avg_n(jb) + one

          ELSE IF ( &
               (z_temp > -z_gds+delta_2) .AND. &
               (z_temp <  z_gds-delta_2) &
             ) THEN
            rho_avg_n(jb) = rho_avg_n(jb) + one

          ELSE
            vac_avg_n(jb) = vac_avg_n(jb) + one

          END IF

          z_temp = zc_avg(jn,jb) ! COM ! use both surfaces -- this should not cause a problem
          IF ( z_temp <= cell_half(d) ) THEN
            z_temp = -z_temp+cell_half(d)
          ELSE
            z_temp =  z_temp-cell_half(d)
          END IF

          IF ( &
               (z_temp >= z_gds-delta_2) .AND. &
               (z_temp <= z_gds+delta_2) &
             ) THEN
            sigma_avg_c(jb) = sigma_avg_c(jb) + one

          ELSE IF ( &
               (z_temp > -z_gds+delta_2) .AND. &
               (z_temp <  z_gds-delta_2) &
             ) THEN
            rho_avg_c(jb) = rho_avg_c(jb) + one

          ELSE
            vac_avg_c(jb) = vac_avg_c(jb) + one

          END IF

        END DO

        CLOSE(9)
        CLOSE(10)

        END SUBROUTINE get_z_avg

        END MODULE get_z_avg_mod
