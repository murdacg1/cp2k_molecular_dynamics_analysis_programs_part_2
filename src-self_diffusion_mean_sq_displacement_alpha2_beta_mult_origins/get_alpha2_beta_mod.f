        MODULE get_alpha2_beta_mod

        USE common_data_mod

        IMPLICIT NONE

        CONTAINS

        SUBROUTINE get_alpha2_beta(my_file)

        CHARACTER(len=80) my_file

        INTEGER :: f0, jf, jm, jd
        REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: dR

        REAL(KIND=dp) :: t_offset
        REAL(KIND=dp) :: dt ! timestep [ps]; usually 0.5d-3_dp ps = 0.5_dp fs for flexible pots and 2.5d-3_dp ps = 2.5_dp fs for rigid pot (SPC/E)

! additional variables to do with multiple time origins

        INTEGER :: jf2, jf2_min, jf2_max, f03, jf3, jf2_counter_temp
        INTEGER, DIMENSION(:), ALLOCATABLE :: jf2_counter

        WRITE(6,'(2a)') '# SUBROUTINE = get_alpha2_beta; filename = ', &
          TRIM(my_file)
        CALL FLUSH(6)

        dt = tim(1) - tim(0)
        t_offset = REAL(f_offset,dp)*dt
        WRITE(6,*) '# dt [ps], t_offset [ps], f_offset = ', dt, t_offset, f_offset

        sd_mol = zero

        msd_mol = zero
        msd_mol = zero ! mean square displacement
        mqd_mol = zero ! mean quartic displacement
        alpha2_mol = zero
        beta_mol_a = zero ! various ways of calculating the beta coefficient; c version is closest to Will and JM way (with the offset)
        beta_mol_b = zero
        beta_mol_c = zero

        ALLOCATE( dR(d) )

        dR = zero

        f0 = f - f_avg

        OPEN(3,file=TRIM(my_file),status='unknown')

        WRITE(3,'(2a)') &
          '# 1f 2tim 3msd_mol 4mqd_mol ', &
          '5alpha2_mol 6beta_mol_a 7beta_mol_b 8beta_mol_c'

        jf2_min = 0
        jf2_max = f - f0 + 1
        ALLOCATE ( jf2_counter(0:f) )
        jf2_counter = 0

!       DO jf = f0+1, f
        DO jf = f0, f

          DO jf2 = jf2_min, jf2_max          

          f03 = f0 + jf2*df
          jf3 = jf + jf2*df
          IF (jf3 > f) CYCLE
          jf2_counter(jf) = jf2_counter(jf) + 1

          DO jm = 1, m

            DO jd = 1, d
              dR(jd) = coor_mol(jf3,jm,jd) - coor_mol(f03,jm,jd)
            END DO

            sd_mol(jf,jm) = (dR(1)**2+dR(2)**2+dR(3)**2)

            msd_mol(jf) = msd_mol(jf) + (dR(1)**2+dR(2)**2+dR(3)**2)
            mqd_mol(jf) = mqd_mol(jf) + (dR(1)**2+dR(2)**2+dR(3)**2)**2

          END DO

          END DO

          jf2_counter_temp = jf2_counter(jf)
          IF (jf2_counter_temp == 0) jf2_counter_temp = 1

          msd_mol(jf) = msd_mol(jf)/REAL(m*jf2_counter_temp,dp)
          mqd_mol(jf) = mqd_mol(jf)/REAL(m*jf2_counter_temp,dp)

        END DO

!       DO jf = f0+1, f
        DO jf = f0, f

          alpha2_mol(jf) = &
            ( ( mqd_mol(jf) ) / ( 3.0_dp * msd_mol(jf)**2 ) ) - 1.0_dp
          beta_mol_a(jf) = &
            ( log( msd_mol(jf) ) - log( msd_mol(jf-1) ) ) / &
            ( log( tim(jf) ) - log( tim(jf-1) ) )
          beta_mol_b(jf) = &
            ( ( msd_mol(jf) - msd_mol(jf-1) ) / ( msd_mol(jf) ) ) / &
            ( ( tim(jf) - tim(jf-1) ) / ( tim(jf) ) )
          IF ( (jf-f_offset >= f0+1) .and. (jf+f_offset <= f) ) THEN
            beta_mol_c(jf) = &
              ( log( msd_mol(jf+f_offset) ) - log( msd_mol(jf-f_offset) ) ) / &
              ( log( tim(jf+f_offset) ) - log( tim(jf-f_offset) ) )
!           WRITE(9,*) &
!           jf,tim(jf+f_offset),tim(jf-f_offset),msd_mol(jf+f_offset),msd_mol(jf-f_offset)
          END IF
          WRITE(3,100) jf, tim(jf), msd_mol(jf), mqd_mol(jf), &
          alpha2_mol(jf), beta_mol_a(jf), beta_mol_b(jf), beta_mol_c(jf)

          DO jm = 1, m
            WRITE(4,200) jf, tim(jf), jm, sd_mol(jf,jm)
!           WRITE(7,300) jf, tim(jf), jm, coor_mol(jf,jm,:)
          END DO

          WRITE(17,400) jf, jf2_counter(jf)

        END DO

        CLOSE(3)

        DEALLOCATE( dR )

        DEALLOCATE( jf2_counter )

        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

100     FORMAT(1(1x,i10),1(1x,e20.10),6(1x,e20.10))
200     FORMAT(1(1x,i10),1(1x,e20.10),1(1x,i10),1(1x,e20.10))
!300     FORMAT(1(1x,i10),1(1x,e20.10),1(1x,i10),3(1x,e20.10))
400     FORMAT(2(1x,i10))

        END SUBROUTINE get_alpha2_beta

        END MODULE get_alpha2_beta_mod
