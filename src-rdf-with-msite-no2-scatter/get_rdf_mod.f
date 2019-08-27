        MODULE get_rdf_mod

        USE common_data_mod

        IMPLICIT NONE

        CONTAINS

        SUBROUTINE get_rdf(name1, name2, my_file)

        CHARACTER(len=2) name1, name2
        CHARACTER(len=80) my_file

        INTEGER :: f0, jf, ja, ja1, ja2, jd, jb, n1, n2, n3, n4, na, nb
        REAL(KIND=dp) :: dist, &
          volume, density1, density2, density3, density4, &
          contrib, factor1, factor2, factor3
        REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: &
          r, g, n, &
          dist_temp
        REAL(KIND=dp) :: pi

        WRITE(6,'(a)') '# SUBROUTINE = get_rdf'
        WRITE(6,'(3(x,a))') 'name1, name2 =', name1, name2
        CALL FLUSH(6)

        pi = ACOS(-1.0_dp)

        ALLOCATE( r(-1:b) )
        ALLOCATE( g(-1:b) )
        ALLOCATE( n(-1:b) )
        ALLOCATE( dist_temp(d) )
 
        r = zero
        g = zero
        n = zero
        dist_temp = zero

        DO jb = 0, b
          r(jb) = (REAL(jb,dp)+0.5_dp)*dr
        END DO

        volume = 1.0_dp
        DO jd = 1, d
          volume = volume*cell(jd)
        END DO

        contrib = 1.0_dp
        IF (name1 == name2) contrib = 2.0_dp

        n1 = 0
        n2 = 0
        n3 = 0
        n4 = 0

        f0 = f - f_avg
        DO jf = f0+1, f

          IF (jf == f0+1) THEN
            DO ja = 1, a
              IF (symbol(ja) == 'O1') n1 = n1 + 1
              IF (symbol(ja) == 'H1') n2 = n2 + 1
              IF (symbol(ja) == 'N2') n3 = n3 + 1
              IF (symbol(ja) == 'O2') n4 = n4 + 1
            END DO
          END IF

          DO ja1 = 1, a-1

            IF (symbol(ja1) == 'M1') CYCLE
            IF (symbol(ja1) == 'M2') CYCLE

            IF ( (name1 == name2) .AND. &
                 (symbol(ja1) /= name1) ) CYCLE

!           IF ( (symbol(ja1) /= name1) .OR.  &
!                (symbol(ja1) /= name2) ) CYCLE

            DO ja2 = ja1+1, a

              IF (symbol(ja2) == 'M1') CYCLE
              IF (symbol(ja2) == 'M2') CYCLE

              IF ( (name1 == name2) .AND. &
                   (symbol(ja2) /= name2) ) CYCLE

              IF ( (name1 /= name2) .AND. &
                   (symbol(ja1) == symbol(ja2)) ) CYCLE

!             IF ( (symbol(ja2) /= name1) .OR.  &
!                  (symbol(ja2) /= name2) ) CYCLE

!             WRITE(6,200) ' # ja1, ja2 = ', ja1, ja2

              IF ( ( (symbol(ja1) == name1) .AND. &
                     (symbol(ja2) == name2) ) .OR. &
                   ( (symbol(ja1) == name2) .AND. &
                     (symbol(ja2) == name1) ) ) THEN

              dist = zero
              DO jd = 1, d
                dist_temp(jd) = coor(jf,ja2,jd) - coor(jf,ja1,jd)
                dist_temp(jd) = &
                  dist_temp(jd) - &
                  cell(jd) * ANINT( dist_temp(jd) / cell(jd) )
                dist = &
                  dist + &
                  ( dist_temp(jd) )**2
              END DO
              dist = SQRT(dist)
              jb = INT(dist/dr)
              IF (jb <= b) THEN
                g(jb) = g(jb) + contrib
!               WRITE(8,*) symbol(ja1), coor(jf,ja1,jd)
!               WRITE(8,*) symbol(ja2), coor(jf,ja2,jd)
!               WRITE(8,*) jb, r(jb), dist
!               WRITE(8,*)
              END IF

              END IF

            END DO

          END DO

        END DO

        IF (name1 == name2) THEN
          IF (name1 == 'O1') THEN
            na = n1-1
            nb = n1
          ELSE IF (name1 == 'H1') THEN
            na = n2
            nb = n2-1
          ELSE IF (name1 == 'N2') THEN
            na = n3
            nb = n3-1
          ELSE IF (name1 == 'O2') THEN
            na = n4
            nb = n4-1
          END IF
        ELSE
          IF (name1 == 'O1') na = n1
          IF (name1 == 'H1') na = n2
          IF (name1 == 'N2') na = n3
          IF (name1 == 'O2') na = n4
          IF (name2 == 'O1') nb = n1
          IF (name2 == 'H1') nb = n2
          IF (name2 == 'N2') nb = n3
          IF (name2 == 'O2') nb = n4
        END IF

        density1 = n1/volume
        density2 = n2/volume
        density3 = n3/volume
        density4 = n4/volume

        factor1 = &
          ( (volume)/((4.0_dp/3.0_dp)*pi) ) / &
            ( REAL(na*nb,dp) )

        WRITE(6,'(3a)') '# name1, name2 = ', name1, name2
        WRITE(6,400) '# n1, n2, n3, n4 = ', n1, n2, n3, n4
        WRITE(6,*)   '# a = ', a
        WRITE(6,200) '# na, nb = ', na, nb
        WRITE(6,110) '# volume = ', volume
        WRITE(6,410) '# density1, density2, density3, density4 = ', &
                        density1, density2, density3, density4
        WRITE(6,110) '# contrib = ', contrib
        WRITE(6,110) '# factor1 = ', factor1
        WRITE(6,300) '# f_avg, f, f0+1 = ', &
          f_avg, f, f0+1
        WRITE(6,100) '# b = ', b
        WRITE(6,110) '# dr = ', dr

        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

        OPEN(3,file=trim(my_file),status='unknown')
        DO jb = 0, b
          factor2 = &
            ( factor1 ) / &
            ( ((jb+1)**d-(jb)**d)*dr**d )
          g(jb) = g(jb) * factor2 / REAL(f_avg,dp)
          IF (name1 == name2) THEN
            IF (name1 == 'O1') factor3 = REAL(n1,dp)*factor2
            IF (name1 == 'H1') factor3 = REAL(n2,dp)*factor2
            IF (name1 == 'N2') factor3 = REAL(n3,dp)*factor2
            IF (name1 == 'O2') factor3 = REAL(n4,dp)*factor2
          ELSE
!           factor3 = REAL(n1,dp)*factor2
            IF ( (name1 == 'O1') .and. (name2 == 'H1') ) &
              factor3 = REAL(n1,dp)*factor2
            IF ( (name1 == 'O1') .and. (name2 == 'N2') ) &
              factor3 = REAL(n1,dp)*factor2
            IF ( (name1 == 'H1') .and. (name2 == 'N2') ) &
              factor3 = REAL(n2,dp)*factor2
            IF ( (name1 == 'O1') .and. (name2 == 'O2') ) &
              factor3 = REAL(n1,dp)*factor2
            IF ( (name1 == 'H1') .and. (name2 == 'O2') ) &
              factor3 = REAL(n2,dp)*factor2
          END IF
          n(jb) = n(jb-1) + g(jb) / factor3
          WRITE(3,320) r(jb), g(jb), n(jb)
        END DO
        CLOSE(3)

        DEALLOCATE( r )
        DEALLOCATE( g )
        DEALLOCATE( n )
        DEALLOCATE( dist_temp )
 
100     FORMAT(1x,a40,1(1x,i10))
110     FORMAT(1x,a40,1(1x,f20.10))
200     FORMAT(1x,a40,2(1x,i10))
410     FORMAT(1x,a40,4(1x,f20.10))
300     FORMAT(1x,a40,3(1x,i10))
320     FORMAT(3(1x,e20.10))
400     FORMAT(1x,a40,4(1x,i10))

        END SUBROUTINE get_rdf

        END MODULE get_rdf_mod
