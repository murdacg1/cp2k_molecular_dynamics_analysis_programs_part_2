        MODULE get_solute_mod

        USE common_data_mod

        IMPLICIT NONE

        CONTAINS

        SUBROUTINE get_solute(my_m,my_d, &
          my_file)

        INTEGER :: my_m,my_d
        CHARACTER(len=80) my_file

        INTEGER :: jf, jm, jd, f_counter
        REAL(KIND=dp) :: &
          x1, y1, z1, &
          x2, y2, z2, &
          bisectorX, bisectorY, bisectorZ, bisector, &
          angleX, angleX_avg, angleX_sqavg, angleX_rmsd, &
          COSangleX, COSangleX_avg, COSangleX_sqavg, COSangleX_rmsd, &
          angleY, angleY_avg, angleY_sqavg, angleY_rmsd, &
          COSangleY, COSangleY_avg, COSangleY_sqavg, COSangleY_rmsd, &
          angleZ, angleZ_avg, angleZ_sqavg, angleZ_rmsd, &
          COSangleZ, COSangleZ_avg, COSangleZ_sqavg, COSangleZ_rmsd

        REAL(KIND=dp) :: pi, rad2deg

        WRITE(6,'(2a)') '# SUBROUTINE = get_solute; filename = ', &
          TRIM(my_file)
        CALL FLUSH(6)

        pi = ACOS(-1.0_dp)
        rad2deg = 180.0_dp/pi

        solute = zero

        OPEN(3,file=TRIM(my_file),status='unknown')

        WRITE(3,'(3a)') &
       '# 1f 2tim 3m angleX angleY angleZ COSangleX COSangleY COSangleZ'

        f_counter = 0

        angleX_avg = zero
        angleX_sqavg = zero
        angleX_rmsd = zero
        COSangleX_avg = zero
        COSangleX_sqavg = zero
        COSangleX_rmsd = zero

        angleY_avg = zero
        angleY_sqavg = zero
        angleY_rmsd = zero
        COSangleY_avg = zero
        COSangleY_sqavg = zero
        COSangleY_rmsd = zero

        angleZ_avg = zero
        angleZ_sqavg = zero
        angleZ_rmsd = zero
        COSangleZ_avg = zero
        COSangleZ_sqavg = zero
        COSangleZ_rmsd = zero

        DO jf = f0+1, f

          f_counter = f_counter + 1

          DO jm = m, m ! just NO2, jm=m=217

            x1 = coor(jf,jm,2,1) - coor(jf,jm,1,1)
            y1 = coor(jf,jm,2,2) - coor(jf,jm,1,2)
            z1 = coor(jf,jm,2,3) - coor(jf,jm,1,3)
            x1 = x1 - cell(1)*ANINT(x1/cell(1))
            y1 = y1 - cell(2)*ANINT(y1/cell(2))
            z1 = z1 - cell(3)*ANINT(z1/cell(3))

            x2 = coor(jf,jm,3,1) - coor(jf,jm,1,1)
            y2 = coor(jf,jm,3,2) - coor(jf,jm,1,2)
            z2 = coor(jf,jm,3,3) - coor(jf,jm,1,3)
            x2 = x2 - cell(1)*ANINT(x2/cell(1))
            y2 = y2 - cell(2)*ANINT(y2/cell(2))
            z2 = z2 - cell(3)*ANINT(z2/cell(3))

            bisectorX = (x1+x2)/2.0_dp
            bisectorY = (y1+y2)/2.0_dp
            bisectorZ = (z1+z2)/2.0_dp
            bisectorZ = -bisectorZ ! because my coordinate system is flipped: molecules collide from the bottom
            bisector = &
              SQRT(bisectorX**2 + bisectorY**2 + bisectorZ**2)

            COSangleX=bisectorX/bisector
            COSangleY=bisectorY/bisector
            COSangleZ=bisectorZ/bisector

            angleX = rad2deg*ACOS(COSangleX)
            angleY = rad2deg*ACOS(COSangleY)
            angleZ = rad2deg*ACOS(COSangleZ)

            solute(jf,my_m,1) = angleX
            solute(jf,my_m,2) = angleY
            solute(jf,my_m,3) = angleZ
            solute(jf,my_m,4) = COSangleX
            solute(jf,my_m,5) = COSangleY
            solute(jf,my_m,6) = COSangleZ

            WRITE(3,100) &
              jf, tim(jf), jm, &
              (solute(jf,my_m,jd),jd=1,my_d)

            angleX_avg = angleX_avg + angleX
            angleX_sqavg = angleX_sqavg + angleX**2
            COSangleX_avg = COSangleX_avg + COSangleX
            COSangleX_sqavg = COSangleX_sqavg + COSangleX**2

            angleY_avg = angleY_avg + angleY
            angleY_sqavg = angleY_sqavg + angleY**2 
            COSangleY_avg = COSangleY_avg + COSangleY
            COSangleY_sqavg = COSangleY_sqavg + COSangleY**2

            angleZ_avg = angleZ_avg + angleZ
            angleZ_sqavg = angleZ_sqavg + angleZ**2 
            COSangleZ_avg = COSangleZ_avg + COSangleZ
            COSangleZ_sqavg = COSangleZ_sqavg + COSangleZ**2

          END DO
        END DO

        CLOSE(3)

        angleX_avg = angleX_avg/REAL(f_counter*my_m,dp)
        angleX_sqavg = angleX_sqavg/REAL(f_counter*m,dp)
        angleX_rmsd = SQRT(angleX_sqavg - angleX_avg**2)
        COSangleX_avg = COSangleX_avg/REAL(f_counter*my_m,dp)
        COSangleX_sqavg = COSangleX_sqavg/REAL(f_counter*m,dp)
        COSangleX_rmsd = SQRT(COSangleX_sqavg - COSangleX_avg**2)

        angleY_avg = angleY_avg/REAL(f_counter*my_m,dp)
        angleY_sqavg = angleY_sqavg/REAL(f_counter*m,dp)
        angleY_rmsd = SQRT(angleY_sqavg - angleY_avg**2)
        COSangleY_avg = COSangleY_avg/REAL(f_counter*my_m,dp)
        COSangleY_sqavg = COSangleY_sqavg/REAL(f_counter*m,dp)
        COSangleY_rmsd = SQRT(COSangleY_sqavg - COSangleY_avg**2)

        angleZ_avg = angleZ_avg/REAL(f_counter*my_m,dp)
        angleZ_sqavg = angleZ_sqavg/REAL(f_counter*m,dp)
        angleZ_rmsd = SQRT(angleZ_sqavg - angleZ_avg**2)
        COSangleZ_avg = COSangleZ_avg/REAL(f_counter*my_m,dp)
        COSangleZ_sqavg = COSangleZ_sqavg/REAL(f_counter*m,dp)
        COSangleZ_rmsd = SQRT(COSangleZ_sqavg - COSangleZ_avg**2)

        WRITE(6,200) '# angleX_avg angleX_rmsd [deg] = ', &
          angleX_avg, angleX_rmsd
        WRITE(6,200) '# COSangleX_avg COSangleX_rmsd = ', &
          COSangleX_avg, COSangleX_rmsd

        WRITE(6,200) '# angleY_avg angleY_rmsd [deg] = ', &
          angleY_avg, angleY_rmsd
        WRITE(6,200) '# COSangleY_avg COSangleY_rmsd = ', &
          COSangleY_avg, COSangleY_rmsd

        WRITE(6,200) '# angleZ_avg angleZ_rmsd [deg] = ', &
          angleZ_avg, angleZ_rmsd
        WRITE(6,200) '# COSangleZ_avg COSangleZ_rmsd = ', &
          COSangleZ_avg, COSangleZ_rmsd

        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

100     FORMAT(1(1x,i10),1(1x,e20.10),1(1x,i4),6(1x,e20.10))
200     FORMAT(1x,a40,2(1x,e20.10))

        END SUBROUTINE get_solute

        END MODULE get_solute_mod
