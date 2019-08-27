        MODULE binning_mod

        USE common_data_mod

        IMPLICIT NONE

        CONTAINS

        SUBROUTINE binning(my_d, my_array, &
          my_file)

        INTEGER :: my_d
        REAL(KIND=dp), DIMENSION(0:f,m,my_d) :: my_array
        CHARACTER(len=80) my_file

        INTEGER :: v ! length of vector to bin
        INTEGER :: jf, jm, jc, jv, jd
        REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: vector
        INTEGER, DIMENSION(:), ALLOCATABLE :: counts ! needed distribution
        REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: ncounts ! normalized distribution
        REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: center ! the center of the distribution
        REAL(KIND=dp) :: width ! width of bins
        REAL(KIND=dp) :: low ! low of bins
        REAL(KIND=dp) :: high ! high of bins
        REAL(KIND=dp) :: mean ! mean of bins (abs)
        REAL(KIND=dp) :: sd ! sd of mean of bins (abs)
        REAL(KIND=dp) :: area

        WRITE(6,'(2a)') '# SUBROUTINE = binning; filename = ', &
          TRIM(my_file)
        CALL FLUSH(6)

        v = f_avg*m ! length of vector to bin
        ALLOCATE( vector(v) )
        vector = zero
        jv = 0
        DO jf = f0+1, f
          DO jm = 1, m
            jv = jv + 1
            vector(jv) = my_array(jf, jm, my_d)
          END DO
        END DO

        ALLOCATE( counts(-1:c+1) )
        ALLOCATE( ncounts(-1:c+1) )
        ALLOCATE( center(-1:c+1) )
        counts = 0
        ncounts = zero
        center = zero

        low = minval(vector)
        high = maxval(vector)
        mean = sum(vector)/REAL(v,dp)

        sd = zero
        DO jv = 1, v
          sd = sd + (vector(jv)-mean)**2
        END DO
        sd = SQRT(sd/REAL(v,dp))

        width = (high - low)/REAL(c,dp)

        IF ( width < eps ) width = eps ! avoid divide by zero errors below

        DO jc = -1, c+1
          center(jc) = low + (REAL(jc,dp)+0.5_dp)*width
        END DO
 
        DO jv = 1, v
          jc = NINT( (vector(jv)-low) / width )
!         WRITE(6,400) 'jv, vector(jv)-low = ', jv, vector(jv)-low
!         WRITE(6,400) 'jc, center(jc)-low = ', jc, center(jc)-low
!         WRITE(6,*)
!         CALL FLUSH(6)
          IF ( (jc >= -1) .AND. (jc <= c+1) ) &
            counts(jc) = counts(jc) + 1
        END DO

        area = REAL(sum(counts),dp)*width
        DO jc = -1, c+1
          ncounts(jc) = REAL(counts(jc),dp)/area
        END DO

        OPEN(3,file=TRIM(my_file),status='unknown')

        WRITE(3,'(a)') '# jc  center  counts  normalized_counts'

        DO jc = -1, c+1
          WRITE(3,300) jc, center(jc), counts(jc), ncounts(jc)
        END DO

        CLOSE(3)

        WRITE(6,110) '# samples = ', v
        WRITE(6,110) '# bins = ', c
        WRITE(6,200) '# bin width = ', width
        WRITE(6,200) '# low = ', low
        WRITE(6,200) '# high = ', high
        WRITE(6,200) '# mean = ', mean
        WRITE(6,200) '# st. dev. = ', sd
        WRITE(6,200) '# area = ', area
        WRITE(6,200) '# area/width = ', area/width
        WRITE(6,110) '# sum(counts) = ', sum(counts)
        WRITE(6,200) '# sum(ncounts)*width = ', sum(ncounts)*width

        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

        DEALLOCATE( vector )
        DEALLOCATE( counts )
        DEALLOCATE( ncounts )
        DEALLOCATE( center )

110     FORMAT(1x,a40,1(1x,i10))
200     FORMAT(1x,a40,1(1x,e20.10))
300     FORMAT(1x,i6,1x,e20.10,1x,i20,1x,e20.10)
400     FORMAT(1x,a40,1(1x,i10),1(1x,f10.3))

        END SUBROUTINE binning

        END MODULE binning_mod
