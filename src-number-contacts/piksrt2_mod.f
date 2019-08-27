        MODULE piksrt2_mod

        USE common_data_mod

        IMPLICIT NONE

        CONTAINS

        SUBROUTINE piksrt2(nn,arr1,arr2)
! sort arr1 and arr2 based on arr1

        INTEGER :: nn
        REAL(KIND=dp), DIMENSION(nn) :: arr1
        INTEGER, DIMENSION(nn) :: arr2

        REAL(KIND=dp) :: aa
        INTEGER :: ii
        INTEGER :: i,j

        DO j=2,nn
          aa=arr1(j)
          ii=arr2(j)
          DO i=j-1,1,-1
            IF (arr1(i) .le. aa) GOTO 10
            arr1(i+1)=arr1(i)
            arr2(i+1)=arr2(i)
          END DO
          i=0
10        arr1(i+1)=aa
          arr2(i+1)=ii
        END DO

        END SUBROUTINE piksrt2

        END MODULE piksrt2_mod
