      PROGRAM ZAMTST
C----------------------------------------------------------------------C
C                                                                      C
C     ZAMBRETTI FORECASTER TEST HARNESS                                C
C                                                                      C
C     AUTHOR: TJH 30-08-2019                                           C
C                                                                      C
C----------------------------------------------------------------------C
      INTEGER IMONTH, ILOOP
      REAL PRESS, PDIFF
      CHARACTER*2 CIND
      CHARACTER*20 CFORE
C
C     MONTH = APRIL (SPRING / AUTUMN CONDITIONS)
C
      IMONTH = 4
C
C     FALLING PRESSURE
C
      PDIFF = -2.0
      WRITE(*,*)"------------------------"
      WRITE(*,*)"FALLING PRESSURE, SPRING"
      WRITE(*,*)"------------------------"
      DO 100 ILOOP=940,1060
         PRESS=REAL(ILOOP)
         CALL ZFCAST(IMONTH, PRESS, PDIFF, CIND, CFORE)
         WRITE(*,*) PRESS, " ", CIND, " ", CFORE
  100 CONTINUE
C
C     STEADY PRESSURE
C
      PDIFF = 0.0
      WRITE(*,*)"------------------------"
      WRITE(*,*)"STEADY PRESSURE, SPRING"
      WRITE(*,*)"------------------------"
      DO 200 ILOOP=940,1060
         PRESS=REAL(ILOOP)
         CALL ZFCAST(IMONTH, PRESS, PDIFF, CIND, CFORE)
         WRITE(*,*) PRESS, " ", CIND, " ", CFORE
  200 CONTINUE
C
C     RISING PRESSURE
C
      PDIFF = 2.0
      WRITE(*,*)"------------------------"
      WRITE(*,*)"RISING PRESSURE, SPRING"
      WRITE(*,*)"------------------------"
      DO 300 ILOOP=940,1060
         PRESS=REAL(ILOOP)
         CALL ZFCAST(IMONTH, PRESS, PDIFF, CIND, CFORE)
         WRITE(*,*) PRESS, " ", CIND, " ", CFORE
  300 CONTINUE
C
C     MONTH = JANUARY (WINTER CONDITIONS)
C
      IMONTH = 1
C
C     FALLING PRESSURE
C
      PDIFF = -2.0
      WRITE(*,*)"------------------------"
      WRITE(*,*)"FALLING PRESSURE, WINTER"
      WRITE(*,*)"------------------------"
      DO 400 ILOOP=940,1060
         PRESS=REAL(ILOOP)
         CALL ZFCAST(IMONTH, PRESS, PDIFF, CIND, CFORE)
         WRITE(*,*) PRESS, " ", CIND, " ", CFORE
  400 CONTINUE
C
C     STEADY PRESSURE
C
      PDIFF = 0.0
      WRITE(*,*)"------------------------"
      WRITE(*,*)"STEADY PRESSURE, WINTER"
      WRITE(*,*)"------------------------"
      DO 500 ILOOP=940,1060
         PRESS=REAL(ILOOP)
         CALL ZFCAST(IMONTH, PRESS, PDIFF, CIND, CFORE)
         WRITE(*,*) PRESS, " ", CIND, " ", CFORE
  500 CONTINUE
C
C     RISING PRESSURE
C
      PDIFF = 2.0
      WRITE(*,*)"------------------------"
      WRITE(*,*)"RISING PRESSURE, WINTER"
      WRITE(*,*)"------------------------"
      DO 600 ILOOP=940,1060
         PRESS=REAL(ILOOP)
         CALL ZFCAST(IMONTH, PRESS, PDIFF, CIND, CFORE)
         WRITE(*,*) PRESS, " ", CIND, " ", CFORE
  600 CONTINUE
C
C     MONTH = JULY (SUMMER CONDITIONS)
C
      IMONTH = 7
C
C     FALLING PRESSURE
C
      PDIFF = -2.0
      WRITE(*,*)"------------------------"
      WRITE(*,*)"FALLING PRESSURE, SUMMER"
      WRITE(*,*)"------------------------"
      DO 700 ILOOP=940,1060
         PRESS=REAL(ILOOP)
         CALL ZFCAST(IMONTH, PRESS, PDIFF, CIND, CFORE)
         WRITE(*,*) PRESS, " ", CIND, " ", CFORE
  700 CONTINUE
C
C     STEADY PRESSURE
C
      PDIFF = 0.0
      WRITE(*,*)"------------------------"
      WRITE(*,*)"STEADY PRESSURE, SUMMER"
      WRITE(*,*)"------------------------"
      DO 800 ILOOP=940,1060
         PRESS=REAL(ILOOP)
         CALL ZFCAST(IMONTH, PRESS, PDIFF, CIND, CFORE)
         WRITE(*,*) PRESS, " ", CIND, " ", CFORE
  800 CONTINUE
C
C     RISING PRESSURE
C
      PDIFF = 2.0
      WRITE(*,*)"------------------------"
      WRITE(*,*)"RISING PRESSURE, SUMMER"
      WRITE(*,*)"------------------------"
      DO 900 ILOOP=940,1060
         PRESS=REAL(ILOOP)
         CALL ZFCAST(IMONTH, PRESS, PDIFF, CIND, CFORE)
         WRITE(*,*) PRESS, " ", CIND, " ", CFORE
  900 CONTINUE
      STOP
      END
