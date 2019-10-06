      REAL FUNCTION RDEWPT(RTEMP,RHUM)
C----------------------------------------------------------------------C
C                                                                      C
C     DEWPOINT CALCULATION FROM TEMPERATURE IN CELSIUS - RTEMP         C
C                               RELATIVE HUMIDITY - RHUM               C
C     USING THE MAGNUS FORMULA WITH CONSTANTS SUITABLE FOR A           C
C     TEMPERATURE RANGE OF -45 TO 60 CELSIUS                           C
C                                                                      C
C     AUTHOR: TJH 28-01-2017                                           C
C                                                                      C
C----------------------------------------------------------------------C
      REAL RH
C
      RH=(LOG10(RHUM)-2)/0.4343+(17.62*RTEMP)/(243.12+RTEMP)
      RDEWPT=243.12*RH/(17.62-RH)
      RETURN
      END

      REAL FUNCTION SPRESS(RHPA, RALT, RTMP)
C----------------------------------------------------------------------C
C                                                                      C
C     ADJUST ABSOLUTE PRESSURE IN hPa (MB) TO SURFACE PRESSURE         C
C     FOR USE IN WEATHER FORECASTING                                   C
C                                                                      C
C     RHPA - ABSOLUTE PRESSURE                                         C
C     RALT - HEIGHT ABOVE SEA LEVEL                                    C
C     RTMP - CURRENT TEMPERATURE                                       C
C                                                                      C
C     AUTHOR: TJH 21-07-2018                                           C
C                                                                      C
C----------------------------------------------------------------------C
      SPRESS=RHPA*(1-(0.0065*RALT)/(RTMP+0.0065*RALT+273.15))**(-5.257)
      RETURN
      END

      REAL FUNCTION COMTEM(ITEMPF)
C----------------------------------------------------------------------C
C                                                                      C
C     CONVERT BME280 TEMPERATURE TO CELSIUS                            C
C                                                                      C
C     ITEMPF - THE VALUE RETURNED FROM THE BME280 TO CONVERT           C
C                                                                      C
C     AUTHOR: TJH 26-07-2018                                           C
C                                                                      C
C----------------------------------------------------------------------C
      COMTEM=((ITEMPF*5+128)/256)/100.0
      RETURN
      END

      SUBROUTINE MFCAST(PDIFF,CIND,CFORE)
C----------------------------------------------------------------------C
C                                                                      C
C     MAKE FORECAST BASED ON 3 HOURLY PRESSURE DIFFERENCE              C
C                                                                      C
C     PDIFF - PRESSURE DIFFERENCE IN LAST THREE HOURS                  C
C     CIND  - CHANGE INDICATOR STRING                                  C
C     CFORE - FORECAST STRING                                          C
C                                                                      C
C     AUTHOR: TJH 26-07-2018                                           C
C                                                                      C
C----------------------------------------------------------------------C
      REAL PDIFF
      CHARACTER*2 CIND
      CHARACTER*20 CFORE
C
      IF (PDIFF.LE.-0.5 .AND. PDIFF.GE.-3.0) THEN
         CIND="v "
         CFORE=" Some rain possible "
      ELSE IF (PDIFF.LT.-3.0 .AND. PDIFF.GE.-6.0) THEN
         CIND="vv"
         CFORE="Wind,rain;fine later"
      ELSE IF (PDIFF.LT.-6.0) THEN
         CIND="vv"
         CFORE="    ** STORMY **    "
      ELSE IF (PDIFF.GE.0.5 .AND. PDIFF.LE.6.0) THEN 
         CIND="^ "
         CFORE="Fine, becoming drier"
      ELSE IF (PDIFF.GT.6.0) THEN
         CIND="^^"
         CFORE="Becoming dry & windy"
      ELSE
         CIND="--"
         CFORE="No change in weather"
      END IF
      RETURN
      END

      SUBROUTINE ZFCAST(IMON,PRESS,PDIFF,CIND,CFORE)
C----------------------------------------------------------------------C
C                                                                      C
C     MAKE FORECAST BASED ON 3 HOURLY PRESSURE DIFFERENCE USING AN     C
C     ALGORITHM BASED ON THE ZAMBRETTI FORECASTER                      C
C                                                                      C
C     IMON  - MONTH NUMBER                                             C
C     PRESS - CURRENT PRESSURE IN MILLIBARS, ADJUSTED FOR SEA LEVEL    C
C     PDIFF - PRESSURE DIFFERENCE IN LAST THREE HOURS                  C
C     CIND  - CHANGE INDICATOR STRING                                  C
C     CFORE - FORECAST STRING                                          C
C                                                                      C
C     AUTHOR: TJH 30-08-2019                                           C
C                                                                      C
C----------------------------------------------------------------------C
      INTEGER IMON
      REAL PRESS
      REAL PDIFF
      CHARACTER*2 CIND
      CHARACTER*20 CFORE
      INTEGER IZ
      CHARACTER CZFALL(9)*20, CZSTDY(10)*20, CZRISE(13)*20
      INTEGER IPFALL(9), IPSTDY(10), IPRISE(13)
      COMMON /ZAMBRT/ CZFALL, CZSTDY, CZRISE, IPFALL, IPSTDY, IPRISE
C
C     CALCULATE FORECAST MESSAGE LOCATION (IZ) IN CZFOR BY USING THE
C     CURRENT PRESSURE AND PRESSURE TREND
C
      IF (PDIFF.LE.-1.25) THEN
C
C        PRESSURE IS FALLING
C
         CIND="vv"
         IF (PRESS.GE.IPFALL(1)) THEN
            IZ=1
         ELSE IF (PRESS.GE.IPFALL(2)) THEN
            IZ=2
         ELSE IF (PRESS.GE.IPFALL(3)) THEN
            IZ=3
         ELSE IF (PRESS.GE.IPFALL(4)) THEN
            IZ=4
         ELSE IF (PRESS.GE.IPFALL(5)) THEN
            IZ=5
         ELSE IF (PRESS.GE.IPFALL(6)) THEN
            IZ=6
         ELSE IF (PRESS.GE.IPFALL(7)) THEN
            IZ=7
         ELSE IF (PRESS.GE.IPFALL(8)) THEN
            IZ=8
         ELSE
            IZ=9
         END IF
C 
C        IF IT IS WINTER AND PRESSURE IS FALLING THEN THE
C        THE FORECAST IS 1 STEP WORSE (IZ+1) THAN 
C        THE CURRENT PRESSURE ALONE PREDICTS.
C        WINTER IS DECEMBER, JANUARY AND FEBRUARY.
C
         IF (IMON.EQ.12 .OR. IMON.LE.2) THEN
            IZ=IZ+1
C
C           CHECK IZ NOT OUT OF RANGE - SHOULDN'T HAPPEN, BUT NEITHER
C           SHOULD MANMADE CLIMATE CHANGE
C
            IF (IZ.GT.9) THEN 
               IZ=9
            END IF
         END IF
C
C        SET THE FORECAST STRING
C
         CFORE=CZFALL(IZ)
      ELSE IF (PDIFF.GE.1.25) THEN
C
C        PRESSURE IS RISING
C
         CIND="^^"
         IF (PRESS.GE.IPRISE(1)) THEN
            IZ=1
         ELSE IF (PRESS.GE.IPRISE(2)) THEN
            IZ=2
         ELSE IF (PRESS.GE.IPRISE(3)) THEN
            IZ=3
         ELSE IF (PRESS.GE.IPRISE(4)) THEN
            IZ=4
         ELSE IF (PRESS.GE.IPRISE(5)) THEN
            IZ=5
         ELSE IF (PRESS.GE.IPRISE(6)) THEN
            IZ=6
         ELSE IF (PRESS.GE.IPRISE(7)) THEN
            IZ=7
         ELSE IF (PRESS.GE.IPRISE(8)) THEN
            IZ=8
         ELSE IF (PRESS.GE.IPRISE(9)) THEN
            IZ=9
         ELSE IF (PRESS.GE.IPRISE(10)) THEN
            IZ=10
         ELSE IF (PRESS.GE.IPRISE(11)) THEN
            IZ=11
         ELSE IF (PRESS.GE.IPRISE(12)) THEN
            IZ=12
         ELSE
            IZ=13
         END IF
C 
C        IF IT IS SUMMER AND PRESSURE IS RISING THEN THE
C        THE FORECAST IS 1 STEP BETTER (IZ-1) THAN 
C        THE CURRENT PRESSURE ALONE PREDICTS.
C        SUMMER IS JUNE, JULY AND AUGUST.
C
         IF (IMON.GE.6 .AND. IMON.LE.8) THEN
            IZ=IZ-1
C
C           CHECK IZ NOT OUT OF RANGE - SHOULDN'T HAPPEN, BUT NEITHER
C           SHOULD MANMADE CLIMATE CHANGE
C
            IF (IZ.LT.1) THEN 
               IZ=1
            END IF
         END IF
C
C        SET THE FORECAST STRING
C
         CFORE=CZRISE(IZ)
      ELSE
C
C        PRESSURE IS STEADY
C
         CIND="--"
         IF (PRESS.GE.IPSTDY(1)) THEN
            IZ=1
         ELSE IF (PRESS.GE.IPSTDY(2)) THEN
            IZ=2
         ELSE IF (PRESS.GE.IPSTDY(3)) THEN
            IZ=3
         ELSE IF (PRESS.GE.IPSTDY(4)) THEN
            IZ=4
         ELSE IF (PRESS.GE.IPSTDY(5)) THEN
            IZ=5
         ELSE IF (PRESS.GE.IPSTDY(6)) THEN
            IZ=6
         ELSE IF (PRESS.GE.IPSTDY(7)) THEN
            IZ=7
         ELSE IF (PRESS.GE.IPSTDY(8)) THEN
            IZ=8
         ELSE IF (PRESS.GE.IPSTDY(9)) THEN
            IZ=9
         ELSE
            IZ=10
         END IF
C
C        SET THE FORECAST STRING
C
         CFORE=CZSTDY(IZ)
      END IF
C
C     DEBUG ONLY
C     WRITE (*,*) PRESS, " ", IMON, " ", IZ, " ", CIND, " ", CFORE
C
      RETURN
      END

      BLOCK DATA
C----------------------------------------------------------------------C
C                                                                      C
C     INITIALISE THE COMMON BLOCK FOR THE ZAMBRETTI FORECAST TEXTS     C
C     MODIFIED TO FIT INTO 20 CHARACTERS DUE TO THE 4 LINE LCD DISPLAY C
C     BEING USED. THE IPXXXX ARRAYS ARE THE THRESHOLDS FOR EACH        C
C     FORECAST IN MILLIBARS BEFORE CORRECTING FOR SEASON AND WIND      C
C     DIRECTION                                                        C
C                                                                      C
C     AUTHOR: TJH 30-08-2019                                           C
C                                                                      C
C----------------------------------------------------------------------C
      CHARACTER CZFALL(9)*20, CZSTDY(10)*20, CZRISE(13)*20
      INTEGER   IPFALL(9), IPSTDY(10), IPRISE(13)

      COMMON /ZAMBRT/ CZFALL, CZSTDY, CZRISE, IPFALL, IPSTDY, IPRISE

      DATA CZFALL(1) /'Settled Fine       '/
      DATA CZFALL(2) /'Fine Weather       '/
      DATA CZFALL(3) /'Fine : Less Settled'/
      DATA CZFALL(4) /'Fairly Fine Showery'/
      DATA CZFALL(5) /'Showery : Unsettled'/
      DATA CZFALL(6) /'Unsettled : Rain   '/
      DATA CZFALL(7) /'Some rain : Worse  '/
      DATA CZFALL(8) /'Rain : Unsettled   '/
      DATA CZFALL(9) /'V. Unsettled : Rain'/

      DATA IPFALL /1050,1040,1024,1018,1010,1004,998,991,985/

      DATA CZSTDY(1) /'Settled Fine       '/
      DATA CZSTDY(2) /'Fine Weather       '/
      DATA CZSTDY(3) /'Fine : Showers Poss'/
      DATA CZSTDY(4) /'Fair : Showers Lkly'/
      DATA CZSTDY(5) /'Showers : Bright In'/
      DATA CZSTDY(6) /'Changeable : Rain  '/
      DATA CZSTDY(7) /'Unsettled : Rain   '/
      DATA CZSTDY(8) /'Frequent Rain      '/
      DATA CZSTDY(9) /'V. Unsettled : Rain'/
      DATA CZSTDY(10) /'Stormy : Much Rain '/

      DATA IPSTDY /1033,1023,1014,1008,1000,994,989,981,974,960/

      DATA CZRISE(1) /'Settled Fine       '/
      DATA CZRISE(2) /'Fine Weather       '/
      DATA CZRISE(3) /'Becoming Fine      '/
      DATA CZRISE(4) /'Fairly Fine Imprvng'/
      DATA CZRISE(5) /'Fairly Fine Showers'/
      DATA CZRISE(6) /'Showery : Improving'/
      DATA CZRISE(7) /'Changeable : Mendng '/
      DATA CZRISE(8) /'Unsettled : Clearng'/
      DATA CZRISE(9) /'Unsettled : May Imp'/
      DATA CZRISE(10) /'Unsettled : Fine In'/
      DATA CZRISE(11) /'Very Unsettled     '/
      DATA CZRISE(12) /'Stormy : Poss Imprv'/
      DATA CZRISE(13) /'Stormy : Much Rain '/

      DATA IPRISE 
     +/1030,1022,1012,1007,1000,995,990,984,978,970,965,959,947/

      END
