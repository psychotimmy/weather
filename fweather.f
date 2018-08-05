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
