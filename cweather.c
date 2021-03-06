/**********************************************************************/
/*	        						      */
/* Raspberry Pi weather station on 20x4 lcd using bme280 sensor (I2C) */
/* Tim Holyoake	19/07/2018					      */
/* Last updated	29/08/2019 					      */
/*								      */
/**********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include <unistd.h>
#include <errno.h>

#include <wiringPi.h>
#include <wiringPiI2C.h>
#include <pcf8574.h>
#include <lcd.h>
#include "hweather.h"

/* Global static variables that can be reset on the command line */

float altAboveSeaLevel = 117.0;     // -a <value>
char *filename = "readings.txt";    // -f <value>
int   delayTime = 60000;            // -d <value> - debugging use only!

/* End global statics */

void printToLCD(int fd, int cpos, int line, char *str) {
/*******************************************************/
/*                                                     */
/* Used to write the contents of str to the LCD screen */
/* at line line starting from cursor position cpos     */
/*                                                     */
/* Pre-requiste - lcdInit must have been called and    */
/*                returned a valid handlle, fd.        */
/*                                                     */
/* TJH 21-07-2018                                      */
/*                                                     */
/*******************************************************/
   if (fd >=0) {
      lcdPosition(fd,cpos,line);
      lcdPrintf(fd,str);
   } else {
      fprintf(stderr, "printToLCD failed with invalid handle\n");
      exit(1);
   }
}

char *getLocalTime(char *format) {
/*******************************************************/
/*                                                     */
/* Used to return the local date and/or time.          */
/*                                                     */
/* format can be LCD, ZAM or any other string          */
/*                                                     */
/* LCD returns the string Time hh:mm                   */
/* ZAM returns the integer month number as a string in */
/* the range 1 to 12 (January to December)             */
/* otherwise the string dd:mm:yyyy:hh:mm is returned   */
/*                                                     */
/* TJH 30-08-2018                                      */
/*     28-08-2019  ZAM format added                    */
/*                                                     */
/*******************************************************/
   time_t utctime;
   struct tm *local;
   static char timestr[80];
   time(&utctime);		
   local = localtime(&utctime);	
   if (strcmp(format,"LCD") == 0) {
      sprintf(timestr,"Time %02d:%02d",local->tm_hour,local->tm_min);
   } else if (strcmp(format,"ZAM") == 0) {
      sprintf(timestr,"%d", local->tm_mon+1);
   }
   else {
      sprintf(timestr,"%02d:%02d:%04d:%02d:%02d", 
              local->tm_mday,
              local->tm_mon+1,
              local->tm_year+1900,
              local->tm_hour,
              local->tm_min);
   }

   return (timestr);
}

void readCalibrationData(int fd, bme280_cal *data) {
/******************************************************/
/*                                                    */
/* Read the calibration data from the BME280          */
/*                                                    */
/* Adapted from Bosch Sensortec BME280 sensor driver  */
/*                                                    */
/* TJH 26-07-2018                                     */
/*                                                    */
/******************************************************/
   data->t1 = (uint16_t)wiringPiI2CReadReg16(fd, BME280_T1);
   data->t2 = (int16_t)wiringPiI2CReadReg16(fd, BME280_T2);
   data->t3 = (int16_t)wiringPiI2CReadReg16(fd, BME280_T3);

   data->p1 = (uint16_t)wiringPiI2CReadReg16(fd, BME280_P1);
   data->p2 = (int16_t)wiringPiI2CReadReg16(fd, BME280_P2);
   data->p3 = (int16_t)wiringPiI2CReadReg16(fd, BME280_P3);
   data->p4 = (int16_t)wiringPiI2CReadReg16(fd, BME280_P4);
   data->p5 = (int16_t)wiringPiI2CReadReg16(fd, BME280_P5);
   data->p6 = (int16_t)wiringPiI2CReadReg16(fd, BME280_P6);
   data->p7 = (int16_t)wiringPiI2CReadReg16(fd, BME280_P7);
   data->p8 = (int16_t)wiringPiI2CReadReg16(fd, BME280_P8);
   data->p9 = (int16_t)wiringPiI2CReadReg16(fd, BME280_P9);

   data->h1 = (uint8_t)wiringPiI2CReadReg8(fd, BME280_H1);
   data->h2 = (int16_t)wiringPiI2CReadReg16(fd, BME280_H2);
   data->h3 = (uint8_t)wiringPiI2CReadReg8(fd, BME280_H3);
   data->h4 = (wiringPiI2CReadReg8(fd, BME280_H4) << 4)   | (wiringPiI2CReadReg8(fd, BME280_H4+1) & 0xF);
   data->h5 = (wiringPiI2CReadReg8(fd, BME280_H5+1) << 4) | (wiringPiI2CReadReg8(fd, BME280_H5) >> 4);
   data->h6 = (int8_t)wiringPiI2CReadReg8(fd, BME280_H6);

   return;
}

int32_t getTemperatureCalibration(bme280_cal *cal, int32_t temp) {
/******************************************************/
/*                                                    */
/* Read the temperature data from the BME280          */
/*                                                    */
/* Adapted from Bosch Sensortec BME280 sensor driver  */
/*                                                    */
/* TJH 26-07-2018                                     */
/*                                                    */
/******************************************************/
   int32_t var1, var2;
   var1  = ((((temp>>3) - ((int32_t)cal->t1 <<1))) *
           ((int32_t)cal->t2)) >> 11;

   var2  = (((((temp>>4) - ((int32_t)cal->t1)) *
           ((temp>>4) - ((int32_t)cal->t1))) >> 12) *
           ((int32_t)cal->t3)) >> 14;

   return (var1 + var2);
}

float compensatePressure(uint32_t uncomp_press, bme280_cal *dev, int32_t t_fine)
/******************************************************/
/*                                                    */
/* Convert the pressure read from the BME280 into hPa */
/*                                                    */
/* Adapted from Bosch Sensortec BME280 sensor driver  */
/*                                                    */
/* Note - this is the absolute pressure read by the   */
/* device. A further conversion is required to adjust */
/* the value returned into surface (sea level)        */
/* pressure used on weather charts                    */
/*                                                    */
/* TJH 26-07-2018                                     */
/*                                                    */
/******************************************************/
{
   int64_t pressure;
   int64_t var1;
   int64_t var2;

   pressure = 0;
   var1 = ((int64_t) t_fine) - 128000;
   var2 = var1 * var1 * (int64_t) dev->p6;
   var2 = var2 + ((var1 * (int64_t) dev->p5) << 17);
   var2 = var2 + (((int64_t) dev->p4) << 35);
   var1 = ((var1 * var1 * (int64_t) dev->p3) >> 8) + ((var1 * (int64_t) dev->p2) << 12);
   var1 = ((INT64_C(0x800000000000) + var1) * ((int64_t) dev->p1)) >> 33;
   if (var1 != 0) {
      pressure = 1048576 - uncomp_press;
      pressure = (((pressure << 31) - var2) * 3125) / var1;
      var1 = (((int64_t) dev->p9) * (pressure >> 13) * (pressure >> 13)) >> 25;
      var2 = (((int64_t) dev->p8) * pressure) >> 19;
      pressure = (((pressure + var1 + var2) >> 8) + (((int64_t) dev->p7) << 4)) >> 8;
   } else {
	pressure = 0;
   }
        
   // Divide by 100 to return pressure in hPa (millibars)
   return ((float) pressure/100);
}

float compensateHumidity(int32_t adc_H, bme280_cal *cal, int32_t t_fine) {
/******************************************************/
/*                                                    */
/* Convert the humidity read from the BME280 into %RH */
/*                                                    */
/* Adapted from Bosch Sensortec BME280 sensor driver  */
/*                                                    */
/* TJH 26-07-2018                                     */
/*                                                    */
/******************************************************/
   int32_t v_x1_u32r;
   float h;

   v_x1_u32r = (t_fine - ((int32_t)76800));

   v_x1_u32r = (((((adc_H << 14) - (((int32_t)cal->h4) << 20) -
       (((int32_t)cal->h5) * v_x1_u32r)) + ((int32_t)16384)) >> 15) *
          (((((((v_x1_u32r * ((int32_t)cal->h6)) >> 10) *
         (((v_x1_u32r * ((int32_t)cal->h3)) >> 11) + ((int32_t)32768))) >> 10) +
       ((int32_t)2097152)) * ((int32_t)cal->h2) + 8192) >> 14));

   v_x1_u32r = (v_x1_u32r - (((((v_x1_u32r >> 15) * (v_x1_u32r >> 15)) >> 7) *
            ((int32_t)cal->h1)) >> 4));

   v_x1_u32r = (v_x1_u32r < 0) ? 0 : v_x1_u32r;
   v_x1_u32r = (v_x1_u32r > 419430400) ? 419430400 : v_x1_u32r;
   h = (v_x1_u32r>>12)/1024.0;
   return  (h);
}

void getRawData(int fd, bme280_raw *raw) {
/******************************************************/
/*                                                    */
/* Get the raw data from the BME280                   */
/*                                                    */
/* Adapted from Bosch Sensortec BME280 sensor driver  */
/*                                                    */
/* TJH 26-07-2018                                     */
/*                                                    */
/******************************************************/
  wiringPiI2CWrite(fd, BME280_PRESSDATA);

  raw->pmsb = wiringPiI2CRead(fd);
  raw->plsb = wiringPiI2CRead(fd);
  raw->pxsb = wiringPiI2CRead(fd);

  raw->tmsb = wiringPiI2CRead(fd);
  raw->tlsb = wiringPiI2CRead(fd);
  raw->txsb = wiringPiI2CRead(fd);

  raw->hmsb = wiringPiI2CRead(fd);
  raw->hlsb = wiringPiI2CRead(fd);

  raw->temperature = (0 | raw->tmsb) << 8;
  raw->temperature = (raw->temperature | raw->tlsb) << 8;
  raw->temperature = (raw->temperature | raw->txsb) >> 4;

  raw->pressure = (0 | raw->pmsb) << 8;
  raw->pressure = (raw->pressure | raw->plsb) << 8;
  raw->pressure = (raw->pressure | raw->pxsb) >> 4;

  raw->humidity = (0 | raw->hmsb) << 8;
  raw->humidity = (raw->humidity | raw->hlsb);

  return;
}

void makeForecast(int fd,float pdiff) {
/******************************************************/
/*                                                    */
/* Wrapper for FORTRAN forecast model - only pressure */
/* changes are used at present to make the prediction */
/*                                                    */
/* TJH 26-07-2018                                     */
/*                                                    */
/******************************************************/
   char indicator[3];
   char forecast[21];
   extern void mfcast_();

   mfcast_(&pdiff, &indicator, &forecast); 
   // Null terminate the strings - FORTRAN doesn't do/need this!
   indicator[2]='\0';
   forecast[20]='\0';
   printToLCD(fd,18,2,indicator);
   printToLCD(fd,0,3,forecast);
   return;
}

char *makeForecastZambretti(int fd,float pdiff, float press) {
/******************************************************/
/*                                                    */
/* Wrapper for FORTRAN Zambretti forecast model       */
/* Requires currrent pressure and differences over 3  */
/* hours for a prediction                             */
/*                                                    */
/* TJH 28-08-2019                                     */
/*                                                    */
/******************************************************/
   int imonth;
   char *strtemp;
   char indicator[3];
   static char forecast[21];
   extern void zfcast_();

   // Get the month
   strtemp=getLocalTime("ZAM");	  	// get the month for the forecast
   imonth=atoi(strtemp);

   zfcast_(&imonth, &press, &pdiff, &indicator, &forecast); 

   // Null terminate the strings - FORTRAN doesn't do/need this!
   indicator[2]='\0';
   forecast[20]='\0';
   printToLCD(fd,18,2,indicator);
   printToLCD(fd,0,3,forecast);
   return(forecast);
}

void usage() {
/******************************************************/
/*                                                    */
/* Usage hints for command line arguments.            */
/*                                                    */
/* TJH 06-08-2018                                     */
/*                                                    */
/******************************************************/
   fprintf(stderr, "\n" \
      "Usage: weather [option] ...\n" \
      "   -d value, delay between readings in milliseconds (debugging only - 100 to 60000 is valid range)   default 60000 (1 minute) \n" \
      "   -f value, filename for 15 minute readings to be stored                                            default readings.txt \n" \
      "   -a value, altitude above sea level in metres                                                      default 117.0 \n\n");
}

int getNum(char *str, int *err) {
/******************************************************/
/*                                                    */
/* Get the numerical value of a command line argument */
/*                                                    */
/* TJH 06-08-2018                                     */
/*                                                    */
/******************************************************/
   int val;
   char *ptr;

   *err = 0;
   val = strtoll(str, &ptr, 0);
   if (*ptr) {
      *err = 1; 
      val = -1;
   }
   return (val);
}

void parseCmdLine(int argc, char *argv[]) {
/******************************************************/
/*                                                    */
/* Parse and act on any command line arguments.       */
/*                                                    */
/* TJH 06-08-2018                                     */
/*                                                    */
/******************************************************/

   int opt, err, i;

   while ((opt = getopt(argc, argv, "a:d:f:")) != -1)

   {
      switch (opt)
      {
         case 'f':
            filename = argv[optind-1];
            break;
            
         case 'd':
            i = getNum(optarg, &err);
            if ((i >= 100) && (i <= 60000)) delayTime = i;
            break;

         case 'a':
            i = getNum(optarg, &err);
            altAboveSeaLevel = i;
            break;

        default: /* '?' */
           usage();
           exit(-1);
       }
   }
   return;
}

int main(int argc, char *argv[]) {
/******************************************************/
/*                                                    */
/* Main programme loop - initiatise the I2C devices,  */
/* Read and covnert data from the BME280,             */
/* Store 3 hours of pressure data and make forecast,  */
/* Write to LCD and repeat every minute.              */
/*                                                    */
/* TJH 06-08-2018                                     */
/*                                                    */
/******************************************************/
   int i,j,ptrNow,ptrThen,lcdfd,bme280fd;
   int32_t t_fine;
   char *str,*strt,*fstr;
   char ftemp[21];
   fstr = &ftemp[0];
   float threeHours[180],threeHoursDiff;
   float t,p,h,d;
   bme280_cal cal;
   bme280_raw raw;
   FILE *filefd;
   extern float rdewpt_();
   extern float spress_();
   extern float comtem_();

   parseCmdLine(argc, argv);

   // Initialise wiringPi library
   if(wiringPiSetup() == -1){ //when initialise wiringPi failed print message to screen
      fprintf(stderr, "wiringPi initialisation failed\n");
      exit(1); 
   }

   pcf8574Setup(BASE,PCF8574);     // initialise PCF8574
   for(i=0; i<8; i++){
      pinMode(BASE+i,OUTPUT);      // set PCF8574 port to output mode
   } 

   // Initialise the LCD and return a handle to it
   lcdfd = lcdInit(LCDROWS,LCDCOLS,LCDBITS,RS,EN,D4,D5,D6,D7,0,0,0,0);
   if(lcdfd <0){
      fprintf(stderr, "LCD display initalisation failed\n");
      exit(1);
   }

   // Initialise the BME280 sensor and return a handle
   bme280fd = wiringPiI2CSetup(BME280_ADDRESS);
   if(bme280fd < 0) {
      fprintf(stderr, "BME280 sensor initialisation failed\n");
      exit(1);
   }

   digitalWrite(LED,HIGH);         // turn on LCD backlight
   digitalWrite(RW,LOW);           // allow writing to LCD

   readCalibrationData(bme280fd, &cal);             // read the BME280 calibration data

   j=0;    // Pointer to location in threeHours circular buffer - pressure records

   while(TRUE) {
      // Read the BME 280 sensor
      wiringPiI2CWriteReg8(bme280fd, BME280_CONTROLHUMID, 0x01);   // humidity oversampling x 1
      wiringPiI2CWriteReg8(bme280fd, BME280_CONTROL, 0x25);        // pressure and temperature oversampling x 1, normal mode
      getRawData(bme280fd, &raw); 

      // Calibrate the returned data
      t_fine = getTemperatureCalibration(&cal, raw.temperature);
      t = comtem_(&t_fine);    							// Temperature in Celsius
      p = compensatePressure(raw.pressure, &cal, t_fine);         		// Absolute pressure in millibars (hPa)
      h = compensateHumidity(raw.humidity, &cal, t_fine);       		// Relative humidity %
      d = rdewpt_(&t,&h);							// Dewpoint

      // Convention requires absolute pressure to be converted to sea level pressure for weather forecasting
          
      p = spress_(&p,&altAboveSeaLevel,&t);

      lcdClear(lcdfd);        		// clear the display
      str=getLocalTime("LCD");	  	// get the local time for the LCD
      printToLCD(lcdfd,0,0,str); 	// print local time
      sprintf(str,"Temp %.1fC",t);      // print temperature
      printToLCD(lcdfd,0,1,str);            
      sprintf(str,"RH  %.1f",h);        // print relative humidity
      strcat(str,"%%");
      printToLCD(lcdfd,11,0,str);
      sprintf(str,"Dew %.1fC",d);       // print dew point
      printToLCD(lcdfd,11,1,str);
      sprintf(str,"Pressure %.1fmb",p);	// print sea level pressure
      printToLCD(lcdfd,0,2,str);
      
      if (j < 180) {                       // don't produce a forecast if less than 3 hours of data available
         threeHours[j]=p;
         threeHoursDiff = 0.0;
         sprintf(fstr,"Forecast in %d mins",180-j);
         printToLCD(lcdfd,0,3,fstr);
      } else {
         ptrNow=j%180;                         // calculate the pointer to the 3 hour circular buffer
         ptrThen=(j+1)%180;
         threeHours[ptrNow]=p;
         threeHoursDiff=threeHours[ptrNow]-threeHours[ptrThen];
         /* makeForecast(lcdfd,threeHoursDiff); */
         fstr=makeForecastZambretti(lcdfd,threeHoursDiff,p);
      }

      // write csv data to file every 15 intervals (minutes when not in debug mode) - can be used as input to programs like gnuplot

      if (j%15 == 0) {
         filefd=fopen(filename,"a");
         if (filefd != NULL) {
            strt=getLocalTime("FULL");     // get the full date and local time
            sprintf(str,"%s,%.1f,%.1f,%.1f,%.1f,%.2f,%s\n",strt,t,d,h,p,threeHoursDiff,fstr);
            fputs(str,filefd);
            fclose(filefd);
         }
         else {
            fprintf(stderr,"Failed to open %s - no data logged\n",filename);
         }
      }
      
      ++j;                      // increment the pointer to the pressure buffer
      delay(delayTime);         // get the next readings in one minute (or less, if -d option used for debugging)
   }
   return (0);
}
