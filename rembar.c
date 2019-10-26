/***********************************************************************/
/*                                                                     */
/* Remote barometer and forecast display                               */
/*                                                                     */
/* Uses a 20x4 line LCD to display barometric and forecast data from   */
/* a remote raspberry pi using my Zambretti forecaster code.           */
/*                                                                     */
/* The I2C interface on the raspberry pi is used and the wiringPi      */
/* library is required to have been installed (code tested with v2.52) */
/*                                                                     */
/* The I2C expander (backpack) for the LCD must be a Texas Instruments */
/* PCF8574 or PCF8574A or compatible. The controller for the LCD       */
/* display must be a Hitachi HD44780U or compatible.                   */
/*                                                                     */
/* To compile: gcc rembar.c -o rembar -lwiringPi -lwiringPiDev -Wall   */
/*                                                                     */
/* Tim Holyoake, 6th October 2019                                      */
/*               Last updated 26th October 2019                        */
/*                                                                     */
/***********************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <wiringPi.h>
#include <pcf8574.h>
#include <lcd.h>
#include <time.h>

#define I2CADDR 	0x27        	// default I2C address of PCF8574. This is 0x3A for a PCF8574A
#define BASE 		64

#define LCDROWS         4               // number of rows on the LCD
#define LCDCOLS         20              // number of columns on the LCD
#define LCDBITS         4               // number of bits for the display mode

/* Define the output pins of the PCF8574, which are directly connected to the LCD2004 pins */
/* This code uses the display in 4 bit mode                                                */

#define RS      BASE+0                  // RS pin
#define RW      BASE+1                  // If set LOW allows writing to the LCD
#define EN      BASE+2                  // Strobe pin
#define LED     BASE+3                  // Backlight - LOW = off, HIGH = on
#define D4      BASE+4
#define D5      BASE+5
#define D6      BASE+6
#define D7      BASE+7

/* Define the hours at which the LCD display is turned on and off */

#define ONHOUR   7
#define OFFHOUR 22

/* Note OUTPUT and TRUE are both defined in wiringPi.h to 1 and (1==1) respectively */

/* Start global variables */

int lcdhd;				// file handle for LCD display

/* End global variables */

void getLastReading() {
/***********************************************************************/
/*                                                                     */
/* Function to get the last recorded reading from the remote Zambretti */
/* forecaster. The current implementation is far from ideal as it uses */
/* system commands to remotely create the lastreadings.txt file from   */
/* the readings.txt file on the forecaster and then copy it to the pi  */
/* running the remote LCD screen.                                      */
/*                                                                     */
/* All values are also hardcoded into this function ... but it         */
/* does work!                                                          */
/*                                                                     */
/* Tim Holyoake, 6th October 2019                                      */
/*               Modified 17th October 2019 to reduce the amount of    */
/*               data copied over the network each time this is called */
/*                                                                     */
/***********************************************************************/
    system("ssh pi@roobarb tail -1 /home/pi/zambretti/readings.txt > lastreading.txt");
}

void printBarFile() {
/***********************************************************************/
/*                                                                     */
/* Function to read and print the data from the 'lastreading.txt' file */
/* created by the getLastReading function. A 4 line, 20 column LCD is  */
/* assumed as the output device.                                       */
/*                                                                     */
/* The file format required is:                                        */
/*                                                                     */
/* dd:mm:yyyy:hh:mm,temperature,dewpoint,humidity,pressure,            */
/* pressure change in last 3 hours,forecast                            */
/*                                                                     */
/* Tim Holyoake, 6th October 2019                                      */
/*                                                                     */
/***********************************************************************/

    FILE *fp;
    char str_temp[80]=" ";
    char str_date[21]=" ";
    char str_press[21]="Pressure: ";
    char str_change[21]="Change: ";
    char str_fore[21]=" ";
    int i,j,p;

    // Read the one line of data from the lastreading.txt file

    fp=fopen("lastreading.txt","r");
    if (fp == NULL) {
       fprintf(stderr,"Unable to open lastreading.txt file\n");
       exit(1);
    }
    fgets(str_temp,80,fp);
    fclose(fp);

    // Extract the date and time from the first 16 characters of the file
    // Date is in the first 10, time is in the last 5

    // First ten characters are the date - dd:mm:yyyy
    for (i=0;i<10;i++) str_date[i]=str_temp[i];

    // Pad the output string with spaces so that the time ends on the far right 
    for (i=10;i<15;i++) str_date[i]=' ';

    // Characters 12 to 16 are the time (character 11 is ignored - it's a :)
    for (i=0;i<5;i++) str_date[15+i]=str_temp[11+i];

    // Null terminate the string
    str_date[20]='\0';

    // Skip to the pressure reading (four commas away - ignoring temperature, dew point and humidity)

    j=0;p=15;
    while (j<4) {
      if (str_temp[++p]==',') ++j;
    }
    ++p;

    // Extract the pressure reading

    // First 10 characters of this output line are fixed as "Pressure: ", so start count at 10
    i=10;

    // Pressure reading will be 5 or 6 characters, so put each character into the output buffer
    // until the next comma is found.

    if (str_temp[p] == '1') {        // If the first pressure reading character is 1, pressure reading is 6 characters
       str_press[i++] = ' ';           // A pretty display needs one extra blank if pressure >= 1000mb, two if < 1000mb
    } else {
       str_press[i++] = ' ';
       str_press[i++] = ' ';
    }
    str_press[i++]=str_temp[p++];

    while (j<5) {
      str_press[i++]=str_temp[p++];
      if (str_temp[p]==',') ++j;
    }
   
    // End the pressure string with " mb", pad with spaces to column 20 and then null termintate
    str_press[i++]=' ';
    str_press[i++]='m';
    str_press[i++]='b';
    while (i<20) str_press[i++]=' ';
    str_press[20]='\0';
    ++p;

    // Extract the pressure change

    // First 8 characters of this output line are fixed as "Change: ", so start count at 8
    i=8;

    // Pressure change will between 4 and 6 characters (x.xx to -xx.xx),
    // so put each character into the output buffer until the next comma is found
    while (j<6) {
      str_change[i++]=str_temp[p++];
      if (str_temp[p]==',') ++j;
    }

    // End the pressure change string with " mb", pad with spaces to column 20 and then null termintate
    str_change[i++]=' ';
    str_change[i++]='m';
    str_change[i++]='b';
    while (i<20) str_change[i++]=' ';
    str_change[20]='\0';
    ++p;

    // Extract the forecast

    // This is the final (up to) 20 characters of the input
    i=0;
    while(p<strlen(str_temp)) {
      str_fore[i++]=str_temp[p++];
    }

    // Pad with spaces until output buffer is 20 characters long and then null terminate
    while (i<20) str_fore[i++]=' ';
    str_fore[20]='\0';

    // Write the four output buffers to the LCD display and to standard output
   
    lcdPosition(lcdhd, 0, 0);
    lcdPrintf(lcdhd, str_date);
    //printf("%s\n",str_date); 
    lcdPosition(lcdhd, 0, 1);
    lcdPrintf(lcdhd, str_press);
    //printf("%s\n",str_press);
    lcdPosition(lcdhd, 0, 2);
    lcdPrintf(lcdhd, str_change);
    //printf("%s\n",str_change);
    lcdPosition(lcdhd, 0, 3);
    lcdPrintf(lcdhd, str_fore);
    //printf("%s\n",str_fore);
}

void clearDisplay() {
    lcdPosition(lcdhd,0,0);
    lcdPrintf(lcdhd,"                    ");
    lcdPosition(lcdhd,0,1);
    lcdPrintf(lcdhd,"                    ");
    lcdPosition(lcdhd,0,2);
    lcdPrintf(lcdhd,"                    ");
    lcdPosition(lcdhd,0,3);
    lcdPrintf(lcdhd,"                    ");
}

int getLocalTimeHour() {
/***********************************************************************/
/*                                                                     */
/* Used to return the local time - current hour                        */
/*                                                                     */
/* Tim Holyoake 11th October 2019                                      */
/*                                                                     */
/***********************************************************************/
   time_t utctime;
   struct tm *local;
   time(&utctime);		
   local = localtime(&utctime);	
   return (local->tm_hour);
}

int main(void){
    int i,backlighton,hour;
    
    // Initialise the wiringPi library
    if(wiringPiSetup() == -1){ 
        fprintf(stderr,"wiringPi failed to initialise\n");
        exit(1); 
    }

    // Initialise the I2C extender
    pcf8574Setup(BASE,I2CADDR);				
    for(i=0;i<8;i++){
        pinMode(BASE+i,OUTPUT);     			// set PCF8574 port to output mode
    } 

    digitalWrite(LED,HIGH);     			// turn on LCD backlight
    backlighton=TRUE;
    digitalWrite(RW,LOW);       			// allow writing to LCD

    // Initialise the LCD and return a handle to it
    lcdhd = lcdInit(LCDROWS,LCDCOLS,LCDBITS,RS,EN,D4,D5,D6,D7,0,0,0,0);
    if(lcdhd == -1){
        fprintf(stderr,"lcdInit failed to initialise\n");
        exit(1);
    }

    // Loop forever, retrieving the barometer data log file from the remote machine and 
    // priniting it to stdout and the LCD every 5 minutes

    while(TRUE){
        getLastReading();
        printBarFile();
        // Toggle the backlight on or off depending on time of day - off after OFFHOUR, on after ONHOUR
        hour = getLocalTimeHour();
        if ((hour >= OFFHOUR && backlighton) || (hour < ONHOUR && backlighton)) {
           digitalWrite(LED,LOW);
           backlighton=FALSE;
        }
        else if ((hour >= ONHOUR && !backlighton) && (hour < OFFHOUR && !backlighton)) {
           digitalWrite(LED,HIGH);
           backlighton=TRUE;
        }
        delay(300000);
    }

    // Should never get here ...

    clearDisplay();
    digitalWrite(LED,LOW);     // turn off LCD backlight

    return(0);
}
