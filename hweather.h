#ifndef __HWEATHER_H__
#define __HWEATHER_H__

// I2C LCD DISPLAY 20x4

#define PCF8574 	     0x27       /* default I2C address of the pcf8574 */
#define LCDROWS 		4       /* number of rows on LCD display      */
#define LCDCOLS 	       20       /* number of columns on LCD display   */
#define LCDBITS 		4       /* number of bits used for display    */
#define BASE                   64       /* BASE is not less than 64           */
#define RS                 BASE+0       /* output pins of LCD2004             */
#define RW                 BASE+1
#define EN                 BASE+2
#define LED                BASE+3
#define D4                 BASE+4
#define D5                 BASE+5
#define D6                 BASE+6
#define D7                 BASE+7

// I2C BME280 SENSOR

#define BME280_ADDRESS       0x76     	/* default I2C address of the BME 280 */
#define BME280_T1            0x88
#define BME280_T2            0x8A
#define BME280_T3            0x8C
#define BME280_P1            0x8E
#define BME280_P2            0x90
#define BME280_P3            0x92
#define BME280_P4            0x94
#define BME280_P5            0x96
#define BME280_P6            0x98
#define BME280_P7            0x9A
#define BME280_P8            0x9C
#define BME280_P9            0x9E
#define BME280_H1            0xA1
#define BME280_H2            0xE1
#define BME280_H3            0xE3
#define BME280_H4            0xE4
#define BME280_H5            0xE5
#define BME280_H6            0xE7
#define BME280_CHIPID        0xD0
#define BME280_VERSION       0xD1
#define BME280_SOFTRESET     0xE0
#define BME280_RESET         0xB6
#define BME280_CAL26         0xE1
#define BME280_CONTROLHUMID  0xF2
#define BME280_CONTROL       0xF4
#define BME280_CONFIG        0xF5
#define BME280_PRESSDATA     0xF7
#define BME280_TEMPDATA      0xFA
#define BME280_HUMIDDATA     0xFD

// Storage for calibration data provided from the BME 280

typedef struct
{
   uint16_t t1;			// Temperature
   int16_t  t2;
   int16_t  t3;
   uint16_t p1;			// Pressure
   int16_t  p2;
   int16_t  p3;
   int16_t  p4;
   int16_t  p5;
   int16_t  p6;
   int16_t  p7;
   int16_t  p8;
   int16_t  p9;
   uint8_t  h1;			// Humidity
   int16_t  h2;
   uint8_t  h3;
   int16_t  h4;
   int16_t  h5;
   int8_t   h6;
} bme280_cal;

// Storage for raw and converted data returned from the BME 280

typedef struct 
{
   uint8_t tmsb;		// Temperature
   uint8_t tlsb;
   uint8_t txsb;
   uint8_t plsb;		// Pressure
   uint8_t pmsb;
   uint8_t pxsb;
   uint8_t hmsb;		// Humidity
   uint8_t hlsb;
   uint32_t temperature;        // Converted data
   uint32_t pressure;
   uint32_t humidity;  
} bme280_raw;

#endif
