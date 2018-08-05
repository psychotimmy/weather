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

// Altitude above sea level (metres)

#define MYALT		     117.0

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

// Licences, copyright and acknowledgements

// The code for this project is Copyright (C) 2017-2018 Tim Holyoake.
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
// Redistributions in binary form must reproduce the above copyright
// notice, this list of conditions and the following disclaimer in the
// documentation and/or other materials provided with the distribution.
// The name of the copyright holder may not be used 
// to endorse or promote products derived from
// this software without specific prior written permission.
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER
// "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY
// DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
// OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO,
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
// ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE
// The information provided is believed to be accurate and reliable.
// The copyright holder assumes no responsibility
// for the consequences of use
// of such information nor for any infringement of patents or
// other rights of third parties which may result from its use.
// No license is granted by implication or otherwise under any patent or
// patent rights of the copyright holder.

// The code for this project makes extensive use of the WiringPi library, a PIN based GPIO access library 
// for the BCM2835, BCM2836 and BCM2837 SoC devices used in all Raspberry Pi computers.
//
// The WiringPi library is licensed under the GNU LGPL, version 3.
//
// More information on WiringPi can be found at http://wiringpi.com/ 

// Copyright notice for the Bosch Sensortech BME280 code incorporated and adapted from
// https://github.com/BoschSensortec/BME280_driver

// Copyright (C) 2016 - 2017 Bosch Sensortec GmbH
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
// Redistributions in binary form must reproduce the above copyright
// notice, this list of conditions and the following disclaimer in the
// documentation and/or other materials provided with the distribution.
// Neither the name of the copyright holder nor the names of the
// contributors may be used to endorse or promote products derived from
// this software without specific prior written permission.
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
// CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL COPYRIGHT HOLDER
// OR CONTRIBUTORS BE LIABLE FOR ANY
// DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
// OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO,
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
// ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE
// The information provided is believed to be accurate and reliable.
// The copyright holder assumes no responsibility
// for the consequences of use
// of such information nor for any infringement of patents or
// other rights of third parties which may result from its use.
// No license is granted by implication or otherwise under any patent or
// patent rights of the copyright holder.

#endif
