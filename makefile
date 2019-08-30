#
# Makefile for the weather program and Zambretti test harness.
# (C) Tim Holyoake, 30th August 2019.
# Typing 'make' or 'make weather' will create the executable.
# Typing 'make zamtest' will create the test executable.
#

CC = gcc
CFLAGS  = -lwiringPi -lwiringPiDev -Wall

FORT = gfortran
FFLAGS = -lgfortran

default: weather

weather: cweather.o fweather.o 
	$(FORT) $(CFLAGS) $(FFLAGS) -o weather cweather.o fweather.o
	strip weather

zamtest: fweather.o zambtest.o
	$(FORT) $(FFLAGS) -o zamtest zambtest.o fweather.o
	strip zamtest

cweather.o:  cweather.c hweather.h 
	$(CC) $(CFLAGS) -c cweather.c

fweather.o:  fweather.f 
	$(FORT) $(FFLAGS) -c fweather.f

zambtest.o:  zambtest.f 
	$(FORT) $(FFLAGS) -c zambtest.f

clean: 
	$(RM) zamtest weather *.o *~
