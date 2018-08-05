#
# Makefile for the weather program.
# (C) Tim Holyoake, 5th August 2018.
# Typing 'make' or 'make weather' will create the executable file.
#

CC = gcc
CFLAGS  = -lwiringPi -lwiringPiDev -Wall

FORT = gfortran
FFLAGS = -lgfortran

default: weather

weather: cweather.o fweather.o 
	$(FORT) $(CFLAGS) $(FFLAGS) -o weather cweather.o fweather.o
	strip weather

cweather.o:  cweather.c hweather.h 
	$(CC) $(CFLAGS) -c cweather.c

fweather.o:  fweather.f 
	$(FORT) $(FFLAGS) -c fweather.f

clean: 
	$(RM) weather *.o *~
