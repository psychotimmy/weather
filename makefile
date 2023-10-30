#
# Makefile for the weather program and Zambretti test harness.
# (C) Tim Holyoake, 30th August 2019.
# Typing 'make' or 'make weather' will create the executable.
# Typing 'make zamtest' will create the test executable.
#
# Makefile revised 30/10/2023 to work properly with ld included
# in bookworm - libraries have to be referenced after the source
# files otherwise they won't be included in the executable by
# default (as was the case with buster and previous os releases)
#

CC = gcc
CFLAGS  = -Wall

FORT = gfortran

LDFLAGS = -lgfortran -lwiringPi -lwiringPiDev

default: weather

weather: cweather.o fweather.o
        $(FORT) $(CFLAGS) -o weather cweather.o fweather.o $(LDFLAGS)
        strip weather

zamtest: fweather.o zambtest.o
        $(FORT) -o zamtest zambtest.o fweather.o $(LDFLAGS)
        strip zamtest

cweather.o: cweather.c hweather.h
        $(CC) $(CFLAGS) -c cweather.c

fweather.o: fweather.f
        $(FORT) -c fweather.f

zambtest.o: zambtest.f
        $(FORT) -c zambtest.f

clean:
        $(RM) zamtest weather *.o *~
