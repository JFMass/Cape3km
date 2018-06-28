#!/usr/bin/env bash

#this file will downlaod mrms files




tstamp=$(date +%Y%m%d%H%M)

wget http://mrms.ncep.noaa.gov/data/2D/SeamlessHSR/MRMS_SeamlessHSR.latest.grib2.gz

gunzip MRMS_SeamlessHSR.latest.grib2.gz




