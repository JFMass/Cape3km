#!/usr/bin/env bash

#will clean the last 24 hours unless day was archived. Designed to be run at 12z every morning.

cd html/couches/nowcast

chkdate=`date --date="2 days ago" +%y%m%d`

if [ ! -z $(grep "$chkdate" archive.txt) ]
then
    echo "Archiving $chkdate"
    mkdir /home/meteo/html/models/archives/"$chkdate"
    x=0
    while [ $x -lt 24 ]
    do
    del=$(date -d "$chkdate 12:00 UTC +$x hour" +%Y%m%d%H)
    mv "nowcast_$del" /home/meteo/html/models/archives/"$chkdate"/"$del"
    x=$[$x+1]
    done
else
    x=0
    echo "Deleting $chkdate"
    while [ $x -lt 24 ]
    do
    del=$(date -d "$chkdate 12:00 UTC +$x hour" +%Y%m%d%H)
    rm -r "nowcast_$del"
    x=$[$x+1]
    done
fi






