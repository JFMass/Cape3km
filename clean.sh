#!/usr/bin/env bash

#will clean the last 24 hours unless day was archived. Designed to be run at 12z every morning.

cd html/couches/nowcast

chkdate=`date --date="2 days ago" +%y%m%d`
deldate=`date --date="2 days ago" +%y%m%d%H`

if grep -Fxq "$chkdate" archive.txt
then
    echo "Date is archived, will not delete"
else
    cd couches/nowcast
    x=0
    while [ $x -lt 24 ]
    do
    del=`date --date="$deldate + $x hour" +%y%m%d%H`
    rm -r "nowcast_$del"
    x=$[$x+1]
    done
fi


echo "Done deleting date"





