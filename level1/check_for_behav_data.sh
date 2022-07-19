#!/bin/bash

for dir in /ix/cladouceur/westbrook-data/rawdata/* ; do
   	subject=`echo ${dir} | grep -Eo '[0-9]{4}'`

	if [[ -e "/ix/cladouceur/westbrook-data/Scripts/level1/behav_data/${subject}_02a.dat" ]] ; then
	  A1=1
	else
	  A1=0
	fi

	if [[ -e "/ix/cladouceur/westbrook-data/Scripts/level1/behav_data/${subject}_02b.dat" ]] ; then
	  A2=1
	else
	  A2=0
	fi

	if [[ -e "/ix/cladouceur/westbrook-data/Scripts/level1/behav_data/${subject}_22a.dat" ]] ; then
	  B1=1
	else
	  B1=0
	fi

	if [[ -e "/ix/cladouceur/westbrook-data/Scripts/level1/behav_data/${subject}_22b.dat" ]] ; then
	  B2=1
	else
	  B2=0
	fi

echo "$subject $A1 $A2 $B1 $B2" >> behav_data.txt

done
