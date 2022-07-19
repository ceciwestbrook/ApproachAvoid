#!/bin/bash

for dir in /ix/cladouceur/westbrook-data/rawdata/*/ses-pretreatment ; do
   	subject=`echo ${dir} | grep -Eo '[0-9]{4}'`
	session=`basename $dir`

	#echo -e "$subject\t$session"

	if [[ -e "/ix/cladouceur/westbrook-data/Scripts/level1/behav_data/${subject}_02a.dat" ]] ; then
	  echo $subject" 02a"
	#else
	#  echo -e "$subject\t$session\tmissing" >> out.txt
	fi

	if [[ -e "/ix/cladouceur/westbrook-data/Scripts/level1/behav_data/${subject}_02b.dat" ]] ; then
	  echo $subject" 02b"
	#else
	#  echo -e "$subject\t$session\tmissing" >> out.txt
	fi

	if [[ -e "/ix/cladouceur/westbrook-data/Scripts/level1/behav_data/${subject}_22a.dat" ]] ; then
	  echo $subject" 22a"
	#else
	#  echo -e "$subject\t$session\tmissing" >> out.txt
	fi

	if [[ -e "/ix/cladouceur/westbrook-data/Scripts/level1/behav_data/${subject}_22b.dat" ]] ; then
	  echo $subject" 22b"
	#else
	#  echo -e "$subject\t$session\tmissing" >> out.txt
	fi

done
