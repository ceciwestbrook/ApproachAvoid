#!/bin/bash

for dir in /ix/cladouceur/westbrook-data/rawdata/* ; do
   	subject=`echo ${dir} | grep -Eo '[0-9]{4}'`
	session=`basename $dir`

	#echo -e "$subject\t$session"
  for session in pretreatment posttreatment ; do
	for run in 1 2 ; do
		if [[ -e "/ix/cladouceur/westbrook-data/processed/sub-${subject}/ses-${session}/level1/run${run}.feat/stats/cope8.nii.gz" ]] ; then
	  	echo $subject" "$session" run"$run" found"
		else
		  echo $subject" "$session" run"$run" NOT FOUND"
		fi
	done
  done
done
