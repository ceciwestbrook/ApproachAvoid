#!/bin/bash

for cope in cope1 ; do
for session in pretreatment posttreatment ; do
	for dir in /ix/cladouceur/westbrook-data/rawdata/* ; do
   		subject=`echo ${dir} | grep -Eo '[0-9]{4}'`

		if [[ -e "/ix/cladouceur/westbrook-data/processed/sub-${subject}/ses-${session}/level2/${cope}.gfeat/cope1.feat/stats/cope1.nii.gz" ]] ; then
	  	echo $subject" "$session" "$cope" found"
		#else
		 # echo $subject" "$session" "$cope" NOT FOUND"
		fi
	done
done
done
