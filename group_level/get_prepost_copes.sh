#!/bin/bash

for cope in cope1 ; do
for session in pretreatment posttreatment ; do
	for dir in /ix/cladouceur/westbrook-data/rawdata/* ; do
   		subject=`echo ${dir} | grep -Eo '[0-9]{4}'`

		precope="/ix/cladouceur/westbrook-data/processed/sub-${subject}/ses-pretreatment/level2/${cope}.gfeat/cope1.feat/stats/cope1.nii.gz"
		postcope="/ix/cladouceur/westbrook-data/processed/sub-${subject}/ses-posttreatment/level2/${cope}.gfeat/cope1.feat/stats/cope1.nii.gz"

		if [[ -e $precope && -e $postcope ]] ; then
	  	echo "/ix/cladouceur/westbrook-data/processed/sub-${subject}/ses-${session}/level2/${cope}.gfeat/cope1.feat/stats/cope1.nii.gz"
		#else
		 # echo $subject" "$session" "$cope" NOT FOUND"
		fi
	done
done
done
