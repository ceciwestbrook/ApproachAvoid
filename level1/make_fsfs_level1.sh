#!/bin/bash

# This script replaces the subject and run numbers in template fsf files

fsfdir=/ix/cladouceur/westbrook-data/Scripts/level1/level1_fsf_files

for dir in `ls -d /ix/cladouceur/westbrook-data/preprocessed/sub-*/`; do
	rawname=`basename $dir`
	subject=`echo ${rawname} | grep -Eo '[0-9]{4}'`

	for session in pretreatment posttreatment; do
	  for run in 1 2 ; do

	    sed 's/SUBNUM/'${subject}'/g' design_template_level1.fsf > $fsfdir'/sub-'${subject}'_ses-'${session}'_fsf-lv1_run-'${run}'.fsf'
	    sed -i 's/SESNUM/'${session}'/g' $fsfdir'/sub-'${subject}'_ses-'${session}'_fsf-lv1_run-'${run}'.fsf'
	    sed -i 's/RUNNUM/'${run}'/g' $fsfdir'/sub-'${subject}'_ses-'${session}'_fsf-lv1_run-'${run}'.fsf'
	
	  done
	done
done

