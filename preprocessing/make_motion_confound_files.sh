#!/bin/bash
# Ceci Westbrook, April 2022
# reads the confound files output by fmriprep and calls an R script to generate motion confound files for
# later use in FSL

# run all subjects
for dir in `ls -d /ix/cladouceur/westbrook-data/preprocessed/sub-*/`; do

# run an individual subject
#for dir in `ls -d /ix/cladouceur/westbrook-data/preprocessed/sub-2025/`; do
	rawname=`basename $dir`
	subject=`echo ${rawname} | grep -Eo '[0-9]{4}'`

  for session in pretreatment posttreatment; do
	if [[ -d $dir"/ses-$session"  ]] ; then
	  cd $dir"/ses-$session/func"
	  for runnum in 1 2; do
	    Rscript /ix/cladouceur/westbrook-data/Scripts/preprocessing/extract_motion_confounds.R $subject $session $runnum >> /ix/cladouceur/westbrook-data/Scripts/preprocessing/spike_numbers.txt
	  done
	fi
  done
done
