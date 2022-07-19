#!/bin/bash

fsfdir=/data/Westbrook_Analysis/Scripts/level1/level1_fsf_files

# to run all subjects
#subs=()
#for dir in `ls -d /data/CATS/fmri/raw/*`; do
#	rawname=`basename $dir`
#	subject=`echo ${rawname} | grep -Eo '[0-9]{4}'`
#	subs+=($subject)
#done

# to run a subset
subs=(2270)
uniq_subs=($(printf "%s\n" ${subs[@]} | sort -u))

for subnum in ${uniq_subs[@]}; do
  mkdir /data/Westbrook_Analysis/processed/sub-$subnum

  for session in pretreatment posttreatment; do
      if [[ -d /data/Westbrook_Analysis/preprocessed/sub-$subnum/ses-$session ]] ; then
	mkdir /data/Westbrook_Analysis/processed/sub-$subnum/ses-$session
	mkdir /data/Westbrook_Analysis/processed/sub-$subnum/ses-$session/level1

	for runnum in 1 2; do 
	  echo "running $subnum $session run $runnum"
	  feat $fsfdir/"sub-${subnum}_ses-${session}_fsf-lv1_run-${runnum}"
	done
      fi
  done
done
