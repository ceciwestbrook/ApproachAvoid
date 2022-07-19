#!/bin/bash

fsfdir=/data/Westbrook_Analysis/Scripts/level2/level2_fsf_files

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
  for session in pretreatment posttreatment; do
      if [[ -d /data/Westbrook_Analysis/processed/sub-$subnum/ses-$session/level1/run1_adjreg.feat ]] ; then
	mkdir /data/Westbrook_Analysis/processed/sub-$subnum/ses-$session/level2
	echo "running $subnum $session"
	feat $fsfdir/"sub-${subnum}_ses-${session}_fsf-lv2.fsf"
      fi
  done
done
