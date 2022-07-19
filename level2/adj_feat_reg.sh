#!/bin/bash

# Ceci Westbrook February 2022
# This code implements the FEAT registration workaround listed here:
# https://mumfordbrainstats.tumblr.com/post/166054797696/feat-registration-workaround

FSLDIR=/ihome/crc/install/fsl/6.0.4/centos7/fsl

# to run all subjects
subs=()
for dir in `ls -d /ix/cladouceur/westbrook-data/processed/*`; do
	rawname=`basename $dir`
	subject=`echo ${rawname} | grep -Eo '[0-9]{4}'`
	subs+=($subject)
done

# to run a subset
subs=(2270)

uniq_subs=($(printf "%s\n" ${subs[@]} | sort -u))

for subject in ${uniq_subs[@]}; do 
	for session in pretreatment posttreatment; do
	  for run in 1 2; do
	    if [[ -d /ix/cladouceur/westbrook-data/processed/sub-$subject/ses-$session/level1/run$run.feat/stats ]]; then
	       cp -r /ix/cladouceur/westbrook-data/processed/sub-$subject/ses-$session/level1/run$run.feat /ix/cladouceur/westbrook-data/processed/sub-$subject/ses-$session/level1/run${run}_adjreg.feat
	       featdir=/ix/cladouceur/westbrook-data/processed/sub-$subject/ses-$session/level1/run${run}_adjreg.feat
	       cp $FSLDIR/etc/flirtsch/ident.mat $featdir/reg/example_func2standard.mat 
	       cp $FSLDIR/etc/flirtsch/ident.mat $featdir/reg/standard2example_func.mat
	       cp $featdir/mean_func.nii.gz $featdir/reg/standard.nii.gz
	    else
	   	rm -r /ix/cladouceur/westbrook-data/processed/sub-$subject/ses-$session/level1/run$run.feat
	    fi
	  done

	done
done
