#!/bin/bash

# Ceci Westbrook February 2022
# This code implements the FEAT registration workaround listed here:
# https://mumfordbrainstats.tumblr.com/post/166054797696/feat-registration-workaround

# Batch options
# Inputs ----------------------------------
#SBATCH -J adj_feat_reg
#SBATCH --time=1:00:00
#SBATCH --cluster=smp
#SBATCH -N 1
#SBATCH --cpus-per-task=1
#SBATCH --array=0-175
# Outputs ----------------------------------
#SBATCH -o log/%x-%A-%a.out
#SBATCH -e log/%x-%A-%a.err
# ------------------------------------------

FSLDIR=/ihome/crc/install/fsl/6.0.4/centos7/fsl

# to run all subjects
subs=()
for dir in `ls -d /ix/cladouceur/westbrook-data/processed/*`; do
	rawname=`basename $dir`
	subject=`echo ${rawname} | grep -Eo '[0-9]{4}'`
	subs+=($subject)
done

n=$SLURM_ARRAY_TASK_ID # index of subject to start at
x=1 # number of subjects to run
uniq_subs=($(printf "%s\n" ${subs[@]} | sort -u))
subject="${uniq_subs[@]:${n}:${x}}"

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

