#!/bin/bash
# Script written in May 2022 by Ceci Westbrook

# Batch options
# Inputs ----------------------------------
#SBATCH -J feat_level2
#SBATCH --time=12:00:00
#SBATCH --cluster=smp
#SBATCH -N 1
#SBATCH --cpus-per-task=2
#SBATCH --array=136-160
# Outputs ----------------------------------
#SBATCH -o log/%x-%A-%a.out
#SBATCH -e log/%x-%A-%a.err
# ------------------------------------------

# creates an array of all subjects from directories in BIDS dir (rawdata)
subs=()
for dir in `ls -d /ix/cladouceur/westbrook-data/processed/*`; do
	rawname=`basename $dir`
	subject=`echo ${rawname} | grep -Eo '[0-9]{4}'`
	subs+=($subject)
done

# chooses a subset of #x subjects starting at index #n
n=$SLURM_ARRAY_TASK_ID # index of subject to start at
x=1 # number of subjects to run
uniq_subs=($(printf "%s\n" ${subs[@]} | sort -u))
subject="${uniq_subs[@]:${n}:${x}}"

fsfdir=/ix/cladouceur/westbrook-data/Scripts/level2/level2_fsf_files
module load fsl

for cope in cope1 cope2 cope3 cope4 cope5 cope6 cope7 cope8 ; do
	for session in pretreatment posttreatment; do
		echo "running $subject $session"
		# Make directories if needed
		if [[ -d "/ix/cladouceur/westbrook-data/processed/sub-${subject}/ses-${session}/level1" ]] ; then
			mkdir "/ix/cladouceur/westbrook-data/processed/sub-${subject}/ses-${session}/level2"
		fi

		# Remove old feat directories if re-running
		if [[ -d "/ix/cladouceur/westbrook-data/processed/sub-${subject}/ses-${session}/level2/${cope}.gfeat" ]] ; then
			rm -r "/ix/cladouceur/westbrook-data/processed/sub-${subject}/ses-${session}/level2/${cope}.gfeat"
		fi

		for featfile in `ls "$fsfdir/sub-${subject}_ses-${session}_${cope}"*` ; do
			feat $featfile
		done
	done
done
