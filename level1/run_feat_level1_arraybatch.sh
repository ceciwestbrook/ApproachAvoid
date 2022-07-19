#!/bin/bash
# Script written in May 2022 by Ceci Westbrook

# Batch options
# Inputs ----------------------------------
#SBATCH -J feat
#SBATCH --time=12:00:00
#SBATCH --cluster=smp
#SBATCH -N 1
#SBATCH --cpus-per-task=1
#SBATCH --array=0
# Outputs ----------------------------------
#SBATCH -o log/%x-%A-%a.out
#SBATCH -e log/%x-%A-%a.err
# ------------------------------------------

session="post" #pre or post
run=2 #1 or 2

# creates an array of all subjects from saved file
subs=()

# if running all subjects:
filename="subjects_${session}run${run}.txt"
subs=(`cat "$filename"`)

# if running/rerunning a subset:
#subs=(2157 2194 2238 2244 2260 2351 2387)
#subs=(2238 2260 2427)
#subs=(2083 2238 2387 2427)
#subs=(2168 2238 2306 2310 2424)

# chooses a subset of #x subjects starting at index #n
n=$SLURM_ARRAY_TASK_ID # index of subject to start at
x=1 # number of subjects to run
uniq_subs=($(printf "%s\n" ${subs[@]} | sort -u))
subject="${uniq_subs[@]:${n}:${x}}"
echo "Running batch: $subject"

# Make directories if needed
if [[ -d "/ix/cladouceur/westbrook_data/preprocessed/sub-${subject}/ses-${session}treatment" ]] ; then
	mkdir /ix/cladouceur/westbrook_data/processed/sub-${subject}/ses-${session}treatment
	mkdir /ix/cladouceur/westbrook_data/processed/sub-${subject}/ses-${session}treatment/level1
fi


# Remove old feat directories if re-running
if [[ -d "/ix/cladouceur/westbrook-data/processed/sub-${subject}/ses-${session}treatment/level1/run${run}.feat" ]] ; then
	rm -r /ix/cladouceur/westbrook-data/processed/sub-${subject}/ses-${session}treatment/level1/run${run}*.feat
fi

# load singularity module
module load fsl

# build the command, with option to write out for debugging purposes
featfile="/ix/cladouceur/westbrook-data/Scripts/level1/level1_fsf_files/sub-${subject}_ses-${session}treatment_fsf-lv1_run-${run}.fsf"
cmd="feat $featfile"
eval $cmd
