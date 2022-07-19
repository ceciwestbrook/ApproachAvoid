#!/bin/bash
# Script written in May 2022 by Ceci Westbrook
# Reprocess subjects with one unusable time-point without the longitudinal tag.

# Batch options I have found to work best on HTC
# Inputs ----------------------------------
#SBATCH -J fmriprep
#SBATCH --time=12:00:00
#SBATCH --cluster=smp
#SBATCH -N 1
#SBATCH --cpus-per-task=24
#SBATCH --array=16
# Outputs ----------------------------------
#SBATCH -o log/%x-%A-%a.out
#SBATCH -e log/%x-%A-%a.err
# ------------------------------------------

# creates an array of subjects needing reprocessed
subs=(2011 2029 2040 2044 2054 2056 2069 2075 2079 2098 2108 2112 2114 2122 2168 2245 2271 2299 2322 2326 2333 2345 2353 2420)

# chooses a subset of #x subjects starting at index #n
n=$SLURM_ARRAY_TASK_ID # index of subject to start at
x=8 # number of subjects to run
uniq_subs=($(printf "%s\n" ${subs[@]} | sort -u))
subjectlist="${uniq_subs[@]:${n}:${x}}"
echo "Running batch: $subjectlist ncores: 16 mem: 8G"

# Build the fmriprep command
base_dir=/ix/cladouceur/westbrook-data
BIDS_dir=$base_dir/rawdata
preprocessed_dir=$base_dir/preprocessed
fsfileloc=$base_dir/license_free_surfer.txt
singularity_cmd="singularity run --cleanenv --bind $base_dir:$base_dir $base_dir/fmriprep-21.0.1.simg"

# Options - specify for individual study needs
# The below run fmriprep on $subjectlist specified above
# !!Important note! Make sure nprocs is set equal to cpus-per-task in the header!
options="--fs-license-file $fsfileloc \
--nprocs 24 \
--ignore fieldmaps slicetiming \
--fd-spike-threshold 0.9 \
--output-spaces MNI152NLin6Asym:res-2 \
--participant-label $subjectlist \
--skip-bids-validation"

# load singularity module
module load singularity

# move to the location you're running fmriprep from (optional, but makes some things easier)
cd /ix/cladouceur/westbrook-data/

# build the command, with option to write out for debugging purposes
# including "time" to check how long batches take to run at different parameters--this writes out in the outfile
cmd="$singularity_cmd $BIDS_dir $preprocessed_dir participant $options"
echo $cmd
eval $cmd
