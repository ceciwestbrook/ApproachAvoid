 #!/bin/bash

cd /ix/cladouceur/westbrook-data/

subs=()
for dir in `ls -d /ix/cladouceur/westbrook-data/rawdata/*`; do
	rawname=`basename $dir`
	subject=`echo ${rawname} | grep -Eo '[0-9]{4}'`
	subs+=($subject)
done

# runs a subset of #x subjects starting at index #n
n=4
x=1
uniq_subs=($(printf "%s\n" ${subs[@]} | sort -u))
subjectlist="${uniq_subs[@]:${n}:${x}}" #subjectlist="${uniq_subs[@]:${n}:10}"
echo "Running batch: $subjectlist"

module load singularity

{ time `singularity run --cleanenv --bind /ix/cladouceur/westbrook-data:/ix/cladouceur/westbrook-data fmriprep-21.0.1.simg  rawdata preprocessed participant --fs-license-file license_free_surfer.txt --nprocs 16 --ignore fieldmaps slicetiming --fd-spike-threshold 0.3 --longitudinal --output-spaces MNI152NLin6Asym:res-2 --participant-label $subjectlist --skip-bids-validation` ; } 2> time.txt

#`singularity run --cleanenv --bind /data/Westbrook_Analysis/ /data/Westbrook_Analysis/fmriprep-21.0.1.simg  rawdata preprocessed participant --fs-license-file /data/Westbrook_Analysis/license.txt --ignore fieldmaps slicetiming --fd-spike-threshold 0.3 --longitudinal --output-spaces MNI152NLin6Asym:res-2 --participant-label $subjectlist --skip-bids-validation`
