#!/bin/bash

timing_dir=/ix/cladouceur/westbrook-data/Scripts/level1/fsl_level1_timingfiles
raw_dir=/ix/cladouceur/westbrook-data/rawdata

for dir in $raw_dir/* ; do
  subfolder=`basename $dir`
	if [[ -d $timing_dir/$subfolder ]] ; then
	  if [[ -d $timing_dir/$subfolder/ses-pretreatment && -d $raw_dir/$subfolder/ses-pretreatment/func ]] ; then
		 cp $timing_dir/$subfolder/ses-pretreatment/func/*.tsv $raw_dir/$subfolder/ses-pretreatment/func
	  fi

	  if [[ -d $timing_dir/$subfolder/ses-posttreatment && -d $raw_dir/$subfolder/ses-posttreatment/func ]] ; then
		 cp $timing_dir/$subfolder/ses-posttreatment/func/*.tsv $raw_dir/$subfolder/ses-posttreatment/func
	  fi

	fi
done
