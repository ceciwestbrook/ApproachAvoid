#!/bin/bash

subs=()

for dir in `ls -d /data/CATS/fmri/raw/*`; do
	rawname=`basename $dir`
	subject=`echo ${rawname} | grep -Eo '[0-9]{4}'`
	subs+=($subject)
done

uniq_subs=($(printf "%s\n" ${subs[@]} | sort -u))
#echo ${uniq_subs[@]}

for subnum in ${uniq_subs[@]}; do
if [[ $subnum > 0 ]]; then
  for sessions in `ls -d /data/CATS/fmri/raw/$subnum*`; do
	sesnum=`basename $sessions | grep -o '..$'`

	if [[ $sesnum == '02' ]]; then
	  sesname=ses-pretreatment
	elif [[ $sesnum == '22' ]]; then
	  sesname=ses-posttreatment
	else
	  break
	fi

	mkdir /data/Westbrook_Analysis/rawdata/sub-${subnum}
	mkdir /data/Westbrook_Analysis/rawdata/sub-${subnum}/${sesname}
	mkdir /data/Westbrook_Analysis/rawdata/sub-${subnum}/${sesname}/func
	mkdir /data/Westbrook_Analysis/rawdata/sub-${subnum}/${sesname}/anat
	

	echo $subnum' '$sesnum' '$sesname

	dcm2niix -o /data/Westbrook_Analysis/rawdata/sub-${subnum}/${sesname}/anat/ -f "sub-${subnum}_${sesname}_T1w" -z y $sessions/mprage1
	if [ -d $sessions/gre_field_mapping* ]; then
	for fieldmap in `ls -d $sessions/gre_field_mapping*`; do
		echo $fieldmap
		mkdir /data/Westbrook_Analysis/rawdata/sub-${subnum}/${sesname}/fmap
		dcm2niix -o /data/Westbrook_Analysis/rawdata/sub-${subnum}/${sesname}/fmap/ -f "sub-${subnum}_${sesname}_phasediff" -z y $fieldmap
	done
	fi

	  scannum=0
	  for avoids in `ls -d $sessions/avoid* | grep 'avoid[0-9]'`; do
	    scannum=$((scannum+1))
	    dcm2niix -o /data/Westbrook_Analysis/rawdata/sub-${subnum}/${sesname}/func/ -f "sub-${subnum}_${sesname}_task-avoid_run-0${scannum}_bold" -z y $avoids
	  done
	done
fi
done
