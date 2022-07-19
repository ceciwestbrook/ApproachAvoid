#!/bin/bash

# Ceci Westbrook May 2022
# This script replaces the subject and run numbers in template fsf files
# based on how many level1 runs they have for pre- and post-treatment timepoints
# as well as excluding individual runs where the EV was blank (no trials of that type)

fsfdir=/ix/cladouceur/westbrook-data/Scripts/prepost/prepost_fsf_files
filedir=/ix/cladouceur/westbrook-data/Scripts/prepost/make_fsfs

# creates an array of all subjects from directories in processed dir
subs=()
for dir in `ls -d /ix/cladouceur/westbrook-data/processed/*`; do
	rawname=`basename $dir`
	subject=`echo ${rawname} | grep -Eo '[0-9]{4}'`
	subs+=($subject)
done

#subs=(2016)
uniq_subs=($(printf "%s\n" ${subs[@]} | sort -u))

for subject in ${uniq_subs[@]}; do
	for cope in cope1 cope2 cope3 cope4 cope5 cope6 cope7 cope8 ; do
		for session in pre post; do
			sed 's/SUBNUM/'${subject}'/g' design_template_prepost.fsf > $fsfdir'/sub-'${subject}'_prepost_'${cope}'.fsf'
			sed -i 's/COPENUM/'${cope}'/g' $fsfdir'/sub-'${subject}'_prepost_'${cope}'.fsf'

		if [[ $subject == `grep -R $subject "$filedir/${cope}${session}run1run2.txt"` ]] ; then
			eval ${session}_r1="run1"
			eval ${session}_r2="run2"
		elif [[ $subject == `grep -R $subject "$filedir/${cope}${session}run1run1.txt"` ]] ; then
			eval ${session}_r1="run1"
			eval ${session}_r2="run1"
		elif [[ $subject == `grep -R $subject "$filedir/${cope}${session}run2run2.txt"` ]] ; then
			eval ${session}_r1="run2"
			eval ${session}_r2="run2"
		else
			eval ${session}_r1="NOFEAT"
			eval ${session}_r2="NOFEAT"
		fi
		done

    		sed -i 's/RUNNUM1/'$pre_r1'/g' $fsfdir'/sub-'${subject}'_prepost_'${cope}'.fsf'
    		sed -i 's/RUNNUM2/'$pre_r2'/g' $fsfdir'/sub-'${subject}'_prepost_'${cope}'.fsf'
    		sed -i 's/RUNNUM3/'$post_r1'/g' $fsfdir'/sub-'${subject}'_prepost_'${cope}'.fsf'
    		sed -i 's/RUNNUM4/'$post_r2'/g' $fsfdir'/sub-'${subject}'_prepost_'${cope}'.fsf'
  done
done
