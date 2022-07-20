#!/bin/bash

basedir=/ix/cladouceur/westbrook-data
cd $basedir/results

find . -name '*.fsf' -exec cp --parents \{\} /$basedir/Scripts/group_level/fsfs/ \;

