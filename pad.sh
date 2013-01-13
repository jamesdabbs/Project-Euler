#!/bin/bash
# A simple script to pad all the filenames in this directory with leading zeros
for f in *.hs
do
    name=${f%.hs}
    if [ ${#name} -lt 3 ]
    then
        new=`printf "%03d" $name`
        echo "Renaming $name.hs to $new.hs"
        mv $name.hs $new.hs
    fi 
done
