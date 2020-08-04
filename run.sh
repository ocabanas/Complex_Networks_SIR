#!/bin/bash
#Getting the date and time to create the
tmp_folder=$(date +'%Y_%m_%d_%H_%M_%S')
#echo $(date)
#echo $tmp_folder

# Creating the temporal file in the PROGRAM dir.

mkdir PROGRAM/$tmp_folder/
mkdir OUTPUT/

# Copy the INPUT filles to tmp. folder
cp INPUT/* PROGRAM/$tmp_folder/

# Execute compiler to create or actualize programs
cd PROGRAM/
./compile.sh
#gprof created

#Copy the program scripts in the tmp. folder

cp r_* $tmp_folder/

# Running the programs in the tmp folder

cd $tmp_folder/

./r_main
#gmon.out created
#execute gprof to get the final txt
#gprof r_mainSQA gmon.out > resuma_grop.txt
#delete all other trash

# After the program ends, create tar, delete folder and move to OUTPUT
cd ..
cd ..
pwd
mkdir OUTPUT/$tmp_folder/
mv PROGRAM/$tmp_folder/* OUTPUT/$tmp_folder/

rm -r PROGRAM/$tmp_folder/

echo "Folder for this simulation ${tmp_folder}"