#!/bin/bash

#Compliler for C and Fortran 
comp1='gcc'
comp2='gfortran'

# Optimization Flags



# Warning and Error flags gfortran
gflags_hard='-Wno-tabs -Wall -Wextra -Warray-temporaries -fbounds-check -Wconversion -fimplicit-none -fbacktrace -ffree-line-length-0 -fcheck=all -ffpe-trap=zero,overflow,underflow -finit-real=nan'
gflags_soft='-Wall -fbounds-check -Wno-tabs -ffree-line-length-0'
# Warning and Error flags intel compiler
iflags_hard='-check all -fpe0 -warn -traceback -debug extended'
iflags_soft=''

flags='-ffree-line-length-0'
#flags=$gflags_hard
#opt=
opt='-O3'

$comp2 $opt -c $flags read_data.f90
$comp2 $opt -c $flags init_vars.f90
$comp2 $opt -c $flags initial_config.f90
$comp2 $opt -c $flags routines.f90
$comp2 $opt -c $flags main.f90
$comp2 main.o read_data.o init_vars.o initial_config.o routines.o  $opt $flags -o r_main
##gfortran mainSQA.o dummy.x -o r_mainSQA
rm *.o
rm *.mod