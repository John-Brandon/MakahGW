#!/bin/bash
# Bottom level script that runs simulations for a given trial.
# The trial ID is given by the mid-level script that calls run.sh
# At the top of the script hierarchy is the upper level script, that 
#+   batches sets of trial runs. 
# This low level script also calls programs to process simulation output,
#+   and organize that output into the appropriate files and directories. 
# Commands were ported from DOS script by Andre Punt / Cherry Allison, to
#+   bash script here by John Brandon. 
# -----------------------------------------------------------------------

#set -x

# cd ~/MakahGW/f90/Project
# ~/MakahGW/bash
cd ../f90/Project

echo 'Hello from run.sh'
echo $PWD

rm copy.dat;
rm aw-resI;
rm aw-resZ;
rm aw-res;

echo "Running Trial G$1"

cp ./Runstreams/G$1.DDD ./copy.dat
#cp ./Runstreams/GB01a.DDD ./copy.dat;

cp manage.z manage.dat;
./main.app | tee a.0;

cp manage.i manage.dat;
# ./main.app > a.0;
./main.app | tee a.0;

#cp manage.$2 manage.dat;
cp manage.1 manage.dat;
./main.app | tee a.1;

./aw-res7.app > a.2;  # This doesn't seem necessary

./proc.app > ../AllOut/figf$2$1.out;

cp aw-traj.out ../AllOut/aw-traj$2.$1;
cp aw-conf.out ../AllOut/aw-conf$2.$1;
cp aw-sum.out ../AllOut/aw-sum$2.$1;


cd ~/MakahGW/bash
# cd ../bash

#
##rm aw-resI
##rm aw-resZ
##rm aw-res
#
#rm aw-traj-out;
#rm aw-sum.out;
#rm *.20;
#rm *.100;
#rm *.res;
#rm aw-table;
#rm aw-1-2.out;
#rm aw-conf.out;


# MS DOS version ----------------------------------------------------------
# del copy.dat
# del aw-resI
# del aw-resZ
# del aw-res
# copy runstreams\G%1.DDD copy.dat
# copy manage.z manage.dat
# f2-gup2 >a.0
# copy manage.i manage.dat
# f2-gup2 >a.0
# copy manage.%2 manage.dat
# f2-gup2 >a.1
# aw-res7 >a.2
# proc > ..\ALLOUT\figout%2.%1
# copy aw-traj.out ..\ALLOUT\aw-traj%2.%1
# copy aw-conf.out ..\ALLOUT\aw-conf%2.%1
# copy aw-sum.out ..\ALLOUT\aw-sum%2.%1
# #copy aw-resZ ..\ALLOUT\aw-resZ%2.%1
# #del aw-resI
# #del aw-resZ
# #del aw-res
# del aw-traj.out
# del aw-sum.out
# del *.20
# del *.100
# del *.res
# del aw-table
# del aw-1-2.out
# del aw-conf.out


