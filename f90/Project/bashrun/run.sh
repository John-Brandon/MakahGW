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

cd ~/MakahGW/f90/Project

rm copy.dat;
rm aw-resI;
rm aw-resZ;
rm aw-res;

#cp ./Runstreams/G$1.DDD ./copy.dat
cp ./Runstreams/GB01a.DDD ./copy.dat;
cp manage.z manage.dat;
./main > a.0;

cp manage.i manage.dat;
./main > a.0;

#cp manage.$2 manage.dat;
cp manage.1 manage.dat;

./main > a.1;
#./aw-res7 > a.2;
#./proc > ./finresu/figf$2$1.out;
#cp aw-traj.out ./finresu/aw-traj$2.$1;
#cp aw-conf.out ./finresu/aw-conf$2.$1;
#cp aw-sum.out ./finresu/aw-sum$2.$1;
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

