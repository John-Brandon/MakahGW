# MakahGW

Management Strategy Evaluation of alternating seasonal hunts for gray whales. 

Original Fortran source code provided by courtesy of Andre E. Punt (Univ. of Washington) and Cherry Allison (IWC). That version of the code was used by the most recent Aboriginal Whaling Management Procedure Working Group Gray Whale Implementation Review, as presented to the Scientific Committee of the International Whaling Commission. 

This repository is a fork off the 2012 version of the code. The base of the master branch (i.e. the files used for the 2012 runs) can be assessed by 

This fork includes some independent developments and implements an alternative Strike Limit Control rule.   

## Project notes: 

1. Results from the the version of the code obtained from AEP have been checked against those reported during 2012 and found to be identical.    

2. The developmental version of the code by JRB is on a parrallel branch named, `alt_sla` (c.f. the `master` branch). 
  a. The Strike Limit Algorithm for this hunt is based on an alternating season hunt strategy (e.g. odd years hunt in winter with max strike of 3, even years in summer with max strike 2)..
  b. There are several differences between the developmental SLA and that from 2012, including but not limited to an absence of block quotas for the PCFG stock. 

## Shell scripting: 

1. DOS batch files have been ported to Bash scripts (running under Mac OS 10.11.6) 

## GNU Make(file): 

1. The original Fortran code files include GUP2.FOR, which simply contains a list of "INCLUDE" statements for each \*.FOR file to be compiled into the executable. This approach is the same as compiling all of the source files in a single step __VS__.  

2. A "MODULAR" approach, wherein, each \*.FOR code file is compiled first into an object \*.o file. The object files are then linked during the step of compiling the executable. Compiling individual object files can take advantage of GNU Make's strengths, i.e. only modified code files are recompiled when linking the executable. In contrast, the "Include" approach compiles the entire file set indiscriminantly.

## JB's Dev Environment:
1. OS: Mac OS X El Cap.
2. Shell: GNU bash, version 3.2.57(1)-release (x86\_64-apple-darwin15)
3. GNU Make 3.81
4. Compiler: GNU gcc gfortran 4.2.3 

## __Examples from the command line__:

### Compiling

``` shell
cd ~/MakahGW      # make sure you are at top of project directory 
make              # compile Fortran 90 code into executable (see Makefile)

``` 

### Run a single trial

``` shell
cd ./f90/Project    # location of executable
chmod a+x main.app  # only needed first time, but you may need to run as 'sudo chmod ...'
./main.app          # run executable for a single trial (input file, copy.dat)
```

### Run a batch of many trials

``` shell
cd ~/MakahGW/bash           # where more of the magic happens
chmod a+x run.sh runset.sh  # grant these scripts execute privelage. see comment above re: 'sudo chmod ...' 
./runset.sh                 # try with a small set first (e.g. say 4 trials), 
                            # before moving onto the full set (72 trials and ~48 mins on 2016 MacBook Pro)
```

### Example ouput in terminal from single trial run

``` shell
-----------------------------------------
--------------------
------------
------
--
                  
CATCH TREATMENT OPTION (MANAGE):   
                    
PAR FILE = GB01D.PAR   
                      
Starting Trials
                        
-> In progress... [#################===]  88% 
Trial Number:           88
Target popln 2 not hit   81.9562963746184        73.1372725000000     
-> In progress... [####################] 100% 
                              
--
------
------------
--------------------
-----------------------------------------

```
