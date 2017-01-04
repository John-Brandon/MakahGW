# MakahGW

Management Strategy Evaluation of alternating seasonal hunts for gray whales. 

Original Fortran source code provided by Andre E. Punt (Univ. of Washington). That version of the code was used by the Aboriginal Whaling Management Procedure Working Group, for the Gray Whale Implementation Review, at the 2012 meeting of the Scientific Committee of the International Whaling Commission. 

This repository is essentially a fork off the 2012 version of the code (which does not have its own repository at present). This fork includes some independent developments and implements an alternative Strike Limit Control rule.   

## Project notes: 

1. Code is being developed to evaluate (via simulation) an alternating season hunt strategy (e.g. odd years hunt in winter, even years in summer, or vice-versa). 

2. Results from available code and 2012 runs have been checked and are equal.

3. Population dynamics and availability (to hunters) are modeled for two stocks. The dynamics between stocks are treated independently with one exception. Immigration from the larger stock into the smaller stock is allowed. 


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

## Examples from the command line:
``` shell
cd ~/MakahGW      # make sure you are at top of project directory 
make              # compile Fortran 90 code into executable (see Makefile)
cd ./f90/Project  # location of executable
./main.app        # run executable for a single trial (input file, copy.dat)

``` 
### Compiling

### Run a single trial

### Run a batch of many trials

``` shell
/Users/johnbrandon/MakahGW/f90/code
>upp

/Users/johnbrandon/MakahGW
>make
cd ./f90/code;\
gfortran -fbounds-check -c -w F2TST9.FOR;
cd ./f90/code;\
gfortran -fbounds-check -w -c GUP2.FOR;
cd ./f90/code;\
gfortran -fbounds-check -w -c AWEXTRD.FOR;
gfortran -fbounds-check -w ./f90/code/F2TST9.o ./f90/code/MATRIX.o ./f90/code/COMMON.o ./f90/code/DMSLC.o ./f90/code/JBSLC.o ./f90/code/GUP2.o ./f90/code/AWEXTRD.o -O3 -o ./f90/Project/main.app;
/Users/johnbrandon/MakahGW
>fp 

/Users/johnbrandon/MakahGW/f90/Project
>./main.app
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
