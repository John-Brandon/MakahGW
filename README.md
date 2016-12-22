# MakahGW

Management Strategy Evaluation of alternating seasonal hunts for gray whales. 

Original Fortran source code provided by Andre E. Punt (Univ. of Washington). That version of the code was used by the Aboriginal Whaling Management Procedure Working Group, for the Gray Whale Implementation Review, at the 2012 meeting of the Scientific Committee of the International Whaling Commission. 

This repository is essentially a fork off the 2012 version of the code (which does not have its own repository at present). This fork includes some independent developments and implements an alternative Strike Limit Control rule.   

## Project notes: 

1. Code is being developed to evaluate (via simulation) an alternating season hunt strategy (e.g. odd years hunt in winter, even years in summer, or vice-versa). 


2. Population dynamics and availability (to hunters) are modeled for two stocks. The dynamics between stocks are treated independently with one exception. Immigration from the larger stock into the smaller stock is allowed. 


## Shell scripting: 

1. DOS batch files are being ported to Bash scripts (running under Mac OS 10.11.6) 

## GNU Make(file): 

1. The original Fortran code files include GUP2.FOR, which simply contains a list of "INCLUDE" statements for each *.FOR file to be compiled into the executable. This approach is the same as concantinating the text in each individual file during compilation __VS__.  

2. A "MODULAR" approach, wherein, each *.FOR code file is compiled first into an object *.o file. The object files are then linked during the step of compiling the executable. 





