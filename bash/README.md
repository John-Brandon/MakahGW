# Bash scripts

These Bash scripts can be used to automate the batching of simulations over a list of trial IDs.

The lowest level script, 'run.sh' calls the Main program and creates output files from a single trial. 

The mid-level script, 'runset.sh' calls 'run.sh' for each ID in a list of trials. The IDs are sent as input to 'run.sh', and match IDs for the input files for each trial in the list. 
