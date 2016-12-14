#!/bin/bash
# Copy 'manage' files to new directory
# In the file structure on DropBox from AEP, the 'manage' files are not in the same directory as the executable. 
#

mkdir ~/MakahGW/f90/code/manage;
find ~/MakahGW/f90/Project -name "manage.*" -exec cp -i {} ~/MakahGW/f90/code/manage \;

