GBALL <- c("GB01A","GB01B","GB01C","GB01D","GB02A","GB02B","GB02D",
           "GB03A","GB03B","GB04A","GB04B","GB05A","GB05B","GB06A","GB06B",
           "GB07A","GB07B","GB08A","GB08B","GB09A","GB09B","GB10A","GB10B",
           "GB11A","GB11B","GB12A","GB12B") # "GB02C" dropped this trial

GPALL <- c("GP01A","GP01B","GP01C","GP01D","GP02A","GP02B","GP02C","GP02D",
           "GP03A","GP03B","GP04A","GP04B","GP05B","GP06A","GP06B",
           "GP07A","GP07B","GP08A","GP08B","GP09A","GP09B","GP10A","GP10B",
           "GP11A","GP11B","GP12A","GP12B", "GP13A", "GP13B", "GP13C", "GP14A", "GP14B") # "GP05A"

GIALL <- c("GI01A","GI01B","GI01C","GI01D","GI02A","GI02B","GI02D",
           "GI03A","GI03B","GI04A","GI04B","GI05A","GI05B") # "GI02C"

# GALL <- c()

ALL.eval <- c(GBALL, GPALL, GIALL); ALL.eval; length(ALL.eval)
ALLSLA <- c(1:11)

# ===============================================================================

Process <- function(Output, OutFile, Trials, SLAs, Stats, Type=1){

  print(OutFile)
  
  Line1 <- ","; Line2 <- ","
  if (1 %in% Stats) {Line1 <- c(Line1,",Final Dep (1+),,"); Line2 <- c(Line2,"Low5,Med,Up5,")}
  if (2 %in% Stats) {Line1 <- c(Line1,",Final Dep (mat),,"); Line2 <- c(Line2,"Low5,Med,Up5,")}
  if (3 %in% Stats) {Line1 <- c(Line1,",Rescaled Final Dep (Zero catches),,"); Line2 <- c(Line2,"Low5,Med,Up5,")}
  if (4 %in% Stats) {Line1 <- c(Line1,",Rescaled Final Dep (Incidental catches),,"); Line2 <- c(Line2,"Low5,Med,Up5,")}
  if (5 %in% Stats) {Line1 <- c(Line1,",Low Mat Fems,,"); Line2 <- c(Line2,"Low5,Med,Up5,")}
  if (6 %in% Stats) {Line1 <- c(Line1,",Need Satisfaction (1-20),,"); Line2 <- c(Line2,"Low5,Med,Up5,")}
  if (7 %in% Stats) {Line1 <- c(Line1,",Need Satisfaction (1-100),,"); Line2 <- c(Line2,"Low5,Med,Up5,")}
  if (8 %in% Stats) {Line1 <- c(Line1,",Ave Landed (N13),,"); Line2 <- c(Line2,"Low5,Med,Up5,")}
  if (9 %in% Stats) {Line1 <- c(Line1,",Ave S&L (N13),,"); Line2 <- c(Line2,"Low5,Med,Up5,")}
  if (10 %in% Stats) {Line1 <- c(Line1,",PCFG Killed (N15),,"); Line2 <- c(Line2,"Low5,Med,Up5,")}
  if (11 %in% Stats) {Line1 <- c(Line1,",,Reasons,,,"); Line2 <- c(Line2,"S&L,Landed=5,ABL Reached,SLA Reached,Block Land=20")}

  print("here2")  
  print(Line1)
  print(Line2)
  
  write(Line1,OutFile,ncolumns=100)
  print("here2.1")
  
  write(Line2,OutFile,ncolumns=100,append=T)

  print("here2.2")
  
  if (Type == 1)
   for (Itrial in Trials)
    {
     SubSet <- Output[Output[,1]==Itrial,]
     SubSet <- SubSet[SLAs,]
     for (II in 1:length(SubSet[,1]))
      {
       Line3 <- NULL
       if (1 %in% Stats) Line3 <- c(Line3,SubSet[II,2],",",SubSet[II,3],",",SubSet[II,4],",")
       if (2 %in% Stats) Line3 <- c(Line3,SubSet[II,5],",",SubSet[II,6],",",SubSet[II,7],",")
       if (3 %in% Stats) Line3 <- c(Line3,SubSet[II,8],",",SubSet[II,9],",",SubSet[II,10],",")
       if (4 %in% Stats) Line3 <- c(Line3,SubSet[II,11],",",SubSet[II,12],",",SubSet[II,12],",")
       if (5 %in% Stats) Line3 <- c(Line3,SubSet[II,14],",",SubSet[II,15],",",SubSet[II,15],",")
       if (6 %in% Stats) Line3 <- c(Line3,SubSet[II,17],",",SubSet[II,18],",",SubSet[II,19],",")
       if (7 %in% Stats) Line3 <- c(Line3,SubSet[II,20],",",SubSet[II,21],",",SubSet[II,22],",")
       if (8 %in% Stats) Line3 <- c(Line3,SubSet[II,23],",",SubSet[II,24],",",SubSet[II,25],",")
       if (9 %in% Stats) Line3 <- c(Line3,SubSet[II,26],",",SubSet[II,27],",",SubSet[II,28],",")
       if (10 %in% Stats) Line3 <- c(Line3,SubSet[II,29],",",SubSet[II,30],",",SubSet[II,31],",")
       if (11 %in% Stats) Line3 <- c(Line3,SubSet[II,32],",",SubSet[II,33],",",SubSet[II,34],",",SubSet[II,35],",",SubSet[II,36],",")
       write(c(Itrial,SLAs[II],",",Line3),OutFile,ncolumns=100,append=T)
      }
    } 
  
  print("here3")
  
  print(head(Output))    
  if (Type == 2)
   for (II in SLAs)
    for (Itrial in Trials)
     {
      print(II)
      SubSet <- Output[Output[,1]==Itrial,]
#      SubSet <- SubSet[SLAs,]
      print(SubSet)
      Line3 <- NULL
      if (1 %in% Stats) Line3 <- c(Line3,SubSet[II,2],",",SubSet[II,3],",",SubSet[II,4],",")
      if (2 %in% Stats) Line3 <- c(Line3,SubSet[II,5],",",SubSet[II,6],",",SubSet[II,7],",")
      if (3 %in% Stats) Line3 <- c(Line3,SubSet[II,8],",",SubSet[II,9],",",SubSet[II,10],",")
      if (4 %in% Stats) Line3 <- c(Line3,SubSet[II,11],",",SubSet[II,12],",",SubSet[II,12],",")
      if (5 %in% Stats) Line3 <- c(Line3,SubSet[II,14],",",SubSet[II,15],",",SubSet[II,15],",")
      if (6 %in% Stats) Line3 <- c(Line3,SubSet[II,17],",",SubSet[II,18],",",SubSet[II,19],",")
      if (7 %in% Stats) Line3 <- c(Line3,SubSet[II,20],",",SubSet[II,21],",",SubSet[II,22],",")
      if (8 %in% Stats) Line3 <- c(Line3,SubSet[II,23],",",SubSet[II,24],",",SubSet[II,25],",")
      if (9 %in% Stats) Line3 <- c(Line3,SubSet[II,26],",",SubSet[II,27],",",SubSet[II,28],",")
      if (10 %in% Stats) Line3 <- c(Line3,SubSet[II,29],",",SubSet[II,30],",",SubSet[II,31],",")
      if (11 %in% Stats) Line3 <- c(Line3,SubSet[II,32],",",SubSet[II,33],",",SubSet[II,34],",",SubSet[II,35],",",SubSet[II,36],",")
      write(c(Itrial,II,",",Line3),OutFile,ncolumns=100,append=T)
    } 
    
    
  }
 
# AWExtract(ALL.eval, dat.path.eval, out.path.eval, SLAs=c(1:11))

# ===============================================================================

AWExtract <- function(Trials, dat.path, Outfile, SLAs, Stats=1:11, Case="PCFG",Type=1)
  # Requires 'Process' function
 {
  print(dat.path)
  TheData <- read.table(dat.path, sep="")
  # TheData <- read.table("F:\\AW-ALL.OUT",sep="")

  North <- TheData[ , 1:22]
  PCFG <- cbind(TheData[ , 1], TheData[ , 23:57])

  if (Case=="North") Process(North,Outfile,Trials,SLAs,Stats,Type)
  print("here1")
#   print(PCFG)
#   print(Outfile)
#   print(Trials)
#   print(SLAs)
#   print(Stats)
#   print(Type)  
  if (Case=="PCFG") Process(PCFG,Outfile,Trials,SLAs,Stats,Type)

  print(paste("Outfile = ", Outfile))

 }
 
# AWExtract("GB01A","F:\\Out1.CSV",SLAs=c(1,2,3,4,5,6,7,9,11),Type=1)
# AWExtract(c("GB01A","GB01B","GB01C","GB01D"),"F:\\Out2.CSV",SLAs=c(1,3),Type=2)

# setwd("/Users/johnbrandon/Documents/2012 Work/IWC 64 Panama/Gray IR/From Andre/Table Extraction Code/v2")
# setwd("/Users/johnbrandon/Documents/2012 Work/IWC 64 Panama/Gray IR/From Andre/Table Extraction Code/v1")

# Working with Preliminary results from Andre for Intersessional
# setwd("/Users/johnbrandon/Documents/2012 Work/Makah/IWC 65/Intersessional/Prelim Results From Andre")

# setwd("/Users/johnbrandon/Documents/2013 Work/IWC.65/ENP.gray.whale")
# ddir = getwd()  # get current working directory
# 
# ###########################################################
# # get unique Trials (Andre has some different numbers than IWC 64)
# #  May 13, 2013 -- tabling results for draft report on SC request for more exact variant
# dat <- read.table(dat.path,sep="")
# names(dat)
# unique(dat$V1)
# 
# GALL <- as.character(levels(unique(dat$V1))) # get re-numbered trials for IWC 65 
# dat.file = "AW-ALL-JB.txt"
# dat.path = paste(ddir, dat.file, sep="/"); dat.path
# Outfile = "ENP.GW.IWC65.AWMP.OUT.CSV"
# out.path = paste(ddir, Outfile, sep="/"); out.path
# AWExtract(GALL, dat.path, out.path, SLAs=c(1:8))
# system(paste("open ", Outfile, sep=""))
# 
# titles[1] <- "SLA 1; All strikes in May"
# titles[2] <- "1 Strike before May"
# titles[3] <- "2 Strikes before May"
# titles[4] <- "3 Strikes before May"
# titles[5] <- "4 Strikes before May"
# titles[6] <- "5 Strikes before May"
# titles[7] <- "6 Strikes before May"
# titles[8] <- "SLA 2; All strikes before May"
# 
# ###########################################################
# 
# # Try with only one trial and one SLA
# # Outfile = "Out.tmp.CSV"
# AWExtract(c("GB01C", "GB51A"), dat.path, out.path, SLAs=c(1:8))
# system(paste("open ", Outfile, sep=""))
# 
# AWExtract(GBALL, dat.path, out.path, SLAs=c(1,2,3,4,5,6,7,9,11))
# system(paste("open ", Outfile, sep=""))
# 
# # SLAs assuming PCFG availability = 100 %
# AWExtract(GBALL, dat.path, out.path, SLAs=c(3,6,9))
# system(paste("open ", Outfile, sep=""))
# 
# AWExtract(GBALL, dat.path, out.path, SLAs=ALLSLA)
# system(paste("open ", Outfile, sep=""))
# 
# #system(paste("open -e ", out.file, sep="")) # open with TextEdit (-e)
# system(paste("open ", out.file, sep="")) # open with TextEdit (-e)
