DougPlot <- function()
{
 File <- read.csv("G:\\OUT.CSV")
 print(File)
 par(mfrow=c(2,2))
 hist(File[1:19,11],xlim=c(-60,-40),main="P trials",xlab="Median Negative LogLikeligood")
 hist(File[20:38,11],xlim=c(-60,-40),main="B trials",xlab="Median Negative LogLikeligood")
 hist(File[39:43,11],xlim=c(-60,-40),main="E trials",xlab="Median Negative LogLikeligood")
 hist(File[44:48,11],xlim=c(-60,-40),main="I trials",xlab="Median Negative LogLikeligood")


}

DougPlot()