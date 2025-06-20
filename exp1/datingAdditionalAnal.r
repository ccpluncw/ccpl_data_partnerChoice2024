library(chMorals)
library(chutils)
library(stringr)

cleanOverlapFile <- FALSE

#set up the new RT variables
fitCol <- "fit.RT"
resCol <- "res.RT"
respChoiceVals <- c("Top", "Bottom")
item1cols <- c("IA.1.org")
item2cols <- c("IB.1.org")
overlapItem1cols = c("IA1")
overlapItem2cols = c("IB1")
useTwoParameterModel <- TRUE
overlapDataIsComplete <- FALSE

# read in parameters
params<-ch.readMoralsDBfile("datingDBfile.txt")

analysisReadyData.sn <-read.table("analysisReadyData.sn.txt", header=T, sep="\t", quote="\"")

#set up the group and item directories
mainDir <- getwd()
ch.newDir (mainDir, params$gpSubDir)
gpDir <- getwd()

ch.newDir (gpDir, "newItemAnalysis")
niaDir <- getwd()


item.all.out <- ch.moralsItemRTpHit(analysisReadyData.sn, c(item1cols, item2cols), resCol, overlapRoundCol = "overlapRound", correctCol = "correct", correctVals = c(1,0), useTwoParameterModel= TRUE, minNperOverlap = params$minOverlapN, minUniqueOverlaps = 3, statsOutputFile = "all.additionalAnalysis.out.txt", numPlotRows = 2, numPlotCols = 2, dt.set = NULL) 

ch.newDir (niaDir, "female")

item.M.out <- ch.moralsItemRTpHit(analysisReadyData.sn[analysisReadyData.sn$Gender == "Female", ], c(item1cols, item2cols), resCol, overlapRoundCol = "overlapRound", correctCol = "correct", correctVals = c(1,0), useTwoParameterModel= TRUE, minNperOverlap = params$minOverlapN, minUniqueOverlaps = 3, statsOutputFile = "female.additionalAnalysis.out.txt", numPlotRows = 2, numPlotCols = 2, dt.set = NULL) 

setwd(niaDir)
ch.newDir (niaDir, "male")
item.F.out <- ch.moralsItemRTpHit(analysisReadyData.sn[analysisReadyData.sn$Gender == "Male", ], c(item1cols, item2cols), resCol, overlapRoundCol = "overlapRound", correctCol = "correct", correctVals = c(1,0), useTwoParameterModel= TRUE, minNperOverlap = params$minOverlapN, minUniqueOverlaps = 3, statsOutputFile = "male.additionalAnalysis.out.txt", numPlotRows = 2, numPlotCols = 2, dt.set = NULL) 

setwd(mainDir)