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

#set up the group and item directories
mainDir <- getwd()
ch.newDir (mainDir, params$gpSubDir)
gpDir <- getwd()
setwd(mainDir)

ch.newDir (mainDir, params$itemSubDir)
itemDir <- getwd()
setwd(mainDir)

statsOutputFile <- file.path(mainDir,paste(params$dt.set, params$statsOutputFilePrefix))
sink(statsOutputFile, append = F)
  cat("\n***** New Run ****\n\n")
sink(NULL)


### read in data
data.raw <-read.table(params$moralsTaskDataFile, header=T, sep="\t", quote="\"")
data.ovrlp <-read.table(params$valueOverlapDataFile, header=T, sep="\t", quote="\"")

######_____REMOVE PRACTICE TRIALS _____######
data.raw <- data.raw[data.raw$trial_type >=1, ]

### do Prep analysis
processedData <- ch.moralsDataPrep(data.raw, data.ovrlp, "sn", "keybRT", "overlap", "direction", "trial", "keyDef", respChoiceVal = respChoiceVals, item1cols = item1cols, item2cols = item2cols, overlapItem1cols = overlapItem1cols, overlapItem2cols = overlapItem2cols, statsOutputFile = statsOutputFile, params = params, overlapDataIsComplete = overlapDataIsComplete)

### Filter data
analysisReadyData <- ch.moralsFilterDataQ(processedData, "sn", "keybRT", "overlapRound", "correct",c(1,0), statsOutputFile = statsOutputFile, params = params)

### Do RT and p(Hit Analysis on Group Data - remove learning effects for the group)
analysisReadyData.gp <- ch.moralsGrpRTpHit(analysisReadyData, "trial", "keybRT", fitCol, resCol, "overlapRound", "keyDef",respChoiceVals, "correct",c(1,0), useTwoParameterModel = useTwoParameterModel, params = params)
write.table(analysisReadyData.gp, file="analysisReadyData.gp.txt", append=FALSE, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)

### Do RT and p(Hit Analysis on individual subject Data - remove learning effects for each subject)
analysisReadyData.sn <- ch.moralsSnRTpHit(analysisReadyData, "sn", "trial", "keybRT", fitCol, resCol, "overlap", "correct", c(1,0),  useTwoParameterModel = useTwoParameterModel, params = params)
write.table(analysisReadyData.sn, file="analysisReadyData.sn.txt", append=FALSE, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)

#Do d'analysis as a group, but use the data whereby the learning effects were removed by subject
df.dPrime <- ch.moralsDprimeAnalysis(analysisReadyData.sn, "overlapRound", "correct", c(1,0), "targetPresent", c(TRUE,FALSE), resCol, params = params, filenameID = "gp")

#Do an item analysis on the data.  Doesn't matter whether use group or sn data - no rt analysis is done
 itemAnalDat <- ch.moralsItemChoiceAnalysis(analysisReadyData.sn, "IA.1", "IB.1", "overlapRound", "dirOverlap","keyDef", respChoiceVal = respChoiceVals, params = params, saveFigures = T) 
 
 #### For experiments with catagory variable manipulations (e.g., different groups), do an analysis
 #### by group
     grpFitModels <- ch.moralsPlotsByGrpsAndGetModels(analysisReadyData.sn[analysisReadyData.sn$Gender != "Other",], c("Gender"), resCol, "overlapRound", "keyDef", yesNoVal = respChoiceVals, "correct", c(1,0), "targetPresent", c(TRUE,FALSE), useTwoParameterModel = useTwoParameterModel, params = params, minNperOverlap = params$minOverlapN)
     ### and plot the data
     setwd(gpDir)
     ch.moralsPlotFitsByGrps(grpFitModels, c("Gender"), "overlapRound", analysisReadyData.sn[analysisReadyData.sn$Gender != "Other",], filenameID = params$dt.set)

   #### For experiments with quantity variable manipulations, do an analysis a quantity analysis
     ### first plot the directional overlap by quantity grouping

setwd(mainDir)
  ## convert concept names to traits for images, etc.
     nameConv <-read.table("OrginalChanged_Stimuli.txt", header=F, sep="\t", quote="\"")
     colnames(nameConv) <- c("trait1", "orig")
     df.tmp.ovrlp <- merge(data.ovrlp, nameConv, by.x = c("IA1"), by.y = c("orig"))
     colnames(nameConv) <- c("trait2", "orig")
     df.tmp.ovrlp <- merge(df.tmp.ovrlp, nameConv, by.x = c("IB1"), by.y = c("orig"))

   do.filename <- file.path(itemDir,paste0(params$dt.set,"Value by Item",".pdf"))
   parOp <- par(mfrow=c(1,1), mai=c(2,1,1,1), omi=c(1,.75,.25,1), las=2, cex=1.25, lwd=2, bty='n', xpd = T)
   overOut <- ch.moralsGetAndPlotDirOverlap(df.tmp.ovrlp, "trait1", "trait2", "overlap", "direction", filename = do.filename, parOp = parOp, cexLegend = 1, lgndPlacement = c(0,1), cex1 = 1.25)

   overOut$meanDO <- round(overOut$meanDO, 3)
   write.table(overOut, file="datingDirectionalOverlap.txt", append=FALSE, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)


setwd(mainDir)
