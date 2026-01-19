library(chutils)
library(chMorals)

edValueFileName <- "datingVarNumStimPairsTraitValues.txt"
dataFileName <- "../../mergedDatingData.txt"
item1cols <- c("IA.1.org", "IA.2.org")
item2cols <- c("IB.1.org", "IB.2.org")
overlapItem1cols <- c("IA.1.org", "IA.2.org")
overlapItem2cols <- c("IB.1.org", "IB.2.org")
fitCol <- "fit.RT"
resCol <- "res.RT"
useTwoParameterModel <- TRUE


params<-ch.readMoralsDBfile("datingDBfile.txt")
statsOutputFile <- "edStatsOut.txt"


df.raw <- read.table(dataFileName, header=T, sep="\t", quote="\"")
df.edValues <- read.table(edValueFileName, header=T, sep="\t", quote="\"")

nRowsData <- nrow(df.raw)
df.data.all <- merge(df.raw, df.edValues, by.x=c(item1cols, item2cols), by.y=c(overlapItem1cols, overlapItem2cols))
nRowsMerged <- nrow(df.data.all)
if(nRowsData != nRowsMerged) {
  cat("The number of rows in the original dataset (", nRowsData, ") does not equal the number of rows in the dataset once it is merged with the overlaps (", nRowsMerged, "). You should find out why.  Generally, this is because there are stimuli in the choice dataset that are missing in the values dataset. Alternatively, it might be that you have duplicate rows in the item columns in you overlap file.")
  stop()
}

# df.data.all$mvRound <- chutils::ch.round_any(abs(df.data.all$IA.minus.IB), 0.05, ceiling)
# df.data.all$mvRoundScale <- 1 - (df.data.all$mvRound/max(df.data.all$mvRound))


df.data.all$keybRT <- log(df.data.all$keybRT)
df.data.all$correct01 <- ifelse((df.data.all$IA.minus.IB > 0 & df.data.all$keyDef == "Left") | (df.data.all$IA.minus.IB < 0 & df.data.all$keyDef == "Right"), 1, 0)

### get HVO quantity
df.data.all$HVOq <- ifelse((df.data.all$QuantOption1 == 2 & df.data.all$IA.minus.IB > 0) | (df.data.all$QuantOption2 == 2 & df.data.all$IA.minus.IB < 0) , 2, 1)
### get LVO quantity
df.data.all$LVOq <- ifelse((df.data.all$QuantOption1 == 2 & df.data.all$IA.minus.IB < 0) | (df.data.all$QuantOption2 == 2 & df.data.all$IA.minus.IB > 0) , 2, 1)


 analysisReadyData <- ch.moralsFilterDataQ(df.data.all, "sn", "keybRT", "mvRoundScale", "correct01",c(1,0), statsOutputFile = statsOutputFile, params = params)
 analysisReadyData.sn <- ch.moralsSnRTpHit(analysisReadyData, "sn", "trial", "keybRT", fitCol, resCol, "mvScale", "correct01", c(1,0),  useTwoParameterModel = useTwoParameterModel, params = params, minUniqueOverlaps = 4)

write.table(analysisReadyData.sn, file="analysisReadyData.sn.txt", quote=F, sep="\t", row.names = F, append=F)
 # df.sum.pHVO <- df.data.all %>% group_by(mvRoundScale, HVOq, LVOq) %>% summarize(mCor = mean(correct01, na.rm = T))
 # df.sum.RT.i <- df.data.all[df.data.all$correct01 == 0, ] %>% group_by(mvRoundScale, HVOq, LVOq) %>% summarize(mRT = mean(keybRT, na.rm = T))
 # df.sum.RT.i$correct <- FALSE
 # df.sum.RT.c <- df.data.all[df.data.all$correct01 == 1, ] %>% group_by(mvRoundScale, HVOq, LVOq) %>% summarize(mRT = mean(keybRT, na.rm = T))
 # df.sum.RT.c$correct <- TRUE

df.sum.pHVO <- analysisReadyData.sn %>% group_by(mvRoundScale, HVOq, LVOq) %>% summarize(mCor = mean(correct01, na.rm = T))
df.sum.RT.i <- analysisReadyData.sn[analysisReadyData.sn$correct01 == 0, ] %>% group_by(mvRoundScale, HVOq, LVOq) %>% summarize(mRT = mean(res.RT, na.rm = T))
df.sum.RT.i$correct <- FALSE
df.sum.RT.c <- analysisReadyData.sn[analysisReadyData.sn$correct01 == 1, ] %>% group_by(mvRoundScale, HVOq, LVOq) %>% summarize(mRT = mean(res.RT, na.rm = T))
df.sum.RT.c$correct <- TRUE

pdf("responseFigs.pdf")
with(df.sum.pHVO[df.sum.pHVO$HVOq == 1 & df.sum.pHVO$LVOq == 1,], plot(mCor ~ mvRoundScale, ylim = c(0,1), xlim= c(0,1), pch=16, type = "b", frame = F))
with(df.sum.pHVO[df.sum.pHVO$HVOq == 2 & df.sum.pHVO$LVOq == 2,], points(mCor ~ mvRoundScale, ylim = c(0,1), xlim= c(0,1), col = "grey", pch=16, type = "b"))
with(df.sum.pHVO[df.sum.pHVO$HVOq == 2 & df.sum.pHVO$LVOq == 1,], points(mCor ~ mvRoundScale, ylim = c(0,1), xlim= c(0,1), col = "red", pch=16, type = "b"))
with(df.sum.pHVO[df.sum.pHVO$HVOq == 1 & df.sum.pHVO$LVOq == 2,], points(mCor ~ mvRoundScale, ylim = c(0,1), xlim= c(0,1), col = "blue", pch=16, type = "b"))
abline(h=0.5, lty = 2, col = "grey")

with(df.sum.RT.c[df.sum.RT.c$HVOq == 1 & df.sum.RT.c$LVOq == 1,], plot(mRT ~ mvRoundScale, ylim= c(-.5,0.5), xlim= c(0,1), pch=16, type = "b", frame = F))
with(df.sum.RT.c[df.sum.RT.c$HVOq == 2 & df.sum.RT.c$LVOq == 2,], points(mRT ~ mvRoundScale, xlim= c(0,1), col = "grey", pch=16, type = "b"))
with(df.sum.RT.c[df.sum.RT.c$HVOq == 2 & df.sum.RT.c$LVOq == 1,], points(mRT ~ mvRoundScale,  xlim= c(0,1), col = "red", pch=16, type = "b"))
with(df.sum.RT.c[df.sum.RT.c$HVOq == 1 & df.sum.RT.c$LVOq == 2,], points(mRT ~ mvRoundScale, xlim= c(0,1), col = "blue", pch=16, type = "b"))

with(df.sum.RT.i[df.sum.RT.i$HVOq == 1 & df.sum.RT.i$LVOq == 1,], plot(mRT ~ mvRoundScale, ylim= c(-.5,0.5), xlim= c(0,1), pch=16, type = "b", frame = F))
with(df.sum.RT.i[df.sum.RT.i$HVOq == 2 & df.sum.RT.i$LVOq == 2,], points(mRT ~ mvRoundScale, xlim= c(0,1), col = "grey", pch=16, type = "b"))
with(df.sum.RT.i[df.sum.RT.i$HVOq == 2 & df.sum.RT.i$LVOq == 1,], points(mRT ~ mvRoundScale, xlim= c(0,1), col = "red", pch=16, type = "b"))
with(df.sum.RT.i[df.sum.RT.i$HVOq == 1 & df.sum.RT.i$LVOq == 2,], points(mRT ~ mvRoundScale, xlim= c(0,1), col = "blue", pch=16, type = "b"))

dev.off()