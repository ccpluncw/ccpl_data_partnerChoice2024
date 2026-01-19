library(chValues)
library(chMorals)
library(chutils)

traitFileName <- "OrginalChanged_Stimuli.txt"
promptFileName <- "datingVarNumStimPairs.txt"
dataFileName <- "/Users/dalejcohen/Library/CloudStorage/Dropbox/UNCW/active uncw/research/experiments/values/Experiment Overlaps/MinKatConHealth/MinKatConHealthCombinedValuesStats.txt"
itemAcolNames <- c("IA.1.org", "IA.2.org")
itemBcolNames <- c("IB.1.org", "IB.2.org")

data.raw <-read.table(dataFileName, header=T, sep="\t", quote="\"")

df.trait <-read.table(traitFileName, header=F, sep="\t", quote="\"")
colnames(df.trait) <- c("trt", "org")
df.trait$org <- recode(df.trait$org, religion = "your religion", patience = "other people's patience", honesty = "other people's honesty", "financial stability" = "your financial stability", embarrassment = "your embarrassment", goals = "other people's goals")


df.combs <- read.table(promptFileName, header=T, sep="\t", quote="\"")
df.combs$IA.1.org <- ifelse( df.combs$IA.1.org=="NA", NA ,df.combs$IA.1.org)
df.combs$IA.2.org <- ifelse( df.combs$IA.2.org == "NA", NA ,df.combs$IA.2.org)
df.combs$IB.1.org <- ifelse( df.combs$IB.1.org=="NA", NA ,df.combs$IB.1.org)
df.combs$IB.2.org <- ifelse( df.combs$IB.2.org == "NA", NA ,df.combs$IB.2.org)

df.combs <- subset (df.combs, select = -c(IA.1, IA.2, IB.1, IB.2))

data.raw <- data.raw[data.raw$prompt %in% df.trait$org, ]
data.raw <- data.raw[, c("prompt", "valueMean")]

write.table(data.raw, file="edTraitValues.txt", quote=F, sep="\t", row.names = F, append=F)

maxPref <- max(data.raw$valueMean)
#minPref <- mean(data.raw$valueMean)
#minPref <- min(data.raw$valueMean)
minPref <- quantile(data.raw$valueMean, probs = 0.2)

	
df.edTraits <- merge(df.combs, data.raw, by.x = "IA.1.org", by.y = "prompt", all.x = TRUE)
names(df.edTraits)[names(df.edTraits) == "valueMean"] <- "IA.1.val"

df.edTraits <- merge(df.edTraits, data.raw, by.x = "IA.2.org", by.y = "prompt", all.x = TRUE)
names(df.edTraits)[names(df.edTraits) == "valueMean"] <- "IA.2.val"

df.edTraits <- merge(df.edTraits, data.raw, by.x = "IB.1.org", by.y = "prompt", all.x = TRUE)
names(df.edTraits)[names(df.edTraits) == "valueMean"] <- "IB.1.val"

df.edTraits <- merge(df.edTraits, data.raw, by.x = "IB.2.org", by.y = "prompt", all.x = TRUE)
names(df.edTraits)[names(df.edTraits) == "valueMean"] <- "IB.2.val"

df.edTraits$IB.n <- ifelse(is.na(df.edTraits$IB.1.val) | is.na(df.edTraits$IB.2.val), 1, 2)
df.edTraits$IA.n <- ifelse(is.na(df.edTraits$IA.1.val) | is.na(df.edTraits$IA.2.val), 1, 2)

df.edTraits$IA.1.val <- ifelse(is.na(df.edTraits$IA.1.val), maxPref, df.edTraits$IA.1.val)
df.edTraits$IA.2.val <- ifelse(is.na(df.edTraits$IA.2.val), maxPref, df.edTraits$IA.2.val)
df.edTraits$IB.1.val <- ifelse(is.na(df.edTraits$IB.1.val), maxPref, df.edTraits$IB.1.val)
df.edTraits$IB.2.val <- ifelse(is.na(df.edTraits$IB.2.val), maxPref, df.edTraits$IB.2.val)


#by setting the NA term to == maxPref, the second term becomes 0 and therefore does not influence the outcome
#the critical change is the scaling by n
df.edTraits$IA.mv <-  (-1*sqrt((maxPref - df.edTraits$IA.1.val)^2 + (maxPref - df.edTraits$IA.2.val)^2) + sqrt(df.edTraits$IA.n*maxPref^2))/(sqrt(df.edTraits$IA.n*maxPref^2))

df.edTraits$IB.mv <-  (-1*sqrt((maxPref - df.edTraits$IB.1.val)^2 + (maxPref - df.edTraits$IB.2.val)^2) + sqrt(df.edTraits$IB.n*maxPref^2))/(sqrt(df.edTraits$IB.n*maxPref^2))

df.edTraits$IA.minus.IB <- df.edTraits$IA.mv - df.edTraits$IB.mv

df.edTraits$mvScale <- 1 - (abs(df.edTraits$IA.minus.IB)/max(abs(df.edTraits$IA.minus.IB)))
df.edTraits$mvScale <- ifelse(df.edTraits$mvScale == 0, 0.0001, df.edTraits$mvScale)

df.edTraits$mvRoundScale <- chutils::ch.round_any(df.edTraits$mvScale, 0.1, ceiling)
df.edTraits$mvRound <- chutils::ch.round_any(abs(df.edTraits$IA.minus.IB), 0.05, ceiling)
df.edTraits$mvRoundScale <- 1 - (df.edTraits$mvRound/max(df.edTraits$mvRound))


write.table(df.edTraits, file="datingVarNumStimPairsTraitValues.txt", quote=F, sep="\t", row.names = F, append=F)