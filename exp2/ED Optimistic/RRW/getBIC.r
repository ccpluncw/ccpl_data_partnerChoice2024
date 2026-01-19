library(chutils)
equalizeRTandPhit <- TRUE
sinkFilename <- "BICout.txt"



dataFileName <- c("./tqVCtqBModelList/Dec_18_2025_13-25_dating_tqVCtqBModelList Fitted.txt", 
									"./liSEModelList/Dec_17_2025_20-01_dating_liSEModelList Fitted.txt",
									"./tqBliSEModelList/Dec_17_2025_21-31_dating_tqBliSEModelList Fitted.txt",
									"./tqVCliSEModelList/Dec_18_2025_03-20_dating_tqVCliSEModelList Fitted.txt",
									"./tqVCtqBliSEModelList/Dec_18_2025_07-12_dating_tqVCtqBliSEModelList Fitted.txt",
									"./tqBModelList/Dec_18_2025_16-19_dating_tqBModelList Fitted.txt")
									
pars.n <- c(6, 5, 6, 6, 7, 5)

sink(sinkFilename, append = F)
for(i in 1:length(dataFileName)) {
	
	df.fitted <- read.table(dataFileName[i], header=T, sep="\t", quote="\"")
	out.BIC.final <- getICRRW(df.fitted[df.fitted$correct == TRUE, "pCross"], df.fitted[df.fitted$correct == TRUE, "pHit"], 		df.fitted$rtFit,df.fitted[["rt"]], pars.n[i], equalizeRTandPhit = equalizeRTandPhit, ICtype = "BIC", standardize = T)

	cat("\n\n ******** ", dataFileName[i], "******** \n\n")
	cat("BIC =  ", out.BIC.final, "\n\n")
}
sink(NULL)