library(chutils)
equalizeRTandPhit <- TRUE
sinkFilename <- "BICout.txt"



dataFileName <- c("./tqVCtqBModelList", 
									"./liSEModelList",
									"./tqBliSEModelList",
									"./tqVCliSEModelList",
									"./tqVCtqBliSEModelList",
									"./tqBModelList")
									
pars.n <- c(6, 5, 6, 6, 7, 5)

sink(sinkFilename, append = F)
for(i in 1:length(dataFileName)) {
	file_name <- list.files(path = paste("./", dataFileName[i], sep=""), pattern = "Fitted\\.txt$", full.names = TRUE)
	
	df.fitted <- read.table(file_name, header=T, sep="\t", quote="\"")
	out.BIC.final <- getICRRW(df.fitted[df.fitted$correct == TRUE, "pCross"], df.fitted[df.fitted$correct == TRUE, "pHit"], 		df.fitted$rtFit,df.fitted[["rt"]], pars.n[i], equalizeRTandPhit = equalizeRTandPhit, ICtype = "BIC", standardize = T)

	cat("\n\n ******** ", dataFileName[i], "******** \n\n")
	cat("BIC =  ", out.BIC.final, "\n\n")
}
sink(NULL)