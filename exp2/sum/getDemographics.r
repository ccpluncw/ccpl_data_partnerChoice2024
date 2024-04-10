library (chutils)
library (dplyr)

roundValue <- 2
genPresent <- FALSE
sexPresent <- FALSE
ethPresent <- FALSE
agePresent <- FALSE

params<-ch.readMoralsDBfile("datingDBfile.txt")
data.raw <-read.table(params$moralsTaskDataFile, header=T, sep="\t", quote="\"")

data.final <-read.table("analysisReadyData.gp.txt", header=T, sep="\t", quote="\"")

sink("demographics.txt")

if("Birth" %in% colnames(data.raw))
{
	data.raw.tmp <- data.raw %>% group_by(sn) %>% summarize(age = Birth[1])
	data.final.tmp <- data.final %>% group_by(sn) %>% summarize(age = Birth[1])
	
	rawN <- nrow(data.raw.tmp)
	finalN <- nrow(data.final.tmp)
	
	age.mean.r <- round(mean(data.raw.tmp$age, na.rm = T), roundValue)
	age.sd.r <- round(sd(data.raw.tmp$age, na.rm = T), roundValue)
	age.mean.f <- round(mean(data.final.tmp$age, na.rm = T), roundValue)
	age.sd.f <- round(sd(data.final.tmp$age, na.rm = T), roundValue)
	cat("\n ******************** Age ******************** \n")
	cat("\n\t ********** Raw Data ********** \n")
	cat("\t\t Raw Data N =", rawN,"\n")	
	cat("\t\t Raw Data Mean =", age.mean.r,"\n")	
	cat("\t\t Raw Data SD =", age.sd.r,"\n")	
	cat("\n\t ********** Final Data ********** \n")
	cat("\t\t Raw Data N =", finalN,"\n")	
	cat("\t\t Raw Data Mean =", age.mean.f,"\n")	
	cat("\t\t Raw Data SD =", age.sd.f,"\n")	
	agePresent <- TRUE
} else {
	cat("\n No Age Data \n")
}


if("Gender" %in% colnames(data.raw))
{
	data.raw.tmp <- data.raw %>% group_by(sn) %>% summarize(gender = Gender[1])
	data.final.tmp <- data.final %>% group_by(sn) %>% summarize(gender = Gender[1])
	
	data.raw.tmp$gender <- ifelse(is.na(data.raw.tmp$gender), "not collected", data.raw.tmp$gender)
	data.final.tmp$gender <- ifelse(is.na(data.final.tmp$gender), "not collected", data.raw.tmp$gender)
	
	rawN <- with(data.raw.tmp, sum(unlist(table(gender))))
	finalN <- with(data.final.tmp, sum(unlist(table(gender))))
	
	gender.tbl.r <- with(data.raw.tmp, round(prop.table(table(gender)), roundValue))
	gender.tbl.f <- with(data.final.tmp, round(prop.table(table(gender)), roundValue))
	cat("\n\n ******************** Gender ******************** \n")
	cat("\n\t ********** Raw Data ********** \n")
	cat("\t\t Raw Data N =", rawN,"\n\n")	
	writeLines(paste0("\t\t", capture.output(print(gender.tbl.r, row.names = F))))
	cat("\n\t ********** Final Data ********** \n")
	cat("\t\t Final Data N =", finalN,"\n\n")	
	writeLines(paste0("\t\t", capture.output(print(gender.tbl.f, row.names = F))))
	genPresent <- TRUE
} else {
	cat("\n No Gender Data \n")
}

if("Sex" %in% colnames(data.raw))
{
	data.raw.tmp <- data.raw %>% group_by(sn) %>% summarize(sex = Sex[1])
	data.final.tmp <- data.final %>% group_by(sn) %>% summarize(sex = Sex[1])

	data.raw.tmp$sex <- ifelse(is.na(data.raw.tmp$sex), "not collected", data.raw.tmp$sex)
	data.final.tmp$sex <- ifelse(is.na(data.final.tmp$sex), "not collected", data.raw.tmp$sex)

	rawN <- with(data.raw.tmp, sum(unlist(table(sex))))
	finalN <- with(data.final.tmp, sum(unlist(table(sex))))

	sex.tbl.r <- with(data.raw.tmp, round(prop.table(table(sex)), roundValue))
	sex.tbl.f <- with(data.final.tmp, round(prop.table(table(sex)), roundValue))
	cat("\n\n ******************** Sex ******************** \n")
	cat("\n\t ********** Raw Data ********** \n")
	cat("\t\t Raw Data N =", rawN,"\n\n")	
	writeLines(paste0("\t\t", capture.output(print(sex.tbl.r, row.names = F))))
	cat("\n\t ********** Final Data ********** \n")
	cat("\t\t Final Data N =", finalN,"\n\n")	
	writeLines(paste0("\t\t", capture.output(print(sex.tbl.f, row.names = F))))
	sexPresent <- TRUE
} else {
	cat("\n No Sex Data \n")
}

if(sexPresent & genPresent)
{
	data.raw.tmp <- data.raw %>% group_by(sn) %>% summarize(sex = Sex[1], gender = Gender[1])
	data.final.tmp <- data.final %>% group_by(sn) %>% summarize(sex = Sex[1], gender = Gender[1])

	data.raw.tmp$sex <- ifelse(is.na(data.raw.tmp$sex), "not collected", data.raw.tmp$sex)
	data.final.tmp$sex <- ifelse(is.na(data.final.tmp$sex), "not collected", data.raw.tmp$sex)
	data.raw.tmp$gender <- ifelse(is.na(data.raw.tmp$gender), "not collected", data.raw.tmp$gender)
	data.final.tmp$gender <- ifelse(is.na(data.final.tmp$gender), "not collected", data.raw.tmp$gender)

	rawN <- with(data.raw.tmp, sum(unlist(table(sex,gender))))
	finalN <- with(data.final.tmp, sum(unlist(table(sex,gender))))

	sexGen.tbl.r <- with(data.raw.tmp, round(prop.table(table(sex, gender, dnn=c("sex","gender"))), roundValue))
	sexGen.tbl.f <- with(data.final.tmp, round(prop.table(table(sex, gender, dnn=c("sex","gender"))), roundValue))
	
	cat("\n\n ******************** Sex and Gender ******************** \n")
	cat("\n\t ********** Raw Data ********** \n")
	cat("\t\t Raw Data N =", rawN,"\n\n")	
	writeLines(paste0("\t\t", capture.output(print(sexGen.tbl.r, row.names = F))))
	cat("\n\t ********** Final Data ********** \n")
	cat("\t\t Final Data N =", finalN,"\n\n")	
	writeLines(paste0("\t\t", capture.output(print(sexGen.tbl.f, row.names = F))))
} 

if("Ethnicity" %in% colnames(data.raw))
{
	data.raw.tmp <- data.raw %>% group_by(sn) %>% summarize(ethnicity = Ethnicity[1])
	data.final.tmp <- data.final %>% group_by(sn) %>% summarize(ethnicity = Ethnicity[1])

	data.raw.tmp$ethnicity <- ifelse(is.na(data.raw.tmp$ethnicity), "not collected", data.raw.tmp$ethnicity)
	data.final.tmp$ethnicity <- ifelse(is.na(data.final.tmp$ethnicity), "not collected", data.raw.tmp$ethnicity)

	rawN <- with(data.raw.tmp, sum(unlist(table(ethnicity))))
	finalN <- with(data.final.tmp, sum(unlist(table(ethnicity))))

	ethnicity.tbl.r <- as.data.frame(with(data.raw.tmp, round(prop.table(table(ethnicity)), roundValue)))
	ethnicity.tbl.f <- as.data.frame(with(data.final.tmp, round(prop.table(table(ethnicity)), roundValue)))
	cat("\n\n ******************** Ethnicity ******************** \n")
	cat("\n\t ********** Raw Data ********** \n")
	cat("\t\t Raw Data N =", rawN,"\n\n")	
	writeLines(paste0("\t", capture.output(print(ethnicity.tbl.r, row.names = F))))
	cat("\n\t ********** Final Data ********** \n")
	cat("\t\t Final Data N =", finalN,"\n\n")	
	writeLines(paste0("\t", capture.output(print(ethnicity.tbl.f, row.names = F))))
	ethPresent <- TRUE
} else {
	cat("\n No Ethnicity Data \n")
}

sink(NULL)
