splitTS <- function(timeseries){
	map <- strsplit(timeseries, split=", ")
	x <- map[[1]] #vector of strings
	len = length(x)
	condition.name <- rep(NA, len)
	values <- rep(NA, len)
	for (i in 1:len){
		map2 <- strsplit(x[i], split= "=")
		y <- map2[[1]]
		condition.name[i] <- y[1]
		values[i] <- y[2]
	}
	result <- list("conditions" = condition.name, "value" = values)
	return(result)
}

newFindRow <- function(data, map){
	conditions <- map$conditions
	values <- map$value
	len <- length(conditions)
	print(len)
#	count <- 0
	deter <- logical(nrow(data))
	deter <- !deter
	for (i in 1:len){
		data.target <- data[, conditions[i]]
		filter <- data.target == values[i]
		filter[is.na(filter)] <- FALSE
		deter <- deter & filter
	}
	return(deter)
}

#Data is a vector
getVariance<- function(data){
	#setwd("/ssgprojects/project0002/co00QWIV/data/interwrk/") 
	sum <- 0 #sum of x
	sum.sqrd <- 0 #sum of x^2
	count <- 0
	if(length(data)!=0){
		for (i in 1:length(data)){
			x <- data[i]
			if(!is.na(x)){
			sum <- sum + x
			sum.sqrd <- sum.sqrd + (x^2)
			count <- count + 1
			#print(count)
			}
		}
  	vector <- c(sum, sum.sqrd, count)
  	return(vector)
	}
}
cmdArgs <- commandArgs(trailingOnly = TRUE)

timeseries <- cmdArgs[1]
filename <- cmdArgs[2]
colnum <- as.integer(cmdArgs[3])

#print(timeseries)
#print(filename)
#print(colnum)

map <- splitTS(timeseries)
#print(map)
#colnames <- read.csv(filename, header = TRUE, skip=0, nrows=1)
data <- read.csv(filename)
#colnames(data) <- colnames(colnames)
vector.use <- newFindRow(data, map)
#print(vector.use)
#final <- data[vector.use, colnum]
#result <- getVariance(final)

result <- getVariance(data[, colnum])
print(result)
write.csv(result, file=paste(filename, '.res.tmp', sep=""), row.names=FALSE) 
#x= "/ssgprojects/project0002/co00QWIV/data/interwrk/test_ia_wia_county_naics3_pri.csv"
#y= "creation_date=25APR04, state=19, year=2001, quarter=1, qwi_geocode=C1900000000, countyfmt=19 Iowa, qwi_industry_code=B000000, naics3=0, naics3fmt=All NAICS subsectors, ownercode=A00, ownerfmt=All (1-5), sex=0"


