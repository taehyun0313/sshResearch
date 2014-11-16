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
	count <- 0
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

findRow <- function(data, map){
	conditions <- map$conditions
	values <- map$value
	len <- length(conditions)
	count <- 0
	result <- numeric()
	for (i in 1:nrow(data)){
		boolean <- TRUE;
		for (j in 1:len){
			colname <- conditions[j]
			data.target <- data[, colname]
			current <- data.target[i]
			shouldbe <- values[j]
			if(is.na(current)|is.null(shouldbe)){
				boolean <- FALSE
			}
			if(boolean){
				if(current != shouldbe){
					boolean <- FALSE
				}
			}
		}
		if (boolean){
			count <- count + 1
			result <- c(result, i)
		}
	}
	return(result)
}

#Data is a vector
getVariance<- function(data, colnum){
	data.target <- data[, colnum]	
	sum <- 0
	sum.sqrd <- 0
	count <- 0
	if(length(data.target)!=0){
		for (i in 1:length(data.target)){
			if(!is.na(data.target[i])){
				sum <- sum + data.target[i]
				sum.sqrd <- sum.sqrd + (data.target[i]^2)
				count <- count + 1
			}
		}
	}  	
	result <- c(sum, sum.sqrd, count)
  	return(result)
}

cmdArgs <- commandArgs(trailingOnly = TRUE)

timeseries <- cmdArgs[1]
filename <- cmdArgs[2]
colnum <- as.integer(cmdArgs[3])
map <- splitTS(timeseries)
resdata <- read.csv(filename, header = TRUE)
print('I have read the data')
vector.use <- newFindRow(resdata, map)
print('I have found rows')
final <- resdata[vector.use, ]
print(dim(final))
result <- getVariance(final, colnum)
write.csv(result, file=paste(filename, '.res.tmp', sep=""), row.names=FALSE)
