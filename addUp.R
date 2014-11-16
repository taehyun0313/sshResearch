original  = commandArgs(trailingOnly=TRUE)[1]
n = as.integer(commandArgs(trailingOnly=TRUE)[2])
sum=0
sum.sqrd=0
count=0
for (i in 0:(n-1)){
  #setwd("/Users/taehyun0313/Desktop/test/data/")
  filename = paste(original,'-',i,".csv.res.tmp", sep="")
  data <- read.csv(filename)

  sum = sum + data[1,]
  sum.sqrd = sum.sqrd + data[2,]
  count = count + data[3,]
}
if(count > 1){
	mean <- sum / count
	var <- (sum.sqrd - ((sum^2) / count)) / (count-1)
}else{
	mean <- sum
	var <- 0
}

result <- c(mean, var, count)
write.csv(result, file = paste(original, '_', as.character(n), '.result', sep = ""), row.names=FALSE)
