splitData <- function(filename, n) {
  data <- read.csv(filename, header = TRUE)
  len <- nrow(data)
  temp <- strsplit(filename, split = ".", fixed = "TRUE")[[1]]
  filename <- paste(temp[1:length(temp)-1], collapse=".")
  for (i in 0:(n-1)) {
    data.target <- data[(1 + as.integer(len/n) * i):(as.integer(len/n) * (i + 1)), ]
    write.csv(data.target, file = paste(filename, '-', as.character(i), '.csv', sep = ""), row.names = FALSE)
  }
  
  if (as.integer(len/n) != len/n) {
    data.target <- data[(1 + as.integer(len/n) * n):nrow(data), ]
    write.table(data.target, file = paste(filename, '-', as.character(n-1), '.csv', sep = ""), append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
  }
}
filename <- commandArgs(trailingOnly = TRUE)[[1]]
n <- as.integer(commandArgs(trailingOnly = TRUE)[2])

splitData(filename, n)

