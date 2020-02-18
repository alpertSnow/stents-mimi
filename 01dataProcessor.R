library(dplyr)

# read data
inputDF <- read.csv('data.csv', header = FALSE, skip = 1, stringsAsFactors = FALSE)
headers <- read.csv('headers.csv', header = TRUE)
colnames(inputDF) <- t(headers[1,])
items <- c(unique(t(headers[2,])))[-1]  # -1 remove ""
items.chn <- c(unique(t(headers[3,])))[-1] # chinese

# replace strings as 0 except for the first 3 columns.
values <- sapply(inputDF[, -(1:3)], as.numeric)
values[is.na(values)] <- 0
dataSheet <- cbind(inputDF[, 1:3], values)
colnames(dataSheet) <- t(headers[1,])
dataSheet[, -(1:3)] <- sapply(dataSheet[, -(1:3)], as.numeric)

# process
output <- levels(as.factor(dataSheet[, 1])) # prov names
for (i in items){
        i.quant <- paste0(i, '.quant')
        i.price <- paste0(i, '.price')
        grouped <- group_by(dataSheet, dataSheet[, 1])
        sumproducts <- summarise(grouped, sumproduct = sum(eval(parse(text = i.quant)) * eval(parse(text = i.price))))
        output <- cbind(output, sumproducts[,2])
}
colnames(output) <- c(colnames(headers)[1], items.chn)

# write
write.csv(output, 'output.csv', row.names = FALSE)
