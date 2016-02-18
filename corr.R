## this function reports the correlation between data from two monitors
## that has complete cases above the threshold, respectively
## it takes two arguments including directory, 
## and threshold

corr <- function(directory, threshold = 0) {
    correlation <- 0;
    compCase <- complete(directory);
    id <- compCase[compCase[,2] > threshold, 1];
    for (i in id) {
        idi <- formatC(i, width = 3, flag = "0");
        filename <- paste(directory, idi, sep = "/");
        filename <- paste(filename, "csv", sep = ".");
        data <- read.table(filename, header = T, sep = ",");
        good <- complete.cases(data);
        correlation <- append(correlation, cor(data[good, 2],data[good, 3]));
    }
    correlation <- correlation[-1];
}