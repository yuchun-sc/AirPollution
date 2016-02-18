## this function reports the number of complete cases for each monitor id,
## it takes two arguments including directory, 
## and id numbers (1 ~ 332) 

complete <- function(directory, id = 1:332) {
    noComplete <- c(0, 0);
    
    for (i in id) {
        idi <- formatC(i, width = 3, flag = "0");
        filename <- paste(directory, idi, sep = "/");
        filename <- paste(filename, "csv", sep = ".");
        data <- read.table(filename, header = T, sep = ",");
        good <- complete.cases(data);
        noComplete <- rbind(noComplete, c(i, length(data[good,1])));
    }
    noComplete <- noComplete[-1,];
    noComplete <- matrix(noComplete, ncol = 2);
    dimnames(noComplete) <- list(c(1:length(id)), c("id", "nobs")); 
    noComplete
}