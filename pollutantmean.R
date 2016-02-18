## this function calculates the mean of a pollutant, it takes three arguments
## including directory, pollutant measure (sulfrate/nitrate), 
## and id numbers (1 ~ 332) 

pollutantmean <- function(directory, pollutant, id = 1:332) {
    pollutantData = 0;
    for (i in id) {
        idi <- formatC(i, width = 3, flag = "0");
        filename = paste(directory, idi, sep = "/");
        filename = paste(filename, "csv", sep = ".");
        data <- read.table(file = filename, header = T, sep = ",");
        good <- complete.cases(data);
        pollutantData <- append(pollutantData, data[good,pollutant]);    
    }
    mean(pollutantData[-1])
}