# trend analysis of PM 2.5 between 1999 and 2015
# data set can be downloaded from 
# https://aqs.epa.gov/aqsweb/documents/data_mart_welcome.html

data99 <- read.csv("pm25/daily_88101_1999.csv", header = T, na.strings = "")
data15 <- read.csv("pm25/daily_88101_2015.csv", header = T, na.strings = "")

dim(data99)

# if the column names have spaces, the following code can be used to generate 
# valid names for latter usage
# colnames(data15) <- make.names(colnames(data15))

# if the header is read separated for the first line, the following may be used
# cNames <- readLines("pm25/daily_88101_2015.csv", 1)
# cNames <- strsplit(cNames, split = "\",\"", fixed = T)

# grep the observations for PM 2.5, the paramter.code is 88101
data99PM25 <- data99$Arithmetic.Mean
data15PM25 <- data15$Arithmetic.Mean
summary(data99PM25)
summary(data15PM25)
# there is a decrease on the median comparing 15 to 99

# in case there are missing value, the following can be used for computing percentage
# mean(is.na(data99PM25))

boxplot(data99PM25,data15PM25)

# to make the box plot a little bit better scaled 
boxplot(log10(data99PM25),log10(data15PM25))

# notice that there are negative numbers, why?
dates99 <- data99$Date.Local
dates15 <- data15$Date.Local

dates99 <- as.Date(as.character(dates99),"%Y-%m-%d")
dates15 <- as.Date(dates15)

negative99 <- data99PM25 < 0  # note that there is no negative value 
negative15 <- data15PM25 < 0

hist(dates99,"month")
hist(dates15,"month")

# visulize when does negative measurements occur
hist(dates15[negative15],"month")
# one explantion could be measurement error

# analysis for a specific location (a particular monitor)
site99 <- unique(subset(data99, data99$State.Code == 36, c(County.Code, Site.Num)))
site15 <- unique(subset(data15, data15$State.Code == 36, c(County.Code, Site.Num)))

# create dummy varialbes
site99 <- paste(site99[,1], site99[,2], sep = ".")
site15 <- paste(site15[,1], site15[,2], sep = ".")

# find out the common monitors exists in 99 and 15
common <- intersect(site99, site15)

# identify which monitor has more observations than others 
data99$county.site <- paste(data99$County.Code, data99$Site.Num, sep = ".")
data15$county.site <- paste(data15$County.Code, data15$Site.Num, sep = ".")

count99 <- subset(data99, State.Code == 36 & county.site %in% common)
count15 <- subset(data15, State.Code == 36 & county.site %in% common)

# split them by county.site variables, and counts number of observations
sapply(split(count99, count99$county.site), nrow)
sapply(split(count15, count15$county.site), nrow)

# pick 101.3 for comparison
pm99sub <- subset(data99, State.Code == 36 & county.site == 101.3) 
pm15sub <- subset(data15, State.Code == 36 & county.site == 101.3) 

# visulize for this particular s
par(mfrow = c(1,2))
rng <- range(pm99sub$Arithmetic.Mean, pm15sub$Arithmetic.Mean)
plot(as.Date(pm99sub$Date.Local), pm99sub$Arithmetic.Mean, ylim = rng)
abline(h = median(pm99sub$Arithmetic.Mean))
plot(as.Date(pm15sub$Date.Local), pm15sub$Arithmetic.Mean, ylim = rng)
abline(h = median(pm15sub$Arithmetic.Mean))

# average value by state
mean99 <- with(data99, tapply(Arithmetic.Mean, State.Code, mean))
mean15 <- with(data15, tapply(Arithmetic.Mean, State.Code, mean))

mean99 <- data.frame(state = names(mean99), mean = mean99)
mean15 <- data.frame(state = names(mean15), mean = mean15)

meanMerge <- merge(mean99, mean15, by = "state")

rng <- range(meanMerge[,2:3])

par(mfrow = c(1,1))
with(meanMerge, plot(rep(1999,53), meanMerge[,2], xlim = c(1998, 2016), ylim = rng))
with(meanMerge, points(rep(2015,53), meanMerge[,3]))
segments(rep(1999,53), meanMerge[,2], rep(2015,53), meanMerge[,3])
     



