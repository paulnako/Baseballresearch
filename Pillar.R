setwd("~/Desktop/R/Baseball/Baseballresearch")

###Read "Batting" and "Master" tables from Lahman Database
master <- read.csv("./Lahman/master.csv")
batting <- read.csv("./Lahman/batting.csv")

###Merge to include player 
data <- merge(master, batting, by = "playerID")

###Add age, k%, BB%, HR%
data[, c("SO", "BB", "HR", "AB")] <- sapply(data[, c("SO", "BB", "HR", "AB")], as.numeric)
data$age <- with(data, yearID - birthYear)
data$Kpct <- with(data, SO/AB)
data$BBpct <- with(data, BB/AB)
data$HRpct <- with(data, HR/AB)

###Clean up data frame
data <- data[,c("nameGiven", "yearID", "age", "Kpct","BBpct", "HRpct")]
data <- data[complete.cases(data),]

###League Averages
LeagueAvgs <- data.frame(tapply(data$Kpct, data$yearID, mean), 
                         tapply(data$BBpct, data$yearID, mean),
                         tapply(data$HRpct, data$yearID, mean))
colnames(LeagueAvgs) <- c("Kpct", "BBpct", "HRpct")



