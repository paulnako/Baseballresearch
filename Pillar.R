setwd("~/Desktop/R/Baseball/Baseballresearch")

###Read "Batting", "Master", and fielding tables from Lahman Database
fielding <- read.csv("./Lahman/fielding.csv")
master <- read.csv("./Lahman/master.csv")
batting <- read.csv("./Lahman/batting.csv")

###Merge to include player 
matting <- merge(master, batting, by = "playerID")
data <- merge(matting, fielding, by = "playerID")
data <- data[data$yearID.x >= 1955 & data$POS != "P",]

###Add age, k%, BB%, HR%

data$age <- with(data, yearID.x - birthYear)
data$PA <- with(data, AB + BB + IBB + HBP + SH + SF, na.rm = TRUE)
data$Kpct <- with(data, SO/PA, na.rm = TRUE)
data$BBpct <- with(data, BB/PA, na.rm = TRUE)
data$HRpct <- with(data, HR/PA, na.rm = TRUE)


###Clean up data frame
data <- data[,c("nameFirst", "nameLast", "yearID.x", "age", "PA", "SO", "BB", 
                 "HR","Kpct","BBpct", "HRpct")]
data <- unique(data[data$age == 25,])

###League Averages
LeagueAvgs <- data.frame(1955:2013, tapply(data$Kpct, data$yearID, mean, na.rm = TRUE), 
                         tapply(data$BBpct, data$yearID, mean, na.rm = TRUE),
                         tapply(data$HRpct, data$yearID, mean, na.rm = TRUE))
colnames(LeagueAvgs) <- c("year", "Kpct", "BBpct", "HRpct")
LeagueAvgs

###Add K+, BB+, HR+
data$K+ <- 1:length(data)
data$Kplus <- 1:length(data$yearID)
data$BBplus <- 1:length(data$yearID)
data$HRplus <- 1:length(data$yearID)

for (i in 1:length(data$yearID)){
  data$Kplus[i] <- round((
    data$Kpct[i] / LeagueAvgs$Kpct[LeagueAvgs$year == data$yearID[i]]) * 100)
  data$BBplus[i] <- round((
    data$BBpct[i] / LeagueAvgs$BBpct[LeagueAvgs$year == data$yearID[i]]) * 100)
  data$HRplus[i] <- round((
    data$HRpct[i] / LeagueAvgs$HRpct[LeagueAvgs$year == data$yearID[i]]) * 100)
}





