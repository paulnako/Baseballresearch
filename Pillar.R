setwd("~/Desktop/R/Baseball/Baseballresearch")

###Read "Batting", "Master", and fielding tables from Lahman Database
fielding <- read.csv("./Lahman/fielding.csv")
master <- read.csv("./Lahman/master.csv")
batting <- read.csv("./Lahman/batting.csv")

###Merge to include player position, names
matting <- merge(master, batting, by = "playerID")
data <- merge(matting, fielding[, c("playerID", "POS")], by = "playerID")

##Position players post-1955
data <- data[data$yearID >= 1955 & data$POS != "P",]

###Add age, k%, BB%, HR%
data$name <- paste(data$nameFirst, data$nameLast, sep = " ")
data$age <- with(data, yearID - birthYear)
data$PA <- with(data, AB + BB + IBB + HBP + SH + SF, na.rm = TRUE)
data$Kpct <- with(data, SO/PA, na.rm = TRUE)
data$BBpct <- with(data, BB/PA, na.rm = TRUE)
data$HRpct <- with(data, HR/PA, na.rm = TRUE)

###Clean up data frame
clean <- data[,c("playerID", "name", "yearID", "age", "PA", "SO", "BB", 
                 "HR","Kpct","BBpct", "HRpct")]

###League Averages
LeagueAvgs <- data.frame(1955:2013, 
                         tapply(data$Kpct, data$yearID, mean, na.rm = TRUE), 
                         tapply(data$BBpct, data$yearID, mean, na.rm = TRUE),
                         tapply(data$HRpct, data$yearID, mean, na.rm = TRUE))
colnames(LeagueAvgs) <- c("year", "Kpct", "BBpct", "HRpct")

###Add K+, BB+, HR+

final <- clean[c("name", "yearID", "age", "PA", "Kpct", "BBpct", "HRpct")]
final <- final[!duplicated(final),]
final$Kplus <- 1:length(final$yearID)
final$BBplus <- 1:length(final$yearID)
final$HRplus <- 1:length(final$yearID)

###Create function comp

comp <- function(player, yearsOld, margin){
  
  FUNfinal <- final[final$age == yearsOld,]
  
  for (i in 1:length(FUNfinal$yearID)){
    FUNfinal$Kplus[i] <- round((
      FUNfinal$Kpct[i] / LeagueAvgs$Kpct[LeagueAvgs$year == FUNfinal$yearID[i]]) * 100)
    FUNfinal$BBplus[i] <- round((
      FUNfinal$BBpct[i] / LeagueAvgs$BBpct[LeagueAvgs$year == FUNfinal$yearID[i]]) * 100)
    FUNfinal$HRplus[i] <- round((
      FUNfinal$HRpct[i] / LeagueAvgs$HRpct[LeagueAvgs$year == FUNfinal$yearID[i]]) * 100)
  
  }
  
  FUNfinal <- final[c("name", "yearID", "age", "PA", "Kplus", "BBplus", "HRplus")]
  
  finalcomp <-  FUNfinal[FUNfinal$Kplus >= margin - FUNfinal$Kplus[FUNfinal$name == player] &
          FUNfinal$Kplus <= margin + FUNfinal$Kplus[FUNfinal$name == player] &
          FUNfinal$BBplus >= margin - FUNfinal$BBplus[FUNfinal$name == player] &
          FUNfinal$BBplus <= margin + FUNfinal$BBplus[FUNfinal$name == player] &
          FUNfinal$HRplus >= margin - FUNfinal$HRplus[FUNfinal$name == player] &
          FUNfinal$HRplus <= margin + FUNfinal$HRplus[FUNfinal$name == player] &
          complete.cases(FUNfinal),]
    
 return(finalcomp)
}

comp(player = "Jose Bautista", yearsOld = 30, margin = 5)

