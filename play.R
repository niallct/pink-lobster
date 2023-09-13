# A is ad-hoc, working things to make output
# B is the big, all-time, all-in sets
# C not used
# D is lookup sets
# E is the CURRENT dataset loads
# F is filtered and the main lot to use
# G is ARCHIVE dataset loads
# Z is old stuff I am keeping around

# Finding Wills # Nath is  4203792
library(stringr)
wills <- D.people.3[str_detect(D.people.3$name, "Will.* Day"),]
wills <- wills[!is.na(wills$name),]
willbat <- batting[str_detect(batting$batsman_name, "Will.* Day"),]
# id    name    club_id
# 1853009    William Day    1691 Castleford CC
# 1878269    Will Day    12697 Reach CC
# 1878269    Will Day    4363 Milton
# 3682607    Will Day    0
# 3716426    Will Day    14237 Champion of the Thames
# 4453764    William Day    14848 CUPA
# 4456345    William Day    0
# 4500117    Will Day    12697 Reach CC
# 5028307 Niall Taylor NA
# 5147629 Niall Taylor NA
# 4952911 Niall Taylor NA

# # package `cricketdata`
# Download data on some players
EllysePerry <- fetch_player_data(275487, "T20", "batting")
RahulDravid <- fetch_player_data(28114, "ODI", "fielding")
LasithMalinga <- fetch_player_data(49758, "Test", "bowling")
# Create a plot for Ellyse Perry's T20 scores
library(dplyr)
library(ggplot2)
EllysePerry %>% filter(!is.na(Runs)) %>%
  ggplot(aes(x=Start_Date, y=Runs, col = Dismissal, na.rm = TRUE)) + geom_point() +
  ggtitle("Ellyse Perry's T20 Scores")

#============================
# some sort of allrounder omnitable
zxc <- full_join(E.batting, E.bowling, by = c("match_id" = "match_id", "batsman_id" = "bowler_id"),
                 keep = F, na_matches = "never")

# === 
innsDFy <- apply(B.matches, 1,
                 function(x){
                   results <- data.frame(matrix(ncol=length(innsHdrs), nrow=0))
                   names(results) <- innsHdrs
                   teamNames <- x$innings$team_batting_name
                   z <- length(teamNames)
                   y <- x$players
                   #print(x$match_id)
                   #browser()
                   for (i in 1:z){
                     #print(i)
                     #browser()
                     if ((is.data.frame(y)) && (nrow(y) != 0)){
                       results <- rbind(results,
                                        data.frame(teamName=teamNames[i],
                                                   match_innings = i,
                                                   match_id = x$match_id,
                                                   match_date = x$match_date,
                                                   actuallyDate = x$actuallyDate,
                                                   niceDate = x$niceDate,
                                                   year = x$year,
                                                   ground = x$ground_name,
                                                   ground_id = x$ground_id,
                                                   competition_id = x$competition_id,
                                                   result_des = x$result_description,
                                                   winner = x$result_applied_to,
                                                   home_team_id = x$home_team_id,
                                                   away_team_id = x$away_team_id,
                                                   y[[i]][i,innsHdrs])) # gok how to do this
                     }
                     #browser()
                   }
                   return(results)
                   #browser()
                 })
