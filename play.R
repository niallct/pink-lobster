# playing
# Finding Wills

library(stringr)
wills <- D.people[str_detect(D.people$name, "Will.* Day"),]
willbat <- batting[str_detect(batting$batsman_name, "Will.* Day"),]

#1878269    Will Day    4363 Milton
#3716426    Will Day   14237 Champion of the Thames
#4453764 William Day   14848 CUPA
#4456345 William Day       0 
#4500117    Will Day   12697 Reach      

subset(inningses, inningses$club_id_bat==5476 & inningses$actuallyDate=="2019-07-14")

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

# ===========

table(firstinningses$runs)
#todo
# sort out game type eg 20 ov, 35 over, etc
# bins for the run totals 
# logistic regression

#IF NEEDED TO ASSIGN TYPE export the competition data
write.csv(D.competition[,c(1,2)], file="config/competitions.csv")

# ============
a.matchaggregate <- aggregate(Runs ~ match_id, data = B.inningses, sum)
B.inningses[B.inningses$match_id=="3786137",] # a 4 inns
 #============================

B.matches %>%
  select(match_id, innings) %>%
  hoist(innings, ovs = 13)
# ==========================
# bowling all innings
which(B.bowling$O >= B.bowling$inns_ovs_whole %/% 2)
which(B.bowling$O >= B.bowling$inns_ovs_whole %/% 2 & B.bowling$is_us==T & B.bowling$year == 2022)
 #----------

zxc <- F.inningses.us %>% filter(team_batting_id == 9865) %>% arrange(desc(actuallyDate))
zz <- zxc %>% pull(Res) %>% as.vector %>% rle()
xcv <- tibble(l=zz$lengths, v= zz$values)
# zz %>% filter(values == "W") %>% max()
#yy %>% filter(v == "W") %>% slice_max(l)
xcv$cse <- cumsum(xcv$l)
xcv$cs <- xcv$cse - xcv$l + 1
xx <- xcv %>% filter(v == "W") %>% slice_max(l)
xx$Length <- xx$l
xx$FromDate <- zxc[xx$cse,c(6)]
xx$FromOppos <- zxc[xx$cse,c(42)]
xx$ToDate <- zxc[xx$cs,c(6)]
xx$ToOppos <- zxc[xx$cs,c(42)]
