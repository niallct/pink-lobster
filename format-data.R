# load stuff
library(tidyr)
library(yaml)
library(dplyr)
library(stringr)

## load the config
conf <- yaml.load_file("./config/config.yaml")

# define a load of interesting(ish) headers
matchMainHdrs <- c("match_id", "match_date", "ground_name", "ground_id", "competition_id","league_id",
                   "home_team_name", "home_team_id", "home_club_name", "home_club_id",
                   "away_team_name", "away_team_id", "away_club_name", "away_club_id", "toss", "result_description", 
                  "result_applied_to","innings")

#subset the interesting headers
B.matches.A <- D.match[matchMainHdrs]
B.matches.B <- D.match.2[matchMainHdrs]

# sort out variable types
B.matches.A$match_id <- as.integer(B.matches.A$match_id)
B.matches.A$ground_id <- as.integer(B.matches.A$ground_id)
B.matches.A$competition_id <- as.integer(B.matches.A$competition_id)
B.matches.A$league_id <- as.integer(B.matches.A$league_id)
B.matches.A$home_team_id <- as.integer(B.matches.A$home_team_id)
B.matches.A$home_club_id <- as.integer(B.matches.A$home_club_id)
B.matches.A$away_team_id <- as.integer(B.matches.A$away_team_id)
B.matches.A$away_club_id <- as.integer(B.matches.A$away_club_id)
B.matches.A$result_applied_to <- as.integer(B.matches.A$result_applied_to)


# merge old and new  match data
# A is all clubs 2019-04-03 -- 2022-09-04, 8611 matches
# B is all Milton. 1409 total, of which 1194 from 2018 or earlier
# so should be 9805 total matches
# fiddle -- get rid of newer data from the B dataset
B.matches.B <- filter( B.matches.B, as.Date(B.matches.B$match_date, "%d/%m/%Y") < "2018-12-31")
B.matches <- tibble(union(B.matches.A, B.matches.B))
#B.matches <- tibble(B.matches.A)
# B.matches <- D.match[matchMainHdrs]
rm(B.matches.A); rm(B.matches.B)

innIndivHrds <- c("teamName", "teamId", "match_innings", "match_id", "match_date" , "ground", "competition_id","result_des", "winner","home_team_id","away_team_id", "inns_total")

innsHdrs <- c("team_batting_name", "team_batting_id", # is team id, not club id
              "innings_number", #of the team, not of the match
              "extra_byes", "extra_leg_byes", "extra_wides", "extra_no_balls", "extra_penalty_runs", "penalties_runs_awarded_in_other_innings",
              "total_extras",
              "runs", "wickets","overs", "balls", 
              "declared", "forfeited_innings",
              "revised_target_runs", "revised_target_overs","revised_target_balls")

batHrds <- c("position"   ,  "batsman_name",
"batsman_id"  ,"how_out",  "fielder_name" ,"fielder_id",  "bowler_name",
"bowler_id" ,   "runs", "fours",        "sixes" ,       "balls"  )

innbatHrds <- c(innIndivHrds, batHrds)

fowHrds <- c("runs", "wickets", "batsman_out_name","batsman_out_id", "batsman_in_name", "batsman_in_id", "batsman_in_runs")

innFOWHdrs <- c(innIndivHrds, fowHrds)

bowlHdrs <- c("bowler_name", "bowler_id", "overs", "maidens", "runs", "wickets", "wides", "no_balls")

innBowlHdrs <- c(innIndivHrds, bowlHdrs)

#fix the dates
B.matches$actuallyDate <- as.Date(B.matches$match_date, "%d/%m/%Y")
B.matches$niceDate <- as.character(B.matches$actuallyDate, format = "%d %b %Y")
B.matches$year <- as.character(B.matches$actuallyDate, format = "%Y")

# # home, away, which is batting side
# B.matches$innings$home_or_away[B.matches$home_team_id == B.matches$innings$team_batting_id] <- "H"
# B.matches$innings$home_or_away[B.matches$away_team_id == B.matches$innings$team_batting_id] <- "A"
# B.matches$innings[[1]]$team_batting_id


# go through the match data and pull out batting stats
battingDFs <- apply(B.matches, 1,
                       function(x){
                         teamNames <- x$innings$team_batting_name
                         teamIds <- x$innings$team_batting_id
                         y <- x$innings$bat
                         #print(x$match_id)
                         #browser()
                         results <- data.frame(matrix(ncol=length(innbatHrds), nrow=0))
                         names(results) <- innbatHrds
                         for (i in 1:length(y)){
                           #print(i)
                           #browser()
                           if ((is.data.frame(y[[i]])) && nrow(y[[i]] != 0)){
                             results <- rbind(results,
                                              data.frame(team_batting_name=teamNames[i],
                                                         team_batting_id=teamIds[i],
                                                         match_innings = i,
                                                         match_date = x$match_date,
                                                         match_id = x$match_id,
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
                                                        # browser(),
                                                         inns_total = x$innings$runs[[i]],
                                                         y[[i]][,batHrds]))
                           }
                           #browser()
                         }
                         return(results)
                         #browser()
                       })

batting <- tibble(do.call(rbind,battingDFs))
rm(battingDFs)

# go through match df and pull out bowling stats
bowlingDFs <- apply(B.matches, 1,
                    function(x){
                      teamNames <- x$innings$team_batting_name
                      teamIds <- x$innings$team_batting_id
                      y <- x$innings$bowl
                      #print(x$match_id)
                      #browser()
                      results <- data.frame(matrix(ncol=length(innBowlHdrs), nrow=0))
                      names(results) <- innBowlHdrs
                      for (i in 1:length(y)){
                        #print(i)
                        #browser()
                        if ((is.data.frame(y[[i]])) && nrow(y[[i]] != 0)){
                          results <- rbind(results,
                                           data.frame(team_batting_name=teamNames[i],
                                                      team_batting_id=teamIds[i],
                                                      match_innings = i,
                                                      match_date = x$match_date,
                                                      actuallyDate = x$actuallyDate,
                                                      niceDate = x$niceDate,
                                                      year = x$year,
                                                      match_id = x$match_id,
                                                      ground = x$ground_name,
                                                      ground_id = x$ground_id,
                                                      competition_id = x$competition_id,
                                                      result_des = x$result_description,
                                                      winner = x$result_applied_to,
                                                      home_team_id = x$home_team_id,
                                                      away_team_id = x$away_team_id,
                                                      inns_total = x$innings$runs[[i]],
                                                      inns_wkts = x$innings$wickets[[i]],
                                                      inns_ovs = x$innings$overs[[i]],
                                                      y[[i]][,bowlHdrs]))
                        }
                       # browser()
                      }
                      return(results)
                      #browser()
                    })

B.bowling <- do.call(rbind,bowlingDFs)
rm(bowlingDFs)

# go through match df and pull out innings stats
innsDFy <- apply(B.matches, 1,
                    function(x){
                      results <- data.frame(matrix(ncol=length(innsHdrs), nrow=0))
                      names(results) <- innsHdrs
                      teamNames <- x$innings$team_batting_name
                      z <- length(teamNames)
                      y <- x$innings
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
                                                      y[i,innsHdrs]))
                        }
                        #browser()
                      }
                      return(results)
                      #browser()
                    })

inningses <- do.call(rbind,innsDFy)
rm(innsDFy)

# =======================
# Sort out team lookup
# Dump out a csv so we can manually assign which are youth teams

D.team$cclub_id <- as.character(D.team$club_id)
foo <- left_join(D.team, D.club, by=c("cclub_id"="id"))
foo$team_id <- foo$id
foo$team_name <- foo$name.x
foo$club_name <- foo$name.y
C.teamlook <- foo[,c("team_id", "team_name","club_id", "club_name")]
C.teamlook$team_id <- as.integer(C.teamlook$team_id)
rm(foo)
# write.csv(C.teamlook, file="./output/teams-out.csv")
# 
# # get a list of our team's players
# # TODO add this

# ========================================
# change runs and balls data to be numeric
inningses$runs[inningses$runs == ""] <- NA
inningses$runs <- as.numeric(inningses$runs)

batting$runs[batting$runs == ""] <- NA
batting$runs <- as.numeric(batting$runs)
batting$balls[batting$balls ==""] <- NA
batting$balls <- as.numeric(batting$balls)
batting$fours[batting$fours ==""] <- NA
batting$fours <- as.numeric(batting$fours)
batting$sixes[batting$sixes ==""] <- NA
batting$sixes <- as.numeric(batting$sixes)

zxc <- c("wickets", "extra_byes", "extra_leg_byes", "extra_wides", "extra_no_balls", "extra_penalty_runs", "penalties_runs_awarded_in_other_innings","total_extras")
for(x in zxc){
  inningses[,x] <- as.numeric(inningses[,x])
  
}
rm(zxc); rm(x)

zxc <- c("overs", "maidens", "runs", "wickets", "wides", "no_balls")
for(x in zxc){
  B.bowling[,x] <- as.numeric(B.bowling[,x])
  
}
rm(zxc); rm(x)


inningses$score <- paste(inningses$runs,"/",inningses$wickets, sep="")


# make a column showing whether an inns is home or away
inningses$home_or_away[inningses$home_team_id == inningses$team_batting_id] <- "H"
inningses$home_or_away[inningses$away_team_id == inningses$team_batting_id] <- "A"
batting$home_or_away <- NA
batting$home_or_away[batting$home_team_id == batting$team_batting_id] <- "H"
batting$home_or_away[batting$away_team_id == batting$team_batting_id] <- "A"

B.bowling$home_or_away <- NA
B.bowling$home_or_away[B.bowling$home_team_id == B.bowling$team_batting_id] <- "A"
B.bowling$home_or_away[B.bowling$away_team_id == B.bowling$team_batting_id] <- "H"


# assign fielding side id: use home and away ids as the team list and make fielding side 
# the one which isn't batting; but warn if nonsense listed.

inningses$team_fielding_id <- NA
for (i in seq(1:length(inningses$team_batting_id))){
  if(!inningses$team_batting_id[i] %in% c(inningses$home_team_id[i], inningses$away_team_id[i])){warning(c("Batting team not listed as playing in innings data, line ",i))}
  if(inningses$team_batting_id[i] == inningses$home_team_id[i]){inningses$team_fielding_id[i]<-inningses$away_team_id[i]}
  if(inningses$team_batting_id[i] == inningses$away_team_id[i]){inningses$team_fielding_id[i]<-inningses$home_team_id[i]}
}

batting$team_fielding_id <- NA
for (i in seq(1:length(batting$team_batting_id))){
  if(!batting$team_batting_id[i] %in% c(batting$home_team_id[i], batting$away_team_id[i])){warning(c("Batting team not listed as playing in batting data, line ",i))}
  if(batting$team_batting_id[i] == batting$home_team_id[i]){batting$team_fielding_id[i]<-batting$away_team_id[i]} else {batting$team_fielding_id[i]<-batting$home_team_id[i]}
}

B.bowling$team_fielding_id <- NA
for (i in seq(1:length(B.bowling$team_batting_id))){
  if(!B.bowling$team_batting_id[i] %in% c(B.bowling$home_team_id[i], B.bowling$away_team_id[i])){warning(c("Batting team not listed as playing in bowling data, line ",i))}
  if(B.bowling$team_batting_id[i] == B.bowling$home_team_id[i]){B.bowling$team_fielding_id[i]<-B.bowling$away_team_id[i]} else {B.bowling$team_fielding_id[i]<-B.bowling$home_team_id[i]}
}

inningses$team_batting_id <- as.integer(inningses$team_batting_id)
batting$team_batting_id <- as.integer(batting$team_batting_id)
B.bowling$team_batting_id <- as.integer(B.bowling$team_batting_id)

# look up team names and club names  # TODO work out how to do this in the main matches df # or just bring in the existing data, d'oh.
inningses <- left_join(inningses,C.teamlook, by=c("team_batting_id"="team_id"))
inningses <- left_join(inningses,C.teamlook, by=c("team_fielding_id"="team_id"), suffix=c("_bat","_field"))

batting <- left_join(batting,C.teamlook, by=c("team_batting_id"="team_id"))
batting <- left_join(batting,C.teamlook, by=c("team_fielding_id"="team_id"), suffix=c("_bat","_field"))

B.bowling <- left_join(B.bowling,C.teamlook, by=c("team_batting_id"="team_id"))
B.bowling <- left_join(B.bowling,C.teamlook, by=c("team_fielding_id"="team_id"), suffix=c("_bat","_field"))

B.bowling$team_fielding_name <- paste(B.bowling$club_name_field, B.bowling$team_name_field, sep=" - ")

# put in results from perspective of batting side # TODO work out how to do this in the main matches df
inningses$bats_result <- NA
inningses$bats_result[inningses$result_des=="Abandoned"] <- "A"
inningses$bats_result[inningses$result_des=="Tied"] <- "T"
inningses$bats_result[inningses$result_des=="Cancelled"] <- "C"
inningses$bats_result[inningses$result_des=="Draw"] <- "D"
inningses$bats_result[inningses$result_des=="Drawn"] <- "D"
inningses$bats_result[inningses$result_des=="Trophy Shared"] <- "D"
inningses$bats_result[inningses$result_des=="Match In Progress"] <- "?"
inningses$bats_result[inningses$winner == "" & is.na(inningses$bats_result)] <- "?" #strangeness
inningses$bats_result[inningses$winner == inningses$team_batting_id] <- "W"
inningses$bats_result[inningses$winner == inningses$team_fielding_id] <- "L"

inningses$bats_result <- as.factor(inningses$bats_result)
inningses$home_or_away <- as.factor(inningses$home_or_away)

batting$bats_result <- NA
batting$bats_result[batting$result_des=="Abandoned"] <- "A"
batting$bats_result[batting$result_des=="Tied"] <- "T"
batting$bats_result[batting$result_des=="Cancelled"] <- "C"
batting$bats_result[batting$result_des=="Draw"] <- "D"
batting$bats_result[batting$result_des=="Drawn"] <- "D"
batting$bats_result[batting$result_des=="Trophy Shared"] <- "D"
batting$bats_result[batting$result_des=="Match In Progress"] <- "?"
batting$bats_result[batting$winner == "" & is.na(batting$bats_result)] <- "?"
batting$bats_result[batting$winner == batting$team_batting_id] <- "W"
batting$bats_result[batting$winner == batting$team_fielding_id] <- "L"

batting$bats_result <- as.factor(batting$bats_result)
batting$home_or_away <- as.factor(batting$home_or_away)

B.bowling$bowls_result <- NA
B.bowling$bowls_result[B.bowling$result_des=="Abandoned"] <- "A"
B.bowling$bowls_result[B.bowling$result_des=="Tied"] <- "T"
B.bowling$bowls_result[B.bowling$result_des=="Cancelled"] <- "C"
B.bowling$bowls_result[B.bowling$result_des=="Draw"] <- "D"
B.bowling$bowls_result[B.bowling$result_des=="Drawn"] <- "D"
B.bowling$bowls_result[B.bowling$result_des=="Trophy Shared"] <- "D"
B.bowling$bowls_result[B.bowling$result_des=="Match In Progress"] <- "?"
B.bowling$bowls_result[B.bowling$winner == "" & is.na(B.bowling$bowls_result)] <- "?"
B.bowling$bowls_result[B.bowling$winner == B.bowling$team_batting_id] <- "W"
B.bowling$bowls_result[B.bowling$winner == B.bowling$team_fielding_id] <- "L"

B.bowling$bowls_result <- as.factor(B.bowling$bowls_result)
B.bowling$home_or_away <- as.factor(B.bowling$home_or_away)

# =======================
# CALCULATING THINGS

# calculate strike rate but use NA if balls faced not recorded
batting$SR <- format(round(batting$runs * 100 / batting$balls, 2), nsmall = 2)
batting$SR[batting$balls==0 & batting$runs!=0] <- NA


# proportion of a sides runs scored
batting$contributionpc <- batting$runs / as.numeric(batting$inns_total)
batting$contributionpc[batting$inns_total==0] <- NA
batting$nicecontrib <- paste(format(batting$contributionpc * 100, digits = 1),"%")

#bowling average
B.bowling$Avg <- as.numeric(format(B.bowling$runs / B.bowling$wickets, digits = 2))
B.bowling$Avg[B.bowling$wickets == 0] <- NA

# bowling balls #TODO make this a function
B.bowling$ovs.o <- str_split_i(B.bowling$overs, "\\.", 1)
B.bowling$ovs.b <- str_split_i(B.bowling$overs, "\\.", 2)
B.bowling$ovs.b[is.na(B.bowling$ovs.b)] <- 0

B.bowling$inns_ovs_whole <- as.integer(str_split_i(B.bowling$inns_ovs, "\\.", 1))
B.bowling$inns_ovs_whole[B.bowling$inns_ovs_whole == 0] <- NA

B.bowling$BB <- (as.numeric(B.bowling$ovs.o) * 6 ) + as.numeric(B.bowling$ovs.b)

# bowling econ
B.bowling$Econ <- as.numeric(format(B.bowling$runs * 6 / B.bowling$BB, digits = 2))
B.bowling$Econ[B.bowling$BB == 0] <- NA

#bowling SR
B.bowling$SR <- as.numeric(format(B.bowling$wickets * 100 / B.bowling$BB, digits = 2))
B.bowling$SR[B.bowling$BB == 0] <- NA

#nice bowling analyis
B.bowling$nicean <- paste(B.bowling$wickets, B.bowling$runs, sep="-")

# proportion of extras
inningses$extras_proportion <- inningses$total_extras / inningses$runs
inningses$niceextrasprop <- paste(format(inningses$extras_proportion * 100, digits = 1),"%")

# decimal overs
inningses$ovs.o <- str_split_i(inningses$overs, "\\.", 1)
inningses$ovs.b <- str_split_i(inningses$overs, "\\.", 2)
inningses$ovs.b[is.na(inningses$ovs.b)] <- 0
inningses$ovs.bf[inningses$ovs.b >= 6] <- 5
inningses$ovs.bf <- as.numeric(inningses$ovs.b) / 6
inningses$decimal_overs <- as.numeric(inningses$ovs.o)  + as.numeric(inningses$ovs.bf)
 

inningses$actuallyDate <- as.Date(inningses$actuallyDate, origin="1970-01-01")
batting$actuallyDate <- as.Date(batting$actuallyDate, origin="1970-01-01")
B.bowling$actuallyDate <- as.Date(B.bowling$actuallyDate, origin="1970-01-01")

# Get temperatures
# raw <- read.csv("https://www.cl.cam.ac.uk/research/dtg/weather/weather-raw.csv", header = FALSE, col.names= c("date", "temp10", "humid", "dewp10", "press", "meanwind","winddir", "sun", "rain", "maxwind"))
# raw$jdate <- as.Date(raw$date)
# # temps are C * 10 (decicelcius)
# foo <- tapply(raw$temp10 , raw$jdate, max) # temp is the instantaneous temp in decicelcius, need daily max
# maxt <- data.frame(maxtemp=foo/10, date = as.Date(names(foo)))
# rm(raw); rm(foo)
# inningses <- left_join(inningses, maxt, by=c("actuallyDate"="date"))
# rm(maxt)
# ===============================
# AVERAGES - BATTING ALL TIME

#adam <- aggregate(runs ~ batsman_id, data = batting, sum)

adam2 <- aggregate(cbind(runs, runs>=50 & runs<100, runs>=100, !is.na(runs), (how_out=="not out" & !is.na(how_out))) ~ batsman_id, data=batting, sum)
adam2$Avg <- format(round(adam2$runs / (adam2$V4 - adam2$V5), 2), nsmall = 2)
#adam2[adam2$batsman_id=="4453764",]

#highest:
adam3 <-  aggregate(runs ~ batsman_id, data = batting, max)
adam4 <- aggregate(cbind(runs, balls) ~ batsman_id, data=batting[!is.na(batting$balls),], sum)
adam4$SR <- format(round(adam4$runs * 100 / adam4$balls, 2), nsmall = 2)

adam5 <- aggregate(cbind(how_out=="ct", how_out=="st", how_out=="run out") ~ fielder_id, data=batting, sum)
adam6 <- aggregate(cbind(fours, sixes) ~ batsman_id, data = batting, sum)

#adamX <- merge(adam, adam2, by.x = "batsman_id", by.y = "batsman_id", all.x=TRUE)
adamW <- merge(adam2, adam3, by.x = "batsman_id", by.y = "batsman_id", all.x=TRUE)
adamY <- merge(adamW, adam6, by.x = "batsman_id", by.y = "batsman_id", all.x=TRUE)
names(adamY) <- c("batsman_id", "Runs", "50", "100", "Inns", "NO", "Avg", "Max", "4s", "6s")
adamZ <- merge(adamY, adam4, by.x = "batsman_id", by.y = "batsman_id", all.x=TRUE)
names(adamZ) <- c("batsman_id", "Runs", "50", "100",  "Inns", "NO", "Avg", "Max", "4s", "6s", "RunsWBF", "BF","SR")
battingAverages <- merge(adamZ, adam5, by.x = "batsman_id", by.y = "fielder_id", all.x=TRUE)
rm(adam2); rm(adam3); rm(adam4); rm(adam5); rm(adamY); rm(adamZ)

# TODO implement auto-check that no rows have Runs != RunsY, throw a warning if it does

battingAverages <- left_join(battingAverages,D.people, by=c("batsman_id"="id"))
battingAverages <- battingAverages[battingAverages$batsman_id != "",]
#battingAverages <- subset(battingAverages, select = -c(RunsY,RunsWBF, batsman_id,club_id ))

B.battingAverages <- battingAverages[, c(17,       2,       5,    6,    7,     8,  13,      3, 4,         14,     15, 16, 18,          12, 9, 10)]
names(B.battingAverages) <-             c("Name", "Runs", "Inns", "NO", "Avg", "HS",  "SR",  "50", "100",  "Ct", "Std", "RO", "club_id", "BF", "4s", "6s")

rm(battingAverages)
# battingAverages <- battingAverages %>%
#   rename(Name = name,
#          Ground = ground,)

B.battingAverages$Ct[is.na(B.battingAverages$Ct)] <- 0
B.battingAverages$Std[is.na(B.battingAverages$Std)] <- 0
B.battingAverages$RO[is.na(B.battingAverages$RO)] <- 0

B.battingAverages$is_us <- FALSE
B.battingAverages$is_us[B.battingAverages$club_id==as.numeric(conf$club_of_interest)] <- TRUE

# ===============================================================
# AVERAGES -- BATTING THIS YEAR

battingTY <- batting[batting$year == conf$year_of_interest,]
#adam <- aggregate(runs ~ batsman_id, data = batting, sum)

adam2 <- aggregate(cbind(runs, runs>=50 & runs<100, runs>=100, !is.na(runs), (how_out=="not out" & !is.na(how_out))) ~ batsman_id, data=battingTY, sum)
adam2$Avg <- format(round(adam2$runs / (adam2$V4 - adam2$V5), 2), nsmall = 2)
#adam2[adam2$batsman_id=="4453764",]

#highest:
adam3 <-  aggregate(runs ~ batsman_id, data = battingTY, max)
adam4 <- aggregate(cbind(runs, balls) ~ batsman_id, data=battingTY[!is.na(battingTY$balls),], sum)
adam4$SR <- format(round(adam4$runs * 100 / adam4$balls, 2), nsmall = 2)

adam5 <- aggregate(cbind(how_out=="ct", how_out=="st", how_out=="run out") ~ fielder_id, data=battingTY, sum)
adam6 <- aggregate(cbind(fours, sixes) ~ batsman_id, data = battingTY, sum)

#adamX <- merge(adam, adam2, by.x = "batsman_id", by.y = "batsman_id", all.x=TRUE)
adamW <- merge(adam2, adam3, by.x = "batsman_id", by.y = "batsman_id", all.x=TRUE)
adamY <- merge(adamW, adam6, by.x = "batsman_id", by.y = "batsman_id", all.x=TRUE)
names(adamY) <- c("batsman_id", "Runs", "50", "100", "Inns", "NO", "Avg", "Max", "4s", "6s")
adamZ <- merge(adamY, adam4, by.x = "batsman_id", by.y = "batsman_id", all.x=TRUE)
names(adamZ) <- c("batsman_id", "Runs", "50", "100",  "Inns", "NO", "Avg", "Max", "4s", "6s", "RunsWBF", "BF","SR")
battingAveragesTY <- merge(adamZ, adam5, by.x = "batsman_id", by.y = "fielder_id", all.x=TRUE)
rm(adam2); rm(adam3); rm(adam4); rm(adam5); rm(adamY); rm(adamZ); rm(adam6); rm(adamW)

# TODO implement auto-check that no rows have Runs != RunsY, throw a warning if it does

rm(battingTY)

battingAveragesTY <- left_join(battingAveragesTY,D.people, by=c("batsman_id"="id"))
battingAveragesTY <- battingAveragesTY[battingAveragesTY$batsman_id != "",]
#battingAverages <- subset(battingAverages, select = -c(RunsY,RunsWBF, batsman_id,club_id ))

B.battingAveragesTY <- battingAveragesTY[, c(17,       2,       5,    6,    7,     8,  13,      3, 4,         14,     15, 16, 18,          12, 9, 10)]
names(B.battingAveragesTY) <-             c("Name", "Runs", "Inns", "NO", "Avg", "HS",  "SR",  "50", "100",  "Ct", "Std", "RO", "club_id", "BF", "4s", "6s")
#FIXME sort that column number hack
B.battingAveragesTY$Ct[is.na(B.battingAveragesTY$Ct)] <- 0
B.battingAveragesTY$Std[is.na(B.battingAveragesTY$Std)] <- 0
B.battingAveragesTY$RO[is.na(B.battingAveragesTY$RO)] <- 0

B.battingAveragesTY$is_us <- FALSE
B.battingAveragesTY$is_us[B.battingAveragesTY$club_id==as.numeric(conf$club_of_interest)] <- TRUE

rm(battingAveragesTY)
# ==================================================================
# ===============================================================
# AVERAGES -- BOWLING ALL TIME
#* averages - o, m, r, w, best, 5wi, average, econ, sr (year and alltime)

adam <- aggregate(cbind(maidens, runs, wickets, wickets>=5, BB) ~ bowler_id, data=B.bowling, sum)
#this appears to lose a few people, not looked into why FIXME
adam$Avg <- format(round(adam$runs / (adam$wickets), 2), nsmall = 2)
adam$Econ <- format(round(adam$runs * 6 / (adam$BB), 2), nsmall = 2)
adam$SR <- format(round(adam$wickets * 100 / (adam$BB), 2), nsmall = 2)
adam$overs <- paste(adam$BB %/% 6, adam$BB %% 6, sep=".")
#adam2[adam2$batsman_id=="4453764",]

adam2 <- B.bowling %>% 
         select(c(bowler_id, runs, wickets, nicean)) %>%
         filter(!is.na(bowler_id)) %>% 
         group_by(bowler_id) %>% 
         slice_max(wickets) %>% 
         slice_min(runs, n=1, with_ties = F) %>%
          #rename(bestw = wickets, bestr = runs)
         select(c(bowler_id, nicean))

B.bowlingAverages <- tibble(left_join(adam,adam2, by="bowler_id"))
B.bowlingAverages <- left_join(B.bowlingAverages,D.people, by=c("bowler_id"="id"))

B.bowlingAverages <- B.bowlingAverages[,c("bowler_id", "overs", "maidens", "runs", "wickets", "BB",
                                          "V4", "Avg", "Econ", "SR", "nicean", "name", "club_id")]
B.bowlingAverages <- B.bowlingAverages %>%
  rename(O = overs,
         M = maidens,
         R = runs,
         W = wickets,
         `5wi` = V4,
         `Best` = nicean,
         Name = name)
         
rm(adam); rm(adam2)
B.bowlingAverages$is_us <- FALSE
B.bowlingAverages$is_us[B.bowlingAverages$club_id==as.numeric(conf$club_of_interest)] <- TRUE

# ===============================================================
# AVERAGES -- BOWLING THIS YEAR
#* averages - o, m, r, w, best, 5wi, average, econ, sr (year and alltime)
B.bowlingTY <- B.bowling[B.bowling$year == conf$year_of_interest,]

adam <- aggregate(cbind(maidens, runs, wickets, wickets>=5, BB) ~ bowler_id, data=B.bowlingTY, sum)
#this appears to lose a few people, not looked into why FIXME
adam$Avg <- format(round(adam$runs / (adam$wickets), 2), nsmall = 2)
adam$Econ <- format(round(adam$runs * 6 / (adam$BB), 2), nsmall = 2)
adam$SR <- format(round(adam$wickets * 100 / (adam$BB), 2), nsmall = 2)
adam$overs <- paste(adam$BB %/% 6, adam$BB %% 6, sep=".")
#adam2[adam2$batsman_id=="4453764",]

adam2 <- B.bowlingTY %>% 
  select(c(bowler_id, runs, wickets, nicean)) %>%
  filter(!is.na(bowler_id)) %>% 
  group_by(bowler_id) %>% 
  slice_max(wickets) %>% 
  slice_min(runs, n=1, with_ties = F) %>%
  select(c(bowler_id, nicean))

B.bowlingAveragesTY <- tibble(left_join(adam,adam2, by="bowler_id"))
B.bowlingAveragesTY <- left_join(B.bowlingAveragesTY,D.people, by=c("bowler_id"="id"))

B.bowlingAveragesTY <- B.bowlingAveragesTY[,c("bowler_id", "overs", "maidens", "runs", "wickets", "BB",
                                          "V4", "Avg", "Econ", "SR", "nicean", "name", "club_id")]
B.bowlingAveragesTY <- B.bowlingAveragesTY %>%
  rename(O = overs,
         M = maidens,
         R = runs,
         W = wickets,
         `5wi` = V4,
         `Best` = nicean,
         Name = name)

rm(adam); rm(adam2); rm(B.bowlingTY)

B.bowlingAveragesTY$is_us <- FALSE
B.bowlingAveragesTY$is_us[B.bowlingAveragesTY$club_id==as.numeric(conf$club_of_interest)] <- TRUE

# ==================================================================
# look at some match-level parameters


a.matchaggregate <- aggregate(runs ~ match_id, data = inningses, sum)
B.matches <- left_join(B.matches, a.matchaggregate, by=c("match_id"="match_id"))
rm(a.matchaggregate)

a.matchaggregate <- aggregate(decimal_overs ~ match_id, data = inningses, sum, na.action=na.pass)
B.matches <- left_join(B.matches, a.matchaggregate, by=c("match_id"="match_id"))
rm(a.matchaggregate)

B.matches$decimal_overs_nice <- paste(floor(B.matches$decimal_overs), round((B.matches$decimal_overs %% 1 ) * 6), sep=".")

B.matches <- B.matches %>%
  rename(Aggregate = runs,
         `Overs in match` = decimal_overs_nice,
         Date = match_date,
         Home = home_club_name,
         Away = away_club_name,
         Ground = ground_name)
         

# ==================================================================
# ZONES OF INTEREST
# There are three 'zones' -- 
#  the club of interest doing the thing of interest, eg batting in a batting dataset; -- `us'
# matches involving the club of interest; -- `circle'
# and matches in one of the competitions of interest. -- `sphere'
# note that not all circle are in sphere, as matches of the coi with null competition are not in sphere

batting$is_us <-FALSE
batting$is_circle <- FALSE
batting$is_smallsphere <- FALSE
batting$is_sphere <- FALSE

batting$is_us[batting$club_id_bat==as.numeric(conf$club_of_interest)] <- TRUE
batting$is_circle[batting$club_id_bat==as.numeric(conf$club_of_interest) | batting$club_id_field==as.numeric(conf$club_of_interest)] <- TRUE
batting$is_smallsphere[batting$competition_id %in% conf$competitions_of_interest] <- TRUE
batting$is_sphere[batting$is_circle == TRUE | batting$is_smallsphere == TRUE] <- TRUE

inningses$is_us <-FALSE
inningses$is_circle <- FALSE
inningses$is_smallsphere <- FALSE
inningses$is_sphere <- FALSE

inningses$is_us[inningses$club_id_bat==as.numeric(conf$club_of_interest)] <- TRUE
inningses$is_circle[inningses$club_id_bat==as.numeric(conf$club_of_interest) | inningses$club_id_field==as.numeric(conf$club_of_interest)] <- TRUE
inningses$is_smallsphere[inningses$competition_id %in% conf$competitions_of_interest] <- TRUE
inningses$is_sphere[inningses$is_circle == TRUE | inningses$is_smallsphere == TRUE] <- TRUE

B.matches$is_circle <- FALSE
B.matches$is_smallshpere <- FALSE
B.matches$is_sphere <- FALSE

B.matches$is_circle[B.matches$home_club_id == as.numeric(conf$club_of_interest) | B.matches$away_club_id == as.numeric(conf$club_of_interest)] <- TRUE
B.matches$is_smallsphere[B.matches$competition_id %in% conf$competitions_of_interest] <- TRUE
B.matches$is_sphere[B.matches$is_circle == TRUE | B.matches$is_smallsphere == TRUE] <- TRUE

B.bowling$is_us <-FALSE
B.bowling$is_circle <- FALSE
B.bowling$is_smallsphere <- FALSE
B.bowling$is_sphere <- FALSE

B.bowling$is_us[B.bowling$club_id_field==as.numeric(conf$club_of_interest)] <- TRUE
B.bowling$is_circle[B.bowling$club_id_bat==as.numeric(conf$club_of_interest) | B.bowling$club_id_field==as.numeric(conf$club_of_interest)] <- TRUE
B.bowling$is_smallsphere[B.bowling$competition_id %in% conf$competitions_of_interest] <- TRUE
B.bowling$is_sphere[B.bowling$is_circle == TRUE | B.bowling$is_smallsphere == TRUE] <- TRUE

# ==================================================================
# give columns nice names
B.inningses <- inningses %>%
  rename(Team = teamName,
         Ground = ground,
         Result = result_des,
         Byes = extra_byes,
         "Leg byes" = extra_leg_byes,
         "Wides" = extra_wides,
         "No-balls" = extra_no_balls,
         "Pen(B)" = extra_penalty_runs,
         "Pen(F)" = penalties_runs_awarded_in_other_innings,
         Extras = total_extras,
         Runs = runs,
         W = wickets,
         Ovs = overs,
         Score = score,
         Ven = home_or_away,
         Res = bats_result,
         Oppos = club_name_field,
         Date = niceDate)

rm(inningses)

B.batting <- batting %>%
  rename(Team = team_name_bat,
         Ground = ground,
         Result = result_des,
         Pos = position,
         Name = batsman_name,
         "How Out" = how_out,
         Runs = runs,
         "4" = fours,
         "6" = sixes,
         Balls = balls,
         Ven = home_or_away,
         Res = bats_result,
         Oppos = club_name_field,
         Club = club_name_bat,
         Date = niceDate,
         Contrib = nicecontrib
         )
rm(batting)

B.bowling <- B.bowling %>%
  rename(Team = team_name_field,
         Ground = ground,
         Result = result_des,
         Name = bowler_name,
         O = overs,
         M = maidens,
         R = runs,
         W = wickets,
         Wd = wides,
         NB = no_balls,
         Ven = home_or_away,
         Res = bowls_result,
         Oppos = club_name_bat,
         Club = club_name_field,
         Date = niceDate,
  )

# =================================================
# filter out for the club of interest
F.batting.us <- filter(B.batting, is_us==TRUE)
F.batting.circle <- filter(B.batting,is_circle==TRUE) 
F.batting.sphere <- filter(B.batting,is_sphere==TRUE) 

F.bowling.us <- filter(B.bowling, is_us==TRUE)
F.bowling.circle <- filter(B.bowling,is_circle==TRUE) 
F.bowling.sphere <- filter(B.bowling,is_sphere==TRUE) 

F.inningses.us <- filter(B.inningses,is_us==TRUE) 
F.inningses.circle <- filter(B.inningses,is_circle==TRUE) 
F.inningses.sphere <- filter(B.inningses,is_sphere==TRUE) 

# ==================================================================
# dump out for use by other scripts
save(B.matches, file="./data/matches")
save(B.inningses, file="./data/innings")
save(B.batting, file="./data/batting")
save(B.battingAverages, file="./data/battingAvg")
save(B.battingAveragesTY, file="./data/battingAvgTY")
save(B.bowling, file="./data/bowling")
save(B.bowlingAverages, file="./data/bowlingAvg")
save(B.bowlingAveragesTY, file="./data/bowlingAvgTY")
save(D.club, file="./data/club")
