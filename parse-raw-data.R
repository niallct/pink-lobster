# ==== initialise ====
library(jsonlite)
library(purrr)
library(dplyr)
library(stringr)
library(yaml)
library(tidyr)

## load the config
conf <- yaml.load_file("./config/config.yaml")
conf$last_year <- as.character(as.integer(conf$year_of_interest) - 1)

# a function to round averages
avRn <- function(x) trunc(x*100, signif=2 )/100

# ==== current clubs ====
# CURRENT > CLUBS
# this is a load of dirs, named by club id, each containing a list of players

# Parent directory containing subdirectories
parent_directory <- "newdata/current/clubs"

# Get list of subdirectories
subdirectories <- list.dirs(parent_directory, full.names = TRUE)

# Function to load and process a JSON file from a directory
load_json_file <- function(directory) {
  json_files <- list.files(directory, pattern = "*.json", full.names = TRUE)
  
  if (length(json_files) == 0) {
    return(NULL)  # No JSON files in the directory
  }
  
  json_data <- fromJSON(json_files[1])  # Load the first JSON file
  # Perform any necessary transformations or data processing on the json_data object
  return(json_data)
}

# Load and process JSON files from each directory, along with directory names
json_data <- map2(subdirectories, basename(subdirectories), ~ {
  data <- load_json_file(.x)
  if (!is.null(data)) {
    data$club_id <- .y  # Add directory name as a column
  }
  return(data)
})

# Filter out NULL values from the list
filtered_json_data <- json_data %>%
  compact()  # Removes NULL elements from the list
rm(json_data)

# Combine data frames into a single tibble
E.players <- bind_rows(filtered_json_data)
E.players <- rename(E.players, player_id=member_id, Name = name)

# make a lookup just on player ID ie no club id, which means duplicates
E.playersNC <- E.players %>%
  select(player_id, Name) %>% 
  distinct(player_id, Name)

#cleanup
rm(parent_directory)
rm(subdirectories)
rm(load_json_file)
rm(filtered_json_data)


# ==== the match detail loading function  ===============================================

# define what we want as match headers -- drop umpire names and such
matchMainHdrs <- c("match_id", "match_date", "ground_name", "ground_id", "competition_id","league_id",
                   "competition_type", "match_type", "no_of_overs", "batted_first",
                   "home_team_name", "home_team_id", "home_club_name", "home_club_id",
                   "away_team_name", "away_team_id", "away_club_name", "away_club_id", 
                   "toss", "result_description", "result_applied_to", "toss_won_by_team_id"
                   #,"innings"
)

matchCompoHdrs <- c("match_id", "match_date", "competition_id","league_id", "league_name",
                    "competition_name", "competition_type", "match_type", "no_of_overs")

# Function to load and process a JSON file. Takes type m match, i inns, c competition, p player
load_json_file <- function(file, type) {
  json_data <- fromJSON(file)
  jds <- json_data[matchMainHdrs]
  if (is.null(jds$match_date)) {return(NULL)}
  
  #debugging things
  #print(as.data.frame(json_data_sel["innings"]))
  #print(json_data$id)
  #browser()
  
  # the competition function - making a competition/league lookup, but need to first
  # check those columns exist because friendlies etc don't have those keys
  if (type=="c") { 
    if ("league_name" %in% names(json_data) ){
      jdc <- json_data[matchCompoHdrs]
      return(jdc)} else {return(NULL)}}
  
  # deal with overs - calculate into decimal for later addition
  foo <- data.frame(rep(0, length(json_data$innings$overs)))
  foo$ovs.o <- str_split_i(json_data$innings$overs, "\\.", 1)
  foo$ovs.b <- str_split_i(json_data$innings$overs, "\\.", 2)
  foo$ovs.b[is.na(foo$ovs.b)] <- 0
  foo$ovs.bf <- as.numeric(foo$ovs.b) / 6
  foo$ovs.bf[foo$ovs.b >= 6] <- 5
  json_data$innings$decimal_overs <- as.numeric(foo$ovs.o) + as.numeric(foo$ovs.bf)
  
  if(!is.null(nrow(json_data$innings))){
    jds$firstinnsdecovers <- json_data$innings[1,]$decimal_overs
    jds$secondinnsdecovers <- json_data$innings[2,]$decimal_overs} 
  else {jds$firstinnsdecovers <- NA
  jds$secondinnsdecovers <- NA
  }
  
  # for 2-inns matches only, hoick out an inns-summary to the top level
  if(!is.null(nrow(json_data$innings)) &&  nrow(json_data$innings) == 2){ # fiddle?
    jds$firstinnsbattid <- json_data$innings[1,]$team_batting_id
    jds$firstinnsbattname <- as.character(json_data$innings[1,]$team_batting_name)
    jds$firstinnsscore <- paste( json_data$innings[1,]$runs, "/", json_data$innings[1,]$wickets, sep="" )
    jds$firstinnsniceovs <- paste( "(", json_data$innings[1,]$overs, ")", sep="" )
    jds$secondinnsbattid <- json_data$innings[2,]$team_batting_id
    jds$secondinnsbattname <- as.character(json_data$innings[2,]$team_batting_name)
    jds$secondinnsscore <- paste( json_data$innings[2,]$runs, "/", json_data$innings[2,]$wickets, sep="" )
    jds$secondinnsniceovs <- paste( "(", json_data$innings[2,]$overs, ")", sep="" )
    jds$`Match Summary` <- paste(jds$firstinnsbattname, jds$firstinnsscore, jds$firstinnsniceovs, ";",
                                 jds$secondinnsbattname, jds$secondinnsscore, jds$secondinnsniceovs)
  } else {
    jds$firstinnsbattid <- NA
    jds$firstinnsbattname <- ""
    jds$firstinnsscore <- ""
    jds$firstinnsniceovs <- ""
    jds$secondinnsbattid <- NA
    jds$secondinnsbattname <- ""
    jds$secondinnsscore <- ""
    jds$firstinnsniceovs <-""
    jds$`Match Summary` <- ""
  }
  
  
  # if (type=="i") jds[["inns"]] <- as.data.frame(json_data["innings"])
  if (type=="i") jds$inns <- as.data.frame(json_data[["innings"]], optional = FALSE)
  if (type=="p") {
    if(!is.null(nrow(json_data$players)) &&  nrow(json_data$players) == 2){ 
      
      fooH <- fooA <- data.frame(position=integer(),
                                 player_name=character(), 
                                 player_id=numeric(), 
                                 captain=logical(),
                                 wicket_keeper=logical(),
                                 team=factor()) 
      
      if( !is.null(nrow(json_data[["players"]][["home_team"]][[1]])) ){
        fooH <- json_data[["players"]][["home_team"]][[1]]
        fooH$team <- "home"
      } 
      if( !is.null(nrow(json_data[["players"]][["away_team"]][[2]])) ){
        fooA <- json_data[["players"]][["away_team"]][[2]]
        fooA$team <- "away"
      } 
      foo <- rbind(fooH, fooA)
      
      jds$players <- as.data.frame(foo, optional = FALSE)
      #jds$players <- as.data.frame(json_data[["players"]], optional = FALSE)
    }
    else jds$players <- NA
    
    #browser()
  }
  #print(json_data_sel)
  
  jds$`Match Aggregate` <- sum(json_data$innings$runs)
  zxc <- sum(json_data$innings$decimal_overs)
  jds$matchAggDecOvs <- zxc
  jds$`Match Overs` <- paste(floor(zxc), round((zxc %% 1 ) * 6), sep=".")
  
  jds$actuallyDate <- as.Date(jds$match_date, "%Y-%m-%dT%H:%M:%S")
  jds$Date <- format(jds$actuallyDate, format = "%d %b %Y")
  jds$Yr <- format(jds$actuallyDate, format = "%Y")
  
  #jds$is_us <-FALSE
  jds$is_circle <- FALSE
  jds$is_smallsphere <- FALSE
  jds$is_sphere <- FALSE
  jds$is_ourleagues <- FALSE
  
  jds$is_circle[jds$home_club_id == as.numeric(conf$club_of_interest) | 
                  jds$away_club_id == as.numeric(conf$club_of_interest)] <- TRUE
  jds$is_smallsphere[jds$competition_id %in% conf$competitions_of_interest] <- TRUE
  jds$is_ourleagues[jds$league_id %in% conf$leagues_of_interest] <- TRUE 
  jds$is_sphere[jds$is_circle == TRUE | jds$is_smallsphere == TRUE] <- TRUE
  
  # rename these now, rather than with dplyr later, so that they show in both the innings
  # and match level outputs
  jds[["Ground"]] <- jds[["ground_name"]]
  jds[["Home Club"]] <- jds[["home_club_name"]]
  jds[["Away Club"]] <- jds[["away_club_name"]]
  jds[["Home Side"]] <- paste(jds[["home_club_name"]], jds[["home_team_name"]], sep=" - ")
  jds[["Away Side"]] <- paste(jds[["away_club_name"]], jds[["away_team_name"]], sep=" - ")
  jds[["Result_"]] <- jds[["result_description"]]
  jds[["Type"]] <- as.factor(jds[["competition_type"]])
  #jds[["Winner"]] <- jds[["result_applied_to"]]
  
  jds <- jds[-which(names(jds) %in% c("ground_name", "home_club_name", "away_club_name", 
                                      "result_description", "competition_type"))]
  
  # interpolate missing no_of_overs data -- this seems to be friendlies
  #  browser()
  if (is.null(jds$no_of_overs)) {
    jds$no_of_overs = case_when(
      jds$matchAggDecOvs == 0 ~ 0, # of 6694, about 100 have no overs recorded but runs; 80 are conceded etc
      jds$firstinnsdecovers > 60 ~ NA, # to clear any obvious errors
      jds$secondinnsdecovers > 60 ~ NA, # to clear any obvious errors
      jds$firstinnsdecovers > 0  | jds$secondinnsdecovers > 0
      ~ max(ceiling(0.2 * jds$firstinnsdecovers) / 0.2, 
            ceiling(0.2 * jds$secondinnsdecovers) / 0.2, 
            na.rm = TRUE), # round up to nearest 5
      TRUE ~ NA
    )
  }
  
  # browser()
  #a result abbreviation
  if (!is.null(jds$Result_) ){ #& !is.null(jds$result_applied_to))
    jds$result_match <- case_when(
      jds[["Result_"]]  == "Abandoned" ~ "A",
      jds[["Result_"]]  == "Tied" ~ "T",
      jds[["Result_"]]  == "Cancelled" ~ "C",
      jds[["Result_"]]  %in% c("Draw", "Drawn", "Trophy Shared") ~ "D",
      jds[["Result_"]]  == "Match In Progress" ~ "?",
      TRUE ~ "")
  }  else {jds$result_match <- ""}

  if (!is.null(jds$Result_) & !is.null(jds$result_applied_to) & jds$result_match == ""){
    jds$result_match <- case_when(
      is.null(jds[["result_applied_to"]]) ~ "?",
      jds[["result_applied_to"]] == jds[["home_team_id"]] ~ "HW",
      jds[["result_applied_to"]] == jds[["away_team_id"]] ~ "AW",
    )

  }
  ##else {jds$result_match <- ""}
  
  # try to work out the winning margin - do this now so it appears in match and inns
  # and downward outputs. But this duplicates stuff from later, a bit.
  # first we need to remove anything without a winner, and anything we can't pick
  # up each inns score eg two-innings matches and concessions
  # relies on draws and ties being identified elsewhere
  # also relies on 10 wickets to fall in each inns
  
  if (!is.null(jds[["result_applied_to"]])){
    if(jds$firstinnsscore == "" | jds$secondinnsscore == "") {
      jds$`Full Result` <- "-"
      jds$Margin <- NA
      jds$BatInnsWinner <- NA
    } 
    else {
      jds$Margin <- case_when(
        json_data$innings[1,]$runs > json_data$innings[2,]$runs ~ json_data$innings[1,]$runs - json_data$innings[2,]$runs,
        json_data$innings[1,]$runs < json_data$innings[2,]$runs ~ 10 - json_data$innings[2,]$wickets,
        TRUE ~ NA
      )
      
      jds$`Full Result` <-
        case_when(
          jds$Margin == 0 ~ "-",
          is.na(jds$Margin) ~ "-",
          json_data$innings[1,]$runs > json_data$innings[2,]$runs ~ paste(json_data$innings[1,]$team_batting_name,
                                                                          "win by",
                                                                          json_data$innings[1,]$runs - json_data$innings[2,]$runs,
                                                                          "runs"),
          json_data$innings[1,]$runs < json_data$innings[2,]$runs ~ paste(json_data$innings[2,]$team_batting_name,
                                                                          "win by",
                                                                          10 - json_data$innings[2,]$wickets,
                                                                          "wickets"),
          TRUE ~ "-"
        )
      
      if (!is.na(jds$Margin) & jds$Margin == 1) { jds$`Full Result` <- str_sub(jds$`Full Result`, end = -2)}
      
      jds$BatInnsWinner <- case_when(
        jds$Margin == 0 ~ NA,
        json_data$innings[1,]$runs > json_data$innings[2,]$runs ~ 1,
        json_data$innings[1,]$runs < json_data$innings[2,]$runs ~ 2,
        TRUE ~ NA
      )
    }
  } else {jds$`Full Result` <- "-"
  jds$Margin <- NA
  jds$BatInnsWinner <- NA}
  
  if (jds$`Full Result` == "-"){jds$Result = jds$Result_} 
  else {jds$Result = jds$`Full Result`}
  
  
  return(jds)
}

# ==== current year data load  ====
# == match details... CURRENT > SEASONS > YYYY > MATCH_DETAILS
# this is one file per match

# Directory containing JSON files
directory <- "./newdata/current/seasons/2023/match_details"

# Get list of JSON files in the directory
json_files <- list.files(directory, pattern = "*.json", full.names = TRUE)


# apply that on the raw data in match and innings mode
json_data <- map(json_files, load_json_file, "m")
E.matches <- json_data %>% bind_rows()


json_data2 <- map(json_files, load_json_file, "i")
#print(json_data2)
# add in the row number to innings records
for (i in 1:length(json_data2)){
  json_data2[[i]]$inns$`Inns of match` <- as.factor(rownames(json_data2[[i]]$inns))
}
rm(i)

E.inningses <- json_data2 %>% bind_rows() %>% unnest(inns)

# unstack player data
json_data3 <- map(json_files, load_json_file, "p")
E.matchplayers <- json_data3 %>% bind_rows() %>% unnest(players)


# unstack competiton data 
json_data4 <- map(json_files, load_json_file, "c")
E.matchcompos <- json_data4 %>% bind_rows() 


# cleanup
rm(directory)
rm(json_files)
#rm(load_json_file)
rm(json_data)
rm(json_data2)
rm(json_data3)
rm(json_data4)


# ==== previous seasons match details =====
# previous > previous-seasons > seasons
#this is a load of dirs, named by year each containing a directory called match_details, which containss the jsons

# Parent directory containing subdirectories
parent_directory <- "newdata/previous/previous-seasons/seasons"

# Get list of subdirectories
subdirectories <- list.dirs(parent_directory, full.names = TRUE)

# Use the matching indices to extract the matching directory names
matching_directories <- subdirectories[grep("match_details", subdirectories)]

# Initialize an empty vector to store the file paths
json_files <- c()

# Loop through each directory in matching_directories
for (dir in matching_directories) {
  # Get a list of .json files in the current directory
  json_files <- c(json_files, list.files(dir, pattern = "*.json", full.names = TRUE))
}

rm(dir)

# apply the loading function on the big load of old files
json_data <- map(json_files, load_json_file, "m")
G.matches <- json_data %>% bind_rows()


json_data2 <- map(json_files, load_json_file, "i")
#print(json_data2)
# add in the row number to innings records
for (i in 1:length(json_data2)){
  if(!is.null(json_data2[[i]])) {json_data2[[i]]$inns$`Inns of match` <- as.numeric(rownames(json_data2[[i]]$inns))}
}
rm(i)

G.inningses <- json_data2 %>% bind_rows() %>% unnest(inns) 

# unstack player data
json_data3 <- map(json_files, load_json_file, "p")
G.matchplayers <- json_data3 %>% bind_rows() %>% unnest(players)

# unstack competiton data 
json_data4 <- map(json_files, load_json_file, "c")
G.matchcompos <- json_data4 %>% bind_rows() 



#cleanup
rm(parent_directory)
rm(subdirectories)
rm(matching_directories)
#rm(directory)
rm(json_files)
rm(load_json_file)
rm(json_data)
rm(json_data2)
rm(json_data3)
rm(json_data4)

# ==== merge old and new years ==== 
# this used to be done later, after the prettifying and merging
C.matches <- rbind(E.matches, G.matches)
C.inningses <- rbind(E.inningses, G.inningses)
B.matchplayers <- rbind(E.matchplayers, G.matchplayers)
B.matchcompos <- rbind(E.matchcompos, G.matchcompos)

# now filter out uninteresting things, but keep the old set
B.matches <- C.matches %>% 
  filter(!home_team_id %in% conf$excluded_teams, 
         !away_team_id %in% conf$excluded_teams, 
         !league_id %in% conf$excluded_leagues,
         !match_id %in% conf$excluded_matches,
         !Result %in% c("Cancelled", "Match In Progress"))

B.inningses <- C.inningses %>% 
  filter(!home_team_id %in% conf$excluded_teams, 
         !away_team_id %in% conf$excluded_teams, 
         !league_id %in% conf$excluded_leagues,
         !match_id %in% conf$excluded_matches,
         !Result %in% c("Cancelled", "Match In Progress"))

# ==== add club into the player match list ====

B.matchplayers <- B.matchplayers %>%
  mutate(    `Playing For` = case_when(
    team == "home" ~ `Home Club`,
    team == "away" ~ `Away Club`,
    TRUE ~ NA)) %>%
  rename(
    Position = position,
    "Player Name" = player_name,
    "Capt" = captain,
    "W-K" = wicket_keeper)

# ==== make some new and prettier bits of innings-level data ======



B.inningses <- B.inningses %>%
  rename(
    "Batting Side" = team_batting_name,
    batting_team_id = team_batting_id,
    Byes = extra_byes,
    "Leg byes" = extra_leg_byes,
    "Wides" = extra_wides,
    "No-balls" = extra_no_balls,
    "Pen(B)" = extra_penalty_runs,
    "Pen(F)" = penalties_runs_awarded_in_other_innings,
    Extras = total_extras,
    Total = runs,
    W = wickets,
    Ovs = overs,
    inns_balls = balls
  )

B.inningses <- B.inningses %>%
  mutate(
    `Team` = case_when(
      batting_team_id == home_team_id ~ home_team_name,
      batting_team_id == away_team_id ~ away_team_name,
      TRUE ~ NA
    ),
    `Batting Club` = case_when(
      batting_team_id == home_team_id ~ `Home Club`,
      batting_team_id == away_team_id ~ `Away Club`,
      TRUE ~ NA
    ),
    batting_club_id = case_when(
      batting_team_id == home_team_id ~ home_club_id,
      batting_team_id == away_team_id ~ away_club_id,
      TRUE ~ NA
    ),
    fielding_team_id = case_when(
      batting_team_id == home_team_id ~ away_team_id,
      batting_team_id == away_team_id ~ home_team_id,
      TRUE ~ NA
    ),
    `Fielding Team` = case_when(
      batting_team_id == home_team_id ~ away_team_name,
      batting_team_id == away_team_id ~ home_team_name,
      TRUE ~ NA
    ),
    `Fielding Club` = case_when(
      batting_team_id == home_team_id ~ `Away Club`,
      batting_team_id == away_team_id ~ `Home Club`,
      TRUE ~ NA
    ),
    fielding_club_id = case_when(
      batting_team_id == home_team_id ~ away_club_id,
      batting_team_id == away_team_id ~ home_club_id,
      TRUE ~ NA
    ),
    `Fielding Side` = case_when(
      batting_team_id == home_team_id ~ `Away Side`,
      batting_team_id == away_team_id ~ `Home Side`,
      TRUE ~ NA
    ),
    bat_first = case_when( #TODO do this better, somehow
      `Inns of match` == 1 ~ TRUE,
      TRUE ~ FALSE
    ),
    `Opposition Score` = case_when(
      batting_team_id == firstinnsbattid ~ secondinnsscore,
      batting_team_id == secondinnsbattid ~ firstinnsscore
    ))

B.inningses <- mutate(B.inningses, 
                      `Runs/Wkt` = Total / W,
                      Score = paste(Total, W, sep="/"),
                      extras_proportion = Extras / Total,
                      `% Extras` = paste(format(Extras * 100 / Total, digits = 1),"%"),
                      Ven = as.factor(case_when(
                        home_team_id == batting_team_id ~ "H",
                        away_team_id == batting_team_id ~ "A",
                        TRUE ~ as.character(NA)
                      )),
                      # competition_type = as.factor(competition_type),
                      match_type = as.factor(match_type),
                      Venue = as.factor(case_when(
                        ground_id %in% names(conf$home_ground_names) ~ "Home",
                        TRUE ~ "Other"
                      )),
                      Res = as.factor(case_when(
                        Result == "Abandoned" ~ "A",
                        Result == "Tied" ~ "T",
                        Result == "Cancelled" ~ "C",
                        Result %in% c("Draw", "Drawn", "Trophy Shared") ~ "D",
                        Result == "Match In Progress" ~ "?",
                        result_applied_to == "" & is.na(Result) ~ "?",
                        result_applied_to == batting_team_id ~ "W",
                        result_applied_to == fielding_team_id ~ "L",
                        TRUE ~ as.character(NA)
                      )),
                      us_batting = case_when(
                        batting_club_id == conf$club_of_interest ~ TRUE,
                        batting_club_id != conf$club_of_interest ~ FALSE
                      ),
                      us_fielding = case_when(
                        fielding_club_id == conf$club_of_interest ~ TRUE,
                        fielding_club_id != conf$club_of_interest ~ FALSE
                      )
                      # our_role = as.factor(case_when(
                      #   team_batting_id == conf$club_of_interest ~ "bat",
                      #   team_fielding_id == conf$club_of_interest ~ "field",
                      #   TRUE ~ "not"
                      #  )),
)
# ---- batting --------
# unstack the individual batting performance data

B.batting <- B.inningses %>%
  select(-c(bowl,fow)) %>%
  rowwise() %>% 
  mutate(bat = list(as.data.frame(bat)) )%>% unnest(bat)

B.batting <- B.batting %>%
  rename(Pos = position,
         Name = batsman_name,
         "How Out" = how_out,
         Runs = runs,
         "4" = fours,
         "6" = sixes,
         Balls = balls,
  )

B.batting <- B.batting %>%
  mutate(
    Brys = as.integer(`4`) + as.integer(`6`),
    SR = ifelse(Balls == 0 & Runs != 0, NA, format(round(Runs * 100 / Balls, 2), nsmall = 2)),
    contribpc = ifelse(Total == 0, NA, Runs / Total),
    Contrib = ifelse(Total == 0, NA, paste(format((Runs * 100)/Total , digits = 1),"%")),
    RunsWBF =  ifelse(is.na(Balls), NA, Runs)
  )

# interesting things in batting, count them

B.batting <- B.batting %>% select(match_id, `Inns of match`, Runs) %>%
  group_by(match_id, `Inns of match`) %>% 
  summarise(topscore = ifelse(sum(!is.na(Runs))==0, 0, max(Runs, na.rm = TRUE)),
            botscore = ifelse(sum(!is.na(Runs))==0, 0, min(Runs, na.rm = TRUE)),
            numbats = sum(!is.na(Runs)) ) %>% ungroup() %>% right_join(B.batting)

B.inningses <- B.batting %>% select(match_id, `Inns of match`, Runs) %>%
  group_by(match_id, `Inns of match`) %>% 
  summarise(topscore = ifelse(sum(!is.na(Runs))==0, 0, max(Runs, na.rm = TRUE)),
            botscore = ifelse(sum(!is.na(Runs))==0, 0, min(Runs, na.rm = TRUE)),
            numbats = sum(!is.na(Runs)),
            numcenturies = ifelse(sum(!is.na(Runs))==0, 0, sum(Runs>=100, na.rm = TRUE)),
            numfifties = ifelse(sum(!is.na(Runs))==0, 0, sum(Runs>=50, na.rm = TRUE)))%>% 
  ungroup() %>% right_join(B.inningses)


# ---- bowling ------ 
# unstack the individual bowling performance data

# B.bowling <- B.inningses %>%
#   select  (-c(bat, fow)) %>% 
#   rowwise() %>% 
#   print(str(B.inningses$bowl)) 
#   mutate(bowl = list(as.data.frame(bowl) %>%
#                        mutate(overs = as.numeric(overs)) # this can't find `overs`
#                      )) %>%
#          unnest(bowl)
# 
#   B.bowling <- B.inningses %>%
#     select(-c(bat, fow)) %>% 
#     rowwise() %>% 
#     mutate(bowl = map(bowl, ~mutate(.x, overs = as.numeric(overs)))) %>%
#     unnest(bowl)

# this is the one that works but we don't know how
# if it breaks, good luck!
B.bowling <- B.inningses %>%
  select(-c(bat, fow)) %>%
  mutate(
    bowl = map(bowl, ~ {
      df <- as.data.frame(.x)  # Convert to data frame if it's a list
      df$overs <- as.numeric(as.character(df$overs))  # Convert overs to numeric
      df
    })
  ) %>%
  unnest(bowl)

B.bowling <- B.bowling %>%
  rename(
    innsWkts = W,
    Name = bowler_name,
    O = overs,
    M = maidens,
    R = runs,
    W = wickets,
    Wd = wides,
    NB = no_balls) %>% 
  mutate(
    Res = as.factor(case_when(
      Res %in% c("A", "T", "C", "D", "?") ~ Res,
      Res == "W" ~ "L",
      Res == "L" ~ "W",
      TRUE ~ NA
    )),
    Analy = paste(W, R, sep="-"),
    Avg = ifelse(W == 0, NA, avRn(R / W)),
    ovs.o = as.numeric(ifelse(is.na(str_split_i(O, "\\.", 1)),0,str_split_i(O, "\\.", 1))),
    ovs.b = as.numeric(ifelse(is.na(str_split_i(O, "\\.", 2)),0,str_split_i(O, "\\.", 2))),
    BB = ifelse(is.na(O), NA, (ovs.o * 6 ) + ovs.b + Wd + NB),
  #  SRx = ifelse(is.na(BB), NA, ifelse( BB == 0, NA, avRn(W * 100 / BB) ) ),
  #  SR = ifelse(is.na(SRx), NA, as.numeric(SRx)),
    SRx = ifelse(is.na(BB), NA, ifelse( BB == 0, NA, avRn(BB / W) ) ),
    SR = ifelse(is.na(SRx), NA, as.numeric(SRx)),
    Econ = ifelse((ovs.o + ovs.b/6) ==0, NA, as.numeric( avRn(R  / (ovs.o + ovs.b/6)))  ),
    ExtPc = ifelse(is.na(R), NA, avRn(100 * as.numeric( (Wd + NB) / R))),
    MdnPc = ifelse(is.na(O), NA, avRn(100 * as.numeric( M / O )))
  ) %>% select(-SRx)


# interesting things in bowling, count them
B.inningses <- B.bowling %>% select(match_id, `Inns of match`, W, R) %>%
  group_by(match_id, `Inns of match`) %>% 
  summarise(minwkts = ifelse(sum(!is.na(W)) < 2, 0, min(W, na.rm = TRUE)), # if one bowler, is dubious
            numfourfs = ifelse(sum(!is.na(W))==0, 0, sum(W>=4, na.rm = TRUE)),
            numfivefs = ifelse(sum(!is.na(W))==0, 0, sum(W>=5, na.rm = TRUE)),
            maxrunsccd = ifelse(sum(!is.na(R))==0, 0, max(R, na.rm = TRUE))
          #  numfifties = ifelse(sum(!is.na(Runs))==0, 0, sum(Runs>=50, na.rm = TRUE))
            )%>% 
  ungroup() %>% right_join(B.inningses)


# ---- fielding ------------
# pull our the individual fielding performance data from the batting table

adamCatN <- B.batting %>%
  filter(`How Out` == "ct" & !is.na(fielder_id)) %>%
  group_by(
    fielder_id, match_id
  ) %>%
  summarise(catches = n()) %>% ungroup()

adamStumpN <- B.batting %>%
  filter(`How Out` == "st" & !is.na(fielder_id)) %>%
  group_by(
    fielder_id, match_id
  ) %>%
  summarise(stumpings = n()) %>% ungroup()

adamRunOutN <- B.batting %>%
  filter(`How Out` == "run out" & !is.na(fielder_id)) %>%
  group_by(
    fielder_id, match_id
  ) %>%
  summarise(runouts = n()) %>% ungroup()

B.fielding <- B.matchplayers %>%
  left_join(adamCatN, by=c("match_id", "player_id"="fielder_id")) %>% 
  left_join(adamStumpN, by=c("match_id", "player_id"="fielder_id")) %>% 
  left_join(adamRunOutN, by=c("match_id", "player_id"="fielder_id")) %>%
  mutate(
    Ct = ifelse(is.na(catches), 0, catches),
    Std = ifelse(is.na(stumpings), 0, stumpings),
    RO = ifelse(is.na(runouts), 0, runouts),
    Dis = Ct + Std,
    "Batting Club" = case_when(
      team == "home" ~ `Away Club`,
      team == "away" ~ `Home Club`
    ),
    "Fielding Club" =  case_when(
      team == "away" ~ `Away Club`,
      team == "home" ~ `Home Club`
    ),
    "fielding_club_id" =  case_when(
      team == "away" ~ away_club_id,
      team == "home" ~ home_club_id
    ),
    "batting_club_id" =  case_when(
      team == "home" ~ away_club_id,
      team == "away" ~ home_club_id
    ),
    us_fielding = fielding_club_id == as.numeric(conf$club_of_interest),
  ) %>% select(-catches, -stumpings, -runouts) %>%
  rename(Name = `Player Name`)


rm(B.matchplayers)
rm(adamCatN); rm(adamStumpN); rm(adamRunOutN)

makefieldsumm <- function(fielddata) {
 x <- fielddata %>%
  group_by(player_id, Name, fielding_club_id) %>% 
  summarise(Ct = sum(Ct),
            Std = sum(Std), 
            RO = sum(RO), 
            M = n()) %>%
  ungroup() %>%
  mutate(`Dis/Inns` = avRn((Ct + Std  + RO)/M))

return(x)
}

E.fieldsumm <- B.fielding %>% filter(Yr == conf$year_of_interest) %>% makefieldsumm()
  
B.fieldsumm <- B.fielding %>% makefieldsumm()
          

# ---- fall-of-wicket --------------
# not using this yet so commented out 
# E.fow <- E.inningses %>%
#   select(-c(bat, bowl)) %>%
#   rowwise() %>% 
#   mutate(fow = list(as.data.frame(fow)) )%>% unnest(fow)

# ---- allrounder -----

# some sort of allrounder omnitable; this relies on batting as the more complete record
# records with only bowling are therefore incomplete but not very interesting anyway
# not really sure why it's a full join but just left it as it seems to work

B.allround <- full_join(B.batting %>% filter(innings_number==1),
                 B.bowling%>% filter(innings_number==1),
                 by = c("match_id" = "match_id", "batsman_id" = "bowler_id",
                        "Match Summary", "Date", "Result", "Type", "Name", "actuallyDate" ),
                 keep = F, na_matches = "never", multiple = "first") %>%
  left_join(B.fielding %>% select(match_id, player_id, Ct, Std, Capt, "W-K"),
            by=c("match_id", "batsman_id" = "player_id"), keep=F, na_matches= "never" , multiple = "first") %>%
  select("Match Summary", "Date", "Result", "Type", "Name",
         "Ground" = "Ground.x", "Club" = "Batting Club.x",
         "Runs", "Balls", "BatSR" = "SR.x", "How Out", "4", "6", "Contrib",
         "O", "M", "R", "W" = "W.y", "Avg", "BowlSR" = "SR.y", "Econ", "Analy",
         "Ct", "Std", "Capt", "W-K",
         "is_circle" = "is_circle.x", "is_sphere" = "is_sphere.x", 
         "batting_club_id" = "batting_club_id.x", "Yr" = "Yr.x", "actuallyDate")


# ---- dataset size --------------------
# get the size, start and end of the dataset

B.dates <- list(
  our_latest = B.inningses %>% filter(is_circle == TRUE) %>%
    slice_max(actuallyDate, n = 1, with_ties = FALSE) %>% select(Date) %>% pull(Date),
  our_earliest = B.inningses %>% filter(is_circle == TRUE) %>% 
    slice_min(actuallyDate, n = 1, with_ties = FALSE) %>% select(Date) %>% pull(Date),
  all_latest = B.inningses  %>% 
    slice_max(actuallyDate, n = 1, with_ties = FALSE) %>% select(Date) %>% pull(Date),
  all_earliest = B.inningses %>%
    slice_min(actuallyDate, n = 1, with_ties = FALSE) %>% select(Date) %>% pull(Date),
  count_all = nrow(B.inningses),
  count_us = nrow(B.inningses %>% filter(is_circle==TRUE))
)

# ---- batting averages ------
# the function
makeBatAvgs <- function(batdata) {
  
  # batting avg table also includes fielding:
  adamField <- batdata %>%
    group_by(fielder_id) %>%
    summarise(
      Ct = sum(`How Out` == "ct"),
      Std = sum(`How Out` == "st"),
      RO = sum(`How Out` == "run out")
    ) %>%
    ungroup()
  
  outputBatAvgs <- batdata %>%
    mutate(RunsO = Runs) %>%
    group_by(batsman_id) %>%
    summarise(
      Runs = sum(Runs),
      Inns = sum(!is.na(RunsO)),
      NO = sum((`How Out` == "not out" | `How Out` == "retired not out") & !is.na(`How Out`)),
      Avg = avRn(Runs / (Inns - NO)),
      HSraw = max(RunsO),
      adsN = sum(RunsO == HSraw & `How Out` == "not out") ,
      HS = as.character(ifelse(adsN == 0, HSraw, paste(HSraw, "*", sep=""))),
      SR = avRn(sum(RunsWBF, na.rm = TRUE) * 100 / sum(Balls, na.rm = TRUE)),
      `50` = sum(RunsO >= 50 & RunsO < 100),
      `100` = sum(RunsO >= 100),
      #Ct = sum(`How Out` == "ct"),
      #Std = sum(`How Out` == "st"),
      #RO = sum(`How Out` == "run out"),
      BF = sum(Balls, na.rm = TRUE),
      `4s` = sum(`4`, na.rm = TRUE),
      `6s` = sum(`6`, na.rm = TRUE),
      #RunsWBF = sum(Runs),
    ) %>%
    ungroup() %>%
    left_join(adamField, by = c("batsman_id" = "fielder_id")) %>%
    left_join(E.players, by = c("batsman_id" = "player_id")) %>%
    mutate( is_us = case_when(
      club_id == conf$club_of_interest ~ TRUE,
      club_id != conf$club_of_interest ~ FALSE
    ))
  
  return(outputBatAvgs)
}

# subset just this year
A.bat <- B.batting %>%
  filter(Yr == conf$year_of_interest, !is.na(Runs))

E.batAvg.TY <- makeBatAvgs(A.bat)

# now all years
A.bat <- B.batting %>%
  filter(!is.na(Runs))

B.batAvg <- makeBatAvgs(A.bat)
B.batAvg <- B.batAvg[!is.na(B.batAvg$batsman_id),]

# Remove the temporary table
rm(A.bat)

# ---- bowling averages --------------------------------------------

# the function
makeBowlAvgs <- function(bowldata) {
  
  adamBestBowl <- bowldata %>% 
    select(c(bowler_id, R, W, Analy)) %>%
    filter(!is.na(bowler_id)) %>% 
    group_by(bowler_id) %>% 
    slice_max(W) %>% 
    slice_min(R, n=1, with_ties = F) %>%
    select(c(bowler_id, Analy))
  
  outputbowlAvg <- bowldata %>%
    group_by(bowler_id) %>%
    summarise(
      # Runs = sum(Runs),
      `5wi` = sum(W>=5),
      M = sum(M),
      R = sum(R),
      W = sum(W),
      BB = sum(BB, na.rm = TRUE),
      Avg = as.numeric(format(round(R / W, 2), nsmall = 2)),
      Econ = avRn((R * 6 / BB)),
      #SR = avRn(W * 100 / BB),
      SR = avRn(BB / W),
      O = paste(BB %/% 6, BB %% 6, sep="."),
    ) %>%
    ungroup() %>%
    left_join(adamBestBowl, by = join_by(bowler_id)) %>%
    rename(Best = Analy) %>%
    left_join(E.players, by = c("bowler_id" = "player_id")) %>%
    mutate( is_us = case_when(
      club_id == conf$club_of_interest ~ TRUE,
      club_id != conf$club_of_interest ~ FALSE
    ))
  
  return(outputbowlAvg)
}

# make a subset of just this year
A.bowl <- B.bowling %>%
  filter(Yr == conf$year_of_interest)

E.bowlAvg.TY <- makeBowlAvgs(A.bowl)

# now all years
B.bowlAvg <- makeBowlAvgs(B.bowling)
B.bowlAvg <- B.bowlAvg[!is.na(B.bowlAvg$bowler_id),]
#B.bowlAvg <- B.bowlAvg[!is.na(B.bowlAvg$Name),]

# Remove the temporary table
rm(A.bowl)


# ---- a joined avgs table ----

B.joinAvgs <- full_join(B.batAvg, B.bowlAvg, by = c("batsman_id"="bowler_id"),
                        keep = F, na_matches = "never", relationship = "many-to-many") %>%
  select(Name= Name.x, 
         Runs, Inns, NO, BatAvg = Avg.x, HS, BatSR = SR.x, `50`, `100`, BF, `4s`, `6s`,
         Ct, Std, RO,
         O, M, R, W, BB, BwlAvg = Avg.y, `5wi`, Econ, BwlSR = SR.y, Best,
         club_id.x, club_id.y, is_us.x, is_us.y)

# ==== match details ? teams =================================
# match details... CURRENT > SEASONS > YYYY > CLUBS
# this contains team information
# not quite sure how this is useful yet

# ==== Lookup tables ====

Y.players <- B.fielding %>% select(Name, player_id, team, home_club_id, away_club_id, Yr) %>%
  mutate(player_club = as.integer(case_when(
    team == "home" ~ home_club_id,
    team == "away" ~ away_club_id,
    TRUE ~ NA))
  ) %>% select(Name, player_id, player_club, Yr) %>% distinct() %>% 
  group_by(player_id, player_club, Name) %>% summarise(RecentYear = max(Yr)) %>% ungroup()

Y.currentplayers <- Y.players %>% filter(RecentYear %in% c(conf$year_of_interest, conf$last_year))

Y.leagues <- B.matchcompos %>% select(league_id, league_name) %>% distinct() %>%
  mutate(current = case_when(
    league_id %in% E.matches$league_id ~ TRUE,
    TRUE ~ FALSE),
    ours = case_when(
      league_id %in% B.matches[B.matches$is_circle==TRUE,]$league_id ~ TRUE,
      TRUE ~ FALSE)
  )

Y.compos <- B.matchcompos %>% select(competition_id, competition_name, competition_type, 
                                     no_of_overs, league_id, league_name) %>% distinct() %>%
  mutate(current = case_when(
    competition_id %in% E.matches$competition_id ~ TRUE,
    TRUE ~ FALSE),
    ours = case_when(
      competition_id %in% B.matches[B.matches$is_circle==TRUE,]$competition_id ~ TRUE,
      TRUE ~ FALSE)
  )

Y.grounds <- B.matches %>% select(ground_id, Ground) %>% rename(ground_name = Ground) %>% distinct()
# nb there is some association to club but ignoring that here. Also ignore whether current.

Y.clubs <- rbind(B.matches %>% select(home_club_id, `Home Club`) %>% distinct() %>% 
                   rename(club_id = home_club_id, 
                          club_name = `Home Club`), 
                 B.matches %>% select(away_club_id, `Away Club`) %>% distinct() %>%
                   rename(club_id = away_club_id, 
                          club_name = `Away Club`)) %>% 
  distinct() %>% drop_na()

Y.teams <-  rbind(B.matches %>% select(home_team_id, home_team_name, home_club_id, `Home Club`) %>% distinct() %>% 
                    rename(team_id = home_team_id, 
                           team_name = home_team_name,
                           club_id = home_club_id,
                           club_name = `Home Club`), 
                  B.matches %>% select(away_team_id, away_team_name, away_club_id, `Away Club`) %>% distinct() %>%
                    rename(team_id = away_team_id, 
                           team_name = away_team_name,
                           club_id = away_club_id,
                           club_name = `Away Club`)) %>% 
  distinct() %>% drop_na()

# ==== Pretty output tables ====
H.mallow <- c("Match Summary", "Date", "Result",
              "Type", "Ground", "Home Side", "Away Side",
              #could add firstinns and secondinns set if nicely renamed
              "Match Aggregate", "Match Overs")

W.matches <- B.matches %>% select(all_of(H.mallow))

H.pansy <- c("Match Summary", "Date", "Result",
             "Type", "Ground", "Home Side", "Away Side",
             #could add firstinns and secondinns set if nicely renamed
             #"Match Aggregate", "Match Overs"
             "position", "player_name", "captain", "wicket_keeper", "team")

W.matchplayers <- E.matchplayers %>% select(all_of(H.pansy))

H.impatiens <- c("Match Summary", "Date", "Result",
                 "Type", "Ground", #"Home Side", "Away Side",
                 "Batting Side", "Score", "Ovs",  "Fielding Side", "Opposition Score",
                 "Extras", "Byes" ,"Leg byes", "Wides", "No-balls", "% Extras", 
                 "Inns of match", 
                 "Res", "Ven")

W.inningses <- B.inningses %>% select(all_of(H.impatiens))

H.begonia <- c("Match Summary", "Date", "Result",
               "Type", "Ground", #"Home Side", "Away Side",
               "Batting Side", "Score", "Ovs",  "Fielding Side", "Opposition Score",
               #"Extras", "Byes" ,"Leg byes", "Wides", "No-balls", "% Extras", 
               "Inns of match", 
               "Res", "Ven",
               "Name", "Runs", "Balls", "SR", "How Out", "4", "6", "Contrib")

W.batting <- B.batting %>% select(all_of(H.begonia))

H.lilac <- c("Match Summary", "Date", "Result",
             "Type", "Ground", #"Home Side", "Away Side",
             "Batting Side", "Score", "Ovs",  "Fielding Side", "Opposition Score",
             #"Extras", "Byes" ,"Leg byes", "Wides", "No-balls", "% Extras", 
             "Inns of match", 
             "Res", "Ven",
             "Name", "O", "M", "R", "W", "Avg", "SR", "Econ")

W.bowling <- B.bowling %>% select(all_of(H.lilac))

H.foxglove <- c("Match Summary", "Date", "Result",
                "Type", "Ground", #"Home Side", "Away Side",
               # "Batting Side", "Score", "Ovs",  "Fielding Side", "Opposition Score",
                #"Extras", "Byes" ,"Leg byes", "Wides", "No-balls", "% Extras", 
                #"Inns of match", 
                #"Res", "Ven",
                "Name", "Ct", "Std")

W.fielding <- B.fielding %>% select(all_of(H.foxglove))

H.waterlily <- c("Match Summary", "Date", "Result",
              "Type", "Ground", 
              "Name", "Club", "Runs", "Balls", "BatSR", "How Out", "4", "6", "Contrib",
              "O", "M", "R", "W", "Avg", "BowlSR", "Econ", "Ct", "Std", "Capt", "W-K") 

W.allround <- B.allround %>% select(all_of(H.waterlily))

# ==== Output data ====
# dump out for use by other scripts
save(E.batAvg.TY, file="./data/EbatAvgTY")
save(B.batAvg, file="./data/BbatAvg")
#write.csv(Z.club, file="./data/Zclub.csv")
save(B.batting, file="./data/Bbatting")
save(E.bowlAvg.TY, file="./data/EbowlAvgTY")
save(B.bowlAvg, file="./data/BbowlAvg")
save(B.bowling, file="./data/Bbowling")
save(B.dates, file="./data/Bdates")
save(B.fielding, file="./data/Bfielding")
save(B.fieldsumm, file="./data/Bfieldsumm")
save(E.fieldsumm, file="./data/Efieldsumm")
save(B.inningses, file="./data/Binningses")
save(B.allround, file="./data/Ballround")
save(B.joinAvgs, file="./data/BjoinAvgs")
save(E.matches, file="./data/Ematches")
save(B.matches, file="./data/Bmatches")
save(E.players, file="./data/Eplayers")
save(E.playersNC, file="./data/EplayersNC")
save(Y.currentplayers, file="./data/Ycplayers")
save(Y.players, file="./data/Yplayers")
save(Y.clubs, file="./data/Yclubs")
save(Y.compos, file="./data/Ycompos")
# 
# zxc <- B.bowling %>% filter(Yr == conf$year_of_interest) 
# write.csv(zxc, file="./data/allbowl23.csv")


# ==== suggest-o-matic outputs
#table(F.inningses.us$league_id)
