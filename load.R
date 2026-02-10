# ==== initialise ====
library(jsonlite)
library(yaml)
library(tidyverse)

## load the config
conf <- yaml.load_file("./config/config.yaml")
conf$last_year <- as.character(as.integer(conf$year_of_interest) - 1)
A.inferredovers <- read_tsv("./config/compo-to-over.tsv", col_types = list(competition_id="i", inferred_overs="i"))

# ==== current clubs and their players ====
# CURRENT > CLUBS
# this is a load of dirs, named by club id, each containing a list of players

# Parent directory containing subdirectories, get list of them
parent_directory <- "newdata/current/clubs"
subdirectories <- list.dirs(parent_directory, full.names = TRUE)

# Function to load and process a JSON file from a directory
load_json_file <- function(directory) {
  json_files <- list.files(directory, pattern = "*.json", full.names = TRUE)
  
  if (length(json_files) == 0) {
    return(NULL)  # No JSON files in the directory
  }
  
  json_data <- fromJSON(json_files[1])  # Load the first JSON file
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

# Combine data frames into a single tibble
E.players <- json_data %>% compact() %>% bind_rows() %>%
               rename(player_id=member_id, Name = name)

# make a lookup just on player ID ie no club id, which means duplicates
E.playersNC <- E.players %>%
  select(player_id, Name) %>% 
  distinct(player_id, Name) %>% # rm duplicates caused by name changes==
  group_by(player_id) |>
  slice_min(Name) |>
  ungroup()

#cleanup
rm(parent_directory, json_data, subdirectories, load_json_file)

# ==== the match detail loading function  ====

# define what we want as match headers -- drop umpire names and such
H.morningglory <- c("match_id", "match_date", "ground_name", "ground_id", "competition_id","league_id",
                   "competition_type", "match_type", "no_of_overs", "batted_first", "no_of_players",
                   "home_team_name", "home_team_id", "home_club_name", "home_club_id",
                   "away_team_name", "away_team_id", "away_club_name", "away_club_id", 
                   "toss", "result_description", "result_applied_to", "toss_won_by_team_id")

H.cornflower <- c("match_id", "match_date", "competition_id","league_id", "league_name",
                    "competition_name", "competition_type", "match_type", "no_of_overs",
                    "home_team_id", "away_team_id")

# Function to load and process a JSON file. Takes type c competition for a special mode
load_json_file <- function(file, type) {
  json_data <- fromJSON(file)
  if (is.null(json_data$match_date)) {return(NULL)}
  
  # the competition function - making a competition/league lookup, but need to first
  # check those columns exist because friendlies etc don't have those keys
  if (type=="c") { 
    if ("league_name" %in% names(json_data) ){
      json_data["competition_name"] <- as.character(json_data["competition_name"])
      jdc <- json_data[H.cornflower]
      jdc$Yr <- as.integer(format(as.Date(jdc$match_date, "%Y-%m-%dT%H:%M:%S"), format = "%Y"))
      return(jdc)} else {return(NULL)}} # nb we return here 
  
  # make a nice subset of interesting headings 
  jds <- json_data[H.morningglory]
  
  # deal with overs - calculate into decimal for later addition
  foo <- data.frame(rep(0, length(json_data$innings$overs)))
  foo$ovs.o <- str_split_i(json_data$innings$overs, "\\.", 1)
  foo$ovs.b <- str_split_i(json_data$innings$overs, "\\.", 2)
  foo$ovs.b[is.na(foo$ovs.b)] <- 0
  foo$ovs.bf <- as.numeric(foo$ovs.b) / 6
  foo$ovs.bf[foo$ovs.b >= 6] <- 5 # TODO why this? 
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
  #  jds$firstinnsbattname <- as.character(json_data$innings[1,]$team_batting_name) #
    jds$firstinnsscore <- paste( json_data$innings[1,]$runs, "/", json_data$innings[1,]$wickets, sep="" )
    jds$firstinnstotal <- json_data$innings[1,]$runs
  #  jds$firstinnsniceovs <- paste( "(", json_data$innings[1,]$overs, ")", sep="" )
    jds$secondinnsbattid <- json_data$innings[2,]$team_batting_id
    jds$secondinnsscore <- paste( json_data$innings[2,]$runs, "/", json_data$innings[2,]$wickets, sep="" )
    jds$secondinnstotal <- json_data$innings[2,]$runs
  #  jds$`Match Summary` <- paste(jds$firstinnsbattname, jds$firstinnsscore, jds$firstinnsniceovs, ";",                                 jds$secondinnsbattname, jds$secondinnsscore, jds$secondinnsniceovs)
    jds$`Match Summary` <- paste( as.character(json_data$innings[1,]$team_batting_name),
                                  jds$firstinnsscore,  paste( "(", json_data$innings[1,]$overs, ")", sep="" ), ";",
                                  as.character(json_data$innings[2,]$team_batting_name),
                                  jds$secondinnsscore, paste( "(", json_data$innings[2,]$overs, ")", sep="" ))
  } else {
    jds$firstinnsbattid <- NA
  #  jds$firstinnsbattname <- ""
    jds$firstinnsscore <- ""
    jds$firstinnstotal <- NA
 #   jds$firstinnsniceovs <- ""
    jds$secondinnsbattid <- NA
    jds$secondinnsscore <- ""
    jds$`Match Summary` <- ""
  }
  
  jds$`Match Aggregate` <- sum(json_data$innings$runs)
  jds$`Match Aggregate Extras` <- sum(json_data$innings$total_extras)
  zxc <- sum(json_data$innings$decimal_overs)
  jds$matchAggDecOvs <- zxc
  jds$`Match Overs` <- paste(floor(zxc), round((zxc %% 1 ) * 6), sep=".")
  jds$actuallyDate <- as.Date(jds$match_date, "%Y-%m-%dT%H:%M:%S")
  jds$Date <- format(jds$actuallyDate, format = "%d %b %Y")
  jds$Yr <- as.integer(format(jds$actuallyDate, format = "%Y"))
  jds$`Day of Year` <- yday(jds$actuallyDate)
  
  jds$is_circle <- FALSE
  jds$is_smallsphere <- FALSE
  jds$is_sphere <- FALSE
  
  jds$is_circle[jds$home_club_id == as.numeric(conf$club_of_interest)] <- TRUE
  jds$is_circle[jds$away_club_id == as.numeric(conf$club_of_interest)] <- TRUE
  jds$is_smallsphere[jds$league_id %in% conf$leagues_of_interest] <- TRUE 
  jds$is_sphere[jds$is_circle == TRUE | jds$is_smallsphere == TRUE] <- TRUE
  
  if (jds$is_circle){
    jds[["Our Team"]] <- case_when(
      jds[["home_team_id"]] %in% names(conf$our_teams_names) ~ jds$home_team_name,
      jds[["away_team_id"]] %in% names(conf$our_teams_names) ~ jds$away_team_name,
      TRUE ~ "Other" # other means a team of ours but not one set as interesting
    )
  } else  jds[["Our Team"]] <- ""
  
  # rename these now, rather than with dplyr later, because tidier
  jds[["Ground"]] <- jds[["ground_name"]]
  jds[["Home Club"]] <- jds[["home_club_name"]]
  jds[["Away Club"]] <- jds[["away_club_name"]]
  jds[["Home Side"]] <- paste(jds[["home_club_name"]], jds[["home_team_name"]], sep=" - ")
  jds[["Away Side"]] <- paste(jds[["away_club_name"]], jds[["away_team_name"]], sep=" - ")
  jds[["Result_"]] <- jds[["result_description"]]
  jds[["Type"]] <- as.factor(jds[["competition_type"]])
  #jds[["Winner"]] <- jds[["result_applied_to"]]
  
  # interpolate missing no_of_overs data -- this seems to be friendlies and some incorrectly set old competitions
  
  if (is.null(jds$no_of_overs)) {
    jds$no_of_overs <- NA
  }
  
  if (is.na(jds$no_of_overs) & !is.null(jds$competition_id)) {
    if (jds$competition_id %in% A.inferredovers$competition_id ){
    ino <- pull(A.inferredovers[A.inferredovers$competition_id==jds$competition_id,2])
    } else ino <- NA
   # if (is_null(ino)){ ino <- NA}
    #print(ino)
    jds$no_of_overs = case_when(
    jds$competition_id %in% A.inferredovers$competition_id ~ ino,
    TRUE ~ NA
    )
  }

  if (is.na(jds$no_of_overs)) {
    jds$no_of_overs = case_when(
    #  !is.null(jds$competition_id) &    jds$competition_id %in% A.inferredovers$competition_id ~ 99,
      jds$matchAggDecOvs == 0 ~ NA, # of 6694, about 100 have no overs recorded but runs; 80 are conceded etc
      jds$firstinnsdecovers > 60 ~ NA, # to clear any obvious errors
      jds$secondinnsdecovers > 60 ~ NA, # to clear any obvious errors
      jds$firstinnsdecovers > 0  | jds$secondinnsdecovers > 0
      ~ max(ceiling(0.2 * jds$firstinnsdecovers) / 0.2, 
            ceiling(0.2 * jds$secondinnsdecovers) / 0.2, 
            0, na.rm = TRUE), # round up to nearest 5
      TRUE ~ NA
    )
  }
  
  jds <- jds[-which(names(jds) %in% c("ground_name", "home_club_name", "away_club_name", 
                                      "result_description", "competition_type", 
                                      "firstinnsdecovers", "secondinnsdecovers"))]
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
      is.na(jds[["result_applied_to"]]) ~ "?",
      jds[["result_applied_to"]] == jds[["home_team_id"]] ~ "HW",
      jds[["result_applied_to"]] == jds[["away_team_id"]] ~ "AW",
    )
  }
  
  jds$result_match <- as.factor(jds$result_match)
  
  #a result abbreviation for the club orientation
  if (!is.null(jds$Result_) ){ #& !is.null(jds$result_applied_to))
    
    jds$result_club <- case_when(
      jds[["is_circle"]] == FALSE ~ "n/a",
      jds[["Result_"]]  == "Abandoned" ~ "A",
      jds[["Result_"]]  == "Tied" ~ "T",
      jds[["Result_"]]  == "Cancelled" ~ "C",
      jds[["Result_"]]  %in% c("Draw", "Drawn", "Trophy Shared") ~ "D",
      jds[["Result_"]]  == "Match In Progress" ~ "?",
      TRUE ~ "")
    
    if (!is.null(jds$result_applied_to) & jds$result_club == ""){
      jds$result_club <- case_when(
        jds[["result_applied_to"]] %in% names(conf$our_teams_names) ~ "W",
        TRUE ~ "L"
      )
    }
  }  
  
  jds$result_club <- as.factor(jds$result_club)
  
  # result and batting first
  if (!is.null(jds$result_applied_to)){
    jds$bat_first_win <- case_when(
      jds[["result_applied_to"]] == jds$firstinnsbattid ~ TRUE,
      jds[["result_applied_to"]] == jds$secondinnsbattid ~ FALSE,
      TRUE ~ NA
    )
  }
  
  #a result of the toss for this club
  if (!is.null(jds$toss_won_by_team_id) ){ #& !is.null(jds$result_applied_to))
    jds$`Our toss result` <- case_when(
      jds[["is_circle"]] == FALSE ~ "n/a",
      jds[["toss_won_by_team_id"]]  %in% names(conf$our_teams_names) ~ "W",
      TRUE ~ "L")}  
  
  # result and toss winner
  if (!is.null(jds$toss_won_by_team_id)  & (!is.null(jds$result_applied_to)) ){ #& !is.null(jds$result_applied_to))
    jds$win_toss_win_match <- case_when(
      jds[["result_applied_to"]] == jds[["toss_won_by_team_id"]] ~ TRUE,
      jds[["result_applied_to"]] != jds[["toss_won_by_team_id"]]  ~ FALSE,
      TRUE ~ NA
    )
  }
  
  # try to work out the winning margin
  # first we need to remove anything without a winner, and anything we can't pick
  # up each inns score eg two-innings matches and concessions
  # relies on draws and ties being identified elsewhere, and having 11 batters
  
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
          json_data$innings[1,]$runs > json_data$innings[2,]$runs ~ 
            paste(json_data$innings[1,]$team_batting_name,"win by",
                  json_data$innings[1,]$runs - json_data$innings[2,]$runs, "runs"),
          json_data$innings[1,]$runs < json_data$innings[2,]$runs ~
            paste(json_data$innings[2,]$team_batting_name,"win by",
                  10 - json_data$innings[2,]$wickets, "wickets"),
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
  
  jds$inns <- as.data.frame(json_data[["innings"]], optional = FALSE)
  
  #player level data
    if(!is.null(nrow(json_data$players)) &&  nrow(json_data$players) == 2){ 
      
      fooH <- fooA <- data.frame(position=integer(),
                                 player_name=character(), player_id=numeric(), 
                                 captain=logical(), wicket_keeper=logical(),
                                 team=factor())
      
      if( !is.null(nrow(json_data[["players"]][["home_team"]][[1]])) ){
        fooH <- json_data[["players"]][["home_team"]][[1]]
        fooH$team <- "home"
        numcapts <- length(which(json_data[["players"]][["home_team"]][[1]][["captain"]]==TRUE))
        if (numcapts == 1) {jds$homecapt <- json_data[["players"]][["home_team"]][[1]][[which(json_data[["players"]][["home_team"]][[1]][["captain"]]==TRUE),"player_name"]]}
        numkeeps <- length(which(json_data[["players"]][["home_team"]][[1]][["wicket_keeper"]]==TRUE))
        if (numkeeps == 1) {jds$homekeeper <- json_data[["players"]][["home_team"]][[1]][[which(json_data[["players"]][["home_team"]][[1]][["wicket_keeper"]]==TRUE),"player_name"]]}
      } 
      if( !is.null(nrow(json_data[["players"]][["away_team"]][[2]])) ){
        fooA <- json_data[["players"]][["away_team"]][[2]]
        fooA$team <- "away"
        numcapts <- length(which(json_data[["players"]][["away_team"]][[2]][["captain"]]==TRUE))
        if (numcapts == 1) {jds$awaycapt <- json_data[["players"]][["away_team"]][[2]][[which(json_data[["players"]][["away_team"]][[2]][["captain"]]==TRUE),"player_name"]]}
        numkeeps <- length(which(json_data[["players"]][["away_team"]][[2]][["wicket_keeper"]]==TRUE))
        if (numkeeps == 1) {jds$awaykeeper <- json_data[["players"]][["away_team"]][[2]][[which(json_data[["players"]][["away_team"]][[2]][["wicket_keeper"]]==TRUE),"player_name"]]}
      } 
      foo <- rbind(fooH, fooA)
      jds$players <- as.data.frame(foo, optional = FALSE)
    }
    else jds$players <- NA
        
  return(jds)
}

# ==== current year data load  ====
# == match details... CURRENT > SEASONS > YYYY > MATCH_DETAILS

# Directory containing JSON files, one file per match
directory <- "./newdata/current/seasons/2025/match_details" #TODO fix this to take year from config file

# Get list of JSON files in the directory TODO throw error if that current dir is empty
json_files <- list.files(directory, pattern = "*.json", full.names = TRUE)

# apply that on the current raw data:
json_dataP <- json_dataM <- json_dataI <- map(json_files, load_json_file, "x")

# add in the row number to innings records, then also drop it if exists for match level
for (i in 1:length(json_dataI)){
  json_dataI[[i]]$inns$`Inns of match` <- as.factor(rownames(json_dataI[[i]]$inns))
  
  if ('players' %in% names(json_dataI[[i]])) {json_dataI[[i]] <- json_dataI[[i]][!names(json_dataI[[i]]) %in% c("players")]}
  
  if ('inns' %in% names(json_dataP[[i]])) {json_dataP[[i]] <- json_dataP[[i]][!names(json_dataP[[i]]) %in% c("inns")]}

  if ('inns' %in% names(json_dataI[[i]])) {json_dataM[[i]] <- json_dataI[[i]][!names(json_dataI[[i]]) %in% c("inns")]}
    else {json_dataM[[i]] <- json_dataI[[i]]}
}
rm(i)

E.inningses <- json_dataI %>% bind_rows() %>% unnest(inns)
E.matches <- json_dataM %>% bind_rows()
E.matchplayers <- json_dataP %>% bind_rows() %>% unnest(players)

# unstack competiton data 
json_dataC <- map(json_files, load_json_file, "c")
E.matchcompos <- json_dataC %>% bind_rows() 

# cleanup
rm(directory, json_files, json_dataI, json_dataM, json_dataC)

# ==== previous seasons match details =====
# previous > previous-seasons > seasons
#this is a load of dirs, named by year each containing a dir called match_details, which contains the jsons

# Parent directory containing subdirectories
parent_directory <- "newdata/previous-seasons/seasons"

# Get list of subdirectories
subdirectories <- list.dirs(parent_directory, full.names = TRUE)

# Use the matching indices to extract the matching directory names
matching_directories <- subdirectories[grep("match_details", subdirectories)]

# Loop through each directory in matching_directories
json_files <- c() # an  empty vector to store the file paths
for (dir in matching_directories) {
  # Get a list of .json files in the current directory
  json_files <- c(json_files, list.files(dir, pattern = "*.json", full.names = TRUE))
}

rm(dir, parent_directory, subdirectories, matching_directories)

# apply that on the old raw data, removing null values
json_dataP <- json_dataM <- json_dataI <- compact(map(json_files, load_json_file, "x"))

# add in the row number to innings records, then drop unwanted stuff
for (i in 1:length(json_dataI)){
  if (!is.null(nrow(json_dataI[[i]]$inns))) {json_dataI[[i]]$inns$`Inns of match` <- as.factor(rownames(json_dataI[[i]]$inns))}
  if ('players' %in% names(json_dataI[[i]])) {json_dataI[[i]] <- json_dataI[[i]][!names(json_dataI[[i]]) %in% c("players")]}
  if ('inns' %in% names(json_dataP[[i]])) {json_dataP[[i]] <- json_dataP[[i]][!names(json_dataP[[i]]) %in% c("inns")]}
  if ('inns' %in% names(json_dataM[[i]])) {json_dataM[[i]] <- json_dataI[[i]][!names(json_dataI[[i]]) %in% c("inns")]}
  else {json_dataM[[i]] <- json_dataI[[i]]}
}
rm(i)

G.inningses <- json_dataI %>% bind_rows() %>% unnest(inns)
G.matches <- json_dataM %>% bind_rows()
G.matchplayers <- json_dataP %>% bind_rows() %>% unnest(players)

# ==== unstack competition data ====
json_dataC <- map(json_files, load_json_file, "c")
G.matchcompos <- json_dataC %>% bind_rows() 

# cleanup
rm(json_files, json_dataI, json_dataM, json_dataC, json_dataP, load_json_file)

# ==== merge old and new years ==== 
# this used to be done later, after the prettifying and merging
C.matches <- rbind(E.matches, G.matches)
C.inningses <- rbind(E.inningses, G.inningses)
B.matchplayers <- rbind(E.matchplayers, G.matchplayers)
B.matchcompos <- rbind(E.matchcompos, G.matchcompos)

# ==== filter out uninteresting things, but keep the old set for now ====
B.matches <- C.matches %>% 
  filter(!home_team_id %in% conf$excluded_teams, 
         !away_team_id %in% conf$excluded_teams, 
         !league_id %in% conf$excluded_leagues,
         !match_id %in% conf$excluded_matches,
         !Result %in% c("Cancelled", "Match In Progress"),
         Yr >= conf$start_year)

B.inningsesX <- C.inningses %>% 
  filter(!home_team_id %in% conf$excluded_teams, 
         !away_team_id %in% conf$excluded_teams, 
         !league_id %in% conf$excluded_leagues,
         !match_id %in% conf$excluded_matches,
         !Result %in% c("Cancelled", "Match In Progress"),
         Yr >= conf$start_year)

B.matchplayers <- B.matchplayers %>% 
  filter(!home_team_id %in% conf$excluded_teams, 
         !away_team_id %in% conf$excluded_teams, 
         !league_id %in% conf$excluded_leagues,
         !match_id %in% conf$excluded_matches,
         !Result %in% c("Cancelled", "Match In Progress"),
         Yr >= conf$start_year)

B.matchcompos <- B.matchcompos %>% 
  filter(!home_team_id %in% conf$excluded_teams, 
         !away_team_id %in% conf$excluded_teams, 
         !league_id %in% conf$excluded_leagues,
         !match_id %in% conf$excluded_matches,
        # !Result %in% c("Cancelled", "Match In Progress"),
         Yr >= conf$start_year)

# ==== write out and tidy up====
save(B.matches, B.inningsesX, B.matchplayers, B.matchcompos, E.players, E.playersNC, file="./data/coredata.RData")
rm(G.matches, G.inningses, G.matchcompos, G.matchplayers, C.matches, C.inningses)
rm(E.matches, E.inningses, E.matchcompos, E.matchplayers)
