# ==== initialise ====
# new get/format data
library(jsonlite)
library(purrr)
library(dplyr)
library(stringr)
library(yaml)
library(tidyr)

## load the config
conf <- yaml.load_file("./config/config.yaml")

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

# ==== current match details =================================================
# == match details... CURRENT > SEASONS > YYYY > MATCH_DETAILS
# this is one file per match

# Directory containing JSON files
directory <- "./newdata/current/seasons/2023/match_details"

# Get list of JSON files in the directory
json_files <- list.files(directory, pattern = "*.json", full.names = TRUE)

# define what we want as match headers -- drop umpire names and such
matchMainHdrs <- c("match_id", "match_date", "ground_name", "ground_id", "competition_id","league_id",
                   "home_team_name", "home_team_id", "home_club_name", "home_club_id",
                   "away_team_name", "away_team_id", "away_club_name", "away_club_id", 
                   "toss", "result_description", "result_applied_to"
                   #,"innings"
)

# Function to load and process a JSON file. Takes type m match, i inns
load_json_file <- function(file, type) {
  json_data <- fromJSON(file)
  jds <- json_data[matchMainHdrs]
  #print(as.data.frame(json_data_sel["innings"]))
  
  foo <- data.frame(rep(0, length(json_data$innings$overs)))
  foo$ovs.o <- str_split_i(json_data$innings$overs, "\\.", 1)
  foo$ovs.b <- str_split_i(json_data$innings$overs, "\\.", 2)
  foo$ovs.b[is.na(foo$ovs.b)] <- 0
  foo$ovs.bf <- as.numeric(foo$ovs.b) / 6
  foo$ovs.bf[foo$ovs.b >= 6] <- 5
  json_data$innings$decimal_overs <- as.numeric(foo$ovs.o) + as.numeric(foo$ovs.bf)
  
 # if (type=="i") jds[["inns"]] <- as.data.frame(json_data["innings"])
  if (type=="i") jds$inns <- as.data.frame(json_data[["innings"]], optional = FALSE)
  #print(json_data_sel)
  
  jds$matchAggRuns <- sum(json_data$innings$runs)
  zxc <- sum(json_data$innings$decimal_overs)
  jds$matchAggOvs <- paste(floor(zxc), round((zxc %% 1 ) * 6), sep=".")
  
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
  jds[["Result"]] <- jds[["result_description"]]
  #jds[["Winner"]] <- jds[["result_applied_to"]]
  
  jds <- jds[-which(names(jds) %in% c("ground_name", "home_club_name", "away_club_name", 
                                            "result_description"))]
  
  return(jds)
}

# apply that on the raw data in match and innings mode
json_data <- map(json_files, load_json_file, "m")
E.matches <- json_data %>% bind_rows()


json_data2 <- map(json_files, load_json_file, "i")
#print(json_data2)
# add in the row number to innings records
for (i in 1:length(json_data2)){
  json_data2[[i]]$inns$match_innings <- as.numeric(rownames(json_data2[[i]]$inns))
}
rm(i)

E.inningses <- json_data2 %>% bind_rows() %>% unnest(inns)

# cleanup
rm(directory)
rm(json_files)
rm(load_json_file)
rm(json_data)
rm(json_data2)

# make some new and prettier bits of data

E.inningses <- E.inningses %>%
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

E.inningses <- E.inningses %>%
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
    fielding_team_name = case_when(
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
    
  )

E.inningses <- mutate(E.inningses, 
                      Score = paste(Total, W, sep="/"),
                      extras_proportion = Extras / Total,
                      `% Extras` = paste(format(Extras * 100 / Total, digits = 1),"%"),
                      Ven = as.factor(case_when(
                        home_team_id == batting_team_id ~ "H",
                        away_team_id == batting_team_id ~ "A",
                        TRUE ~ as.character(NA)
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

E.batting <- E.inningses %>%
               select(-c(bowl,fow)) %>%
                 rowwise() %>% 
                   mutate(bat = list(as.data.frame(bat)) )%>% unnest(bat)


E.batting <- E.batting %>%
  rename(Pos = position,
         Name = batsman_name,
         "How Out" = how_out,
         Runs = runs,
         "4" = fours,
         "6" = sixes,
         Balls = balls,
  )

E.batting <- E.batting %>%
  mutate(
    SR = ifelse(Balls == 0 & Runs != 0, NA, format(round(Runs * 100 / Balls, 2), nsmall = 2)),
    contribpc = ifelse(Total == 0, NA, Runs / Total),
    Contrib = ifelse(Total == 0, NA, paste(format((Runs * 100)/Total , digits = 1),"%")),
    RunsWBF =  ifelse(is.na(Balls), NA, Runs)
  )


# ---- bowling ------ 
# unstack the individual bowling performance data

E.bowling <- E.inningses %>%
  select  (-c(bat, fow)) %>% 
  rowwise() %>% 
  mutate(bowl = list(as.data.frame(bowl)) )%>% unnest(bowl)

E.bowling <- E.bowling %>%
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
    Avg = ifelse(W == 0, NA, format(R / W, digits = 2)),
    ovs.o = as.numeric(ifelse(is.na(str_split_i(O, "\\.", 1)),0,str_split_i(O, "\\.", 1))),
    ovs.b = as.numeric(ifelse(is.na(str_split_i(O, "\\.", 2)),0,str_split_i(O, "\\.", 2))),
    BB = ifelse(is.na(O), NA, (ovs.o * 6 ) + ovs.b + Wd + NB),
    SRx = ifelse(is.na(BB), NA, ifelse( BB == 0, NA, format(W * 100 / BB, digits = 2) ) ),
    SR = ifelse(is.na(SRx), NA, as.numeric(SRx)),
    Econ = ifelse((ovs.o + ovs.b/6) ==0, NA, as.numeric(format(R  / (ovs.o + ovs.b/6), digits = 2))  )
  ) %>% select(-SRx)

# ---- fielding ------------
# pull our the individual fielding performance data from the batting table

adamCat <- E.batting %>%
  filter(`How Out` == "ct" & !is.na(fielder_id)) %>%
  group_by(
    fielder_id, match_id, fielding_club_id, fielder_name, batting_club_id,
    `Batting Club`, `Fielding Club`, Ground, Date, competition_id, league_id,
    actuallyDate, Yr
  ) %>%
  summarise(catches = n())

adamStump <- E.batting %>%
  filter(`How Out` == "st" & !is.na(fielder_id)) %>%
  group_by(
    fielder_id, match_id, fielding_club_id, fielder_name, batting_club_id,
    `Batting Club`, `Fielding Club`, Ground, Date, competition_id, league_id,
    actuallyDate, Yr
  ) %>%
  summarise(stumpings = n())

adamRunOut <- E.batting %>%
  filter(`How Out` == "run out" & !is.na(fielder_id)) %>%
  group_by(
    fielder_id, match_id, fielding_club_id, fielder_name, batting_club_id,
    `Batting Club`, `Fielding Club`, Ground, Date, competition_id, league_id,
    actuallyDate, Yr
  ) %>%
  summarise(runouts = n())

E.fielding <- adamCat %>%
  full_join(adamStump, by = c(
    "fielder_id", "match_id", "fielding_club_id", "fielder_name",
    "batting_club_id", "Batting Club", "Fielding Club", "Ground", 
    "Date", "competition_id", "league_id", "actuallyDate", "Yr"
  )) %>%
  mutate(
    Ct = ifelse(is.na(catches), 0, catches),
    Std = ifelse(is.na(stumpings), 0, stumpings),
    Dis = Ct + Std
  ) %>%
  rename(
    Club = `Fielding Club`,
    Name = fielder_name,
    Oppos = `Batting Club`
  ) %>%
  ungroup() %>%
  mutate(
    us_fielding = fielding_club_id == as.numeric(conf$club_of_interest),
    is_circle = (batting_club_id == as.numeric(conf$club_of_interest) | 
                   fielding_club_id == as.numeric(conf$club_of_interest)),
    is_smallsphere = competition_id %in% conf$competitions_of_interest,
    is_ourleagues = league_id %in% conf$leagues_of_interest,
    is_sphere = is_circle | is_smallsphere
  )

rm(adamCat); rm(adamStump); rm(adamRunOut)

# ---- fall-of-wicket --------------
# not using this yet so commented out 
# E.fow <- E.inningses %>%
#   select(-c(bat, bowl)) %>%
#   rowwise() %>% 
#   mutate(fow = list(as.data.frame(fow)) )%>% unnest(fow)

# ---- dataset size --------------------
# get the size, start and end of the dataset

E.dates <- list(
  our_latest = E.inningses %>% filter(is_circle == TRUE) %>%
                  slice_max(actuallyDate, n = 1, with_ties = FALSE) %>% select(Date) %>% pull(Date),
  our_earliest = E.inningses %>% filter(is_circle == TRUE) %>% 
                  slice_min(actuallyDate, n = 1, with_ties = FALSE) %>% select(Date) %>% pull(Date),
  all_latest = E.inningses  %>% 
                 slice_max(actuallyDate, n = 1, with_ties = FALSE) %>% select(Date) %>% pull(Date),
  all_earliest = E.inningses %>%
                  slice_min(actuallyDate, n = 1, with_ties = FALSE) %>% select(Date) %>% pull(Date),
  count_all = nrow(E.inningses),
  count_us = nrow(E.inningses %>% filter(is_circle==TRUE))
)

# ---- batting averages ---------------------

A.bat <- E.batting %>%
  filter(Yr == conf$year_of_interest, !is.na(Runs))

# batting avg table also includes fielding:
adamField <- A.bat %>%
  group_by(fielder_id) %>%
  summarise(
    Ct = sum(`How Out` == "ct"),
    Std = sum(`How Out` == "st"),
    RO = sum(`How Out` == "run out")
  ) %>%
  ungroup()

E.batAvg.TY <- A.bat %>%
  mutate(RunsO = Runs) %>%
  group_by(batsman_id) %>%
  summarise(
    Runs = sum(Runs),
    Inns = sum(!is.na(RunsO)),
    NO = sum((`How Out` == "not out" | `How Out` == "retired not out") & !is.na(`How Out`)),
    Avg = format(round(Runs / (Inns - NO), 2), nsmall = 2),
    HS = max(RunsO),
    SR = format(round(sum(RunsWBF, na.rm = TRUE) * 100 / sum(Balls, na.rm = TRUE), 2), nsmall = 2),
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

# Remove the temporary table
rm(A.bat); rm(adamField)

# ---- bowling averages --------------------------------------------

# make a temp table so we can use the same code for this-year and 'all time'
A.bowl <- E.bowling %>%
  filter(Yr == conf$year_of_interest)

adamBestBowl <- A.bowl %>% 
  select(c(bowler_id, R, W, Analy)) %>%
  filter(!is.na(bowler_id)) %>% 
  group_by(bowler_id) %>% 
  slice_max(W) %>% 
  slice_min(R, n=1, with_ties = F) %>%
  select(c(bowler_id, Analy))
 
E.bowlAvg.TY <- A.bowl %>%
  group_by(bowler_id) %>%
  summarise(
   # Runs = sum(Runs),
    M = sum(M),
    R = sum(R),
    W = sum(W),
    BB = sum(BB),
    `5wi` = sum(W>=5),
    Avg = as.numeric(format(round(R / W, 2), nsmall = 2)),
    Econ = format(round(R * 6 / BB, 2), nsmall = 2),
    SR = format(round(W * 100 / BB, 2), nsmall = 2),
    O = paste(BB %/% 6, BB %% 6, sep="."),
  ) %>%
  ungroup() %>%
  left_join(adamBestBowl) %>%
  rename(Best = Analy) %>%
  left_join(E.players, by = c("bowler_id" = "player_id")) %>%
  mutate( is_us = case_when(
    club_id == conf$club_of_interest ~ TRUE,
    club_id != conf$club_of_interest ~ FALSE
  ))

# Remove the temporary table
rm(A.bowl); rm(adamBestBowl)

# ===============================================
# match details... CURRENT > SEASONS > YYYY > CLUBS
# this contains team information
# not quite sure how this is useful yet

# ==== merge old and new years
# this is just a straight copy until the old data get loaded
B.batting <- E.batting
B.bowling <- E.bowling
B.fielding <- E.fielding
B.inningses <- E.inningses
B.batAvg <- E.batAvg.TY
B.bowlAvg <- E.bowlAvg.TY


# ==== Output data ====
# dump out for use by other scripts
save(E.batAvg.TY, file="./data/EbatAvgTY")
save(B.batAvg, file="./data/BbatAvg")
#write.csv(Z.club, file="./data/Zclub.csv")
save(B.batting, file="./data/Bbatting")
save(E.bowlAvg.TY, file="./data/EbowlAvgTY")
save(B.bowlAvg, file="./data/BbowlAvg")
save(B.bowling, file="./data/Bbowling")
save(E.dates, file="./data/Edates")
save(B.fielding, file="./data/Bfielding")
save(B.inningses, file="./data/Binningses")
save(E.matches, file="./data/Ematches")
save(E.players, file="./data/Eplayers")
save(E.playersNC, file="./data/EplayersNC")
