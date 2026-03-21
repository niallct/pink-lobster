# new loader for the all-league data
# TODO figure out the best order for plucking captains, inns data to matches and unstacking
# ==== initialise ====
library(jsonlite)
library(yaml)
library(tidyverse)

# load the config
conf <- yaml.load_file("./config/config.yaml")
conf$last_year <- as.character(as.integer(conf$year_of_interest) - 1)
A.inferredovers <- read_tsv("./config/compo-to-over.tsv", comment = "#",
                            col_types = list(competition_id="i", inferred_overs="i", infer_balls_per_over="i"))

# define what we want as match headers -- drop umpire names and such
H.chickpea <- c("match_id", "match_date", "match_time", "game_type", "competition_type", "match_type",
                "ground_id", "ground_name", "competition_id", "competition_name", "league_id", "league_name", 
                "home_team_id", "home_team_name", "home_club_id", "home_club_name", 
                "away_team_id", "away_team_name", "away_club_id", "away_club_name", 
                "no_of_overs", "batted_first", "no_of_players", "no_of_innings",
                "toss", "result", "result_description", "result_applied_to", "toss_won_by_team_id",
                "players", "innings") #old one is morning-glory.


# over translation functions 

getBallsFromOv <- function(x, bpo=6){ #TODO check input
  o <- as.numeric(str_split_i(x, "\\.", 1))
  b <- as.numeric(str_split_i(x, "\\.", 2))
  # if(b >= bpo) {warning("Balls component is greater than or equal to balls per over", immediate. = T)}
  b <- ifelse(is.na(b),0,b)
  balls = (o * bpo) + b
  return(balls)
}

getDecOvsFromOv <- function(x, bpo=6){
  return(as.numeric(getBallsFromOv(x,bpo) / bpo))
}

getOvsFromDecOvs <- function(x, bpo=6){
  paste(floor(x), round((x %% 1 ) * bpo), sep=".")
}

# ==== the big-all seasons data ====
# say where the data sits
parent_directory <- "input-data/all-seasons"
subdirectories <- list.dirs(parent_directory, full.names = TRUE)

# Loop through each subdirectory and find files called matches.json
json_files <- c() # an  empty vector to store the file paths
for (dir in subdirectories) {
  json_files <- c(json_files, list.files(dir, pattern = "matches.json", full.names = TRUE))
}

# Function to load and process a JSON file
load_json_file <- function(file) {
  jds <- fromJSON(file)
  if (is.null(jds$match_date)) {return(NULL)}
  # make a nice subset of interesting headings 
  #jds <- json_data[H.chickpea]
  return(jds)
}

#map that function on all the files we found then drop duplicates - sloooow
K.matches.all <- map(json_files, load_json_file) %>% compact() %>% 
  bind_rows() %>% select(all_of(H.chickpea)) %>% distinct(match_id, .keep_all = TRUE)

rm(dir, parent_directory, subdirectories, json_files, load_json_file) # tidy up!

# ==== make friendlier data ==== 
# do this bit on the all matches for consistency and good things to filter
K.matches.all <- K.matches.all %>% mutate(
  actuallyDate = as.Date(match_date, "%Y-%m-%dT%H:%M:%S"),
  Date = format(actuallyDate, format = "%d %b %Y"),
  Yr = as.integer(format(actuallyDate, format = "%Y")), 
  `Day of Year` = yday(actuallyDate)
) %>% mutate(
  is_circle = case_when(
    home_club_id == as.numeric(conf$club_of_interest) ~ TRUE,
    away_club_id == as.numeric(conf$club_of_interest) ~ TRUE,
    TRUE ~ FALSE),
  is_league = case_when(
    league_id %in% conf$leagues_of_interest ~ TRUE,
    TRUE ~ FALSE),
  is_sphere = case_when(
    is_circle == TRUE ~ TRUE,
    is_league == TRUE ~ TRUE,
    TRUE ~ FALSE),
  `Our Team` =  case_when(
    home_team_id %in% names(conf$our_teams_names) ~ home_team_name,
    away_team_id %in% names(conf$our_teams_names) ~ away_team_name,
    is_circle ~ "Other", # other means a team of ours but not one set as interesting
    TRUE ~ "") 
  ) %>%
  rename(
    Ground = ground_name,
    `Home Club` = home_club_name,
    `Away Club` = away_club_name,
    Result_ = result_description,
    #Winner = result_applied_to
    Type = competition_type
  ) %>%
  mutate(
   `Home Side` = paste(`Home Club`, home_team_name, sep= " - "),
   `Away Side` = paste(`Away Club`, away_team_name, sep= " - "),
   no_of_players = ifelse(is.na(no_of_players), 11, no_of_players)
  )
    
# filter out uninteresting matches
K.matches <- K.matches.all %>% 
  filter(game_type=="Standard",
         !home_team_id %in% conf$excluded_teams, 
         !away_team_id %in% conf$excluded_teams, 
         !league_id %in% conf$excluded_leagues,
         !match_id %in% conf$excluded_matches,
          result %in% c("A", "D", "T", "W"), # removes cancelled, conceded, in progress, replayed
          Yr >= conf$start_year)

# ==== unstack the innings data to get match summary out ====

K.inns.scores <- K.matches %>% select(-players) %>% unnest(innings) %>% 
  mutate(decimal_overs = getDecOvsFromOv(overs),
         score = paste(runs, "/", wickets, sep="" ),
         fullscore = paste(team_batting_name, ": ", runs, "/", wickets, " (", overs, ")", sep="" )
  ) %>% 
  group_by(match_id) %>%
  mutate(inns_of_match = row_number(),
         inns_this_match = max(row_number())) %>%
  ungroup() %>% 
  relocate(inns_of_match, .after="match_id")

K.matches.scores <- K.inns.scores %>% 
  pivot_wider(
    id_cols = c(match_id, inns_this_match), 
    names_from = inns_of_match,
    values_from = c(runs, wickets, score, fullscore, decimal_overs, team_batting_id, team_batting_name, total_extras),
    values_fn = first
  )  %>%
  unite(
    col = "Match Summary",
    starts_with("fullscore"),
    sep = "; ", na.rm = TRUE, remove = FALSE
  ) %>%
  mutate(`Match Aggregate` = rowSums(pick(starts_with("runs")), na.rm = TRUE),
         match_agg_dec_ovs = rowSums(pick(starts_with("decimal_overs")), na.rm = TRUE),
         `Match Overs` = getOvsFromDecOvs(match_agg_dec_ovs),
         `Match Aggregate Extras` = rowSums(pick(starts_with("total_extras")), na.rm = TRUE)
  )
# join just the interesting/useful things back again
K.matches <- K.matches.scores %>% 
  select(match_id, inns_this_match, starts_with("score"), starts_with("team_batting_id"), 
         starts_with("team_batting_name"),
         starts_with("decimal_overs"), starts_with("runs"), starts_with("wickets"),
         `Match Aggregate`, match_agg_dec_ovs, `Match Overs`, `Match Aggregate Extras`,
         `Match Summary`) %>%
  right_join(K.matches, by = "match_id")

# ==== result wrangling ====
K.matches <- K.matches %>% mutate(
  result = as.factor(result),
  result_match = as.factor(case_when(
    result_applied_to == home_team_id ~ "HW",
    result_applied_to == away_team_id ~ "AW",
    TRUE ~ result) ), # nb this includes conceded as wins -- may need to change that
  result_club = as.factor(case_when(
    is_circle == FALSE ~ "n/a",
    result_applied_to %in% names(conf$our_teams_names) ~ "W",
    result=="W" ~ "L", #TODO this breaks for hidden teams 162507, 198440
    TRUE ~ result_match)),
  win_toss_win_match = case_when(
    result_applied_to == toss_won_by_team_id ~ TRUE,
    result_applied_to != toss_won_by_team_id  ~ FALSE,
    TRUE ~ NA  ),
  `Our toss result` = as.factor(case_when(
    is_circle == FALSE ~ "n/a",
    toss_won_by_team_id %in% names(conf$our_teams_names) ~ "W",
    TRUE ~ "L")) , #TODO this breaks for hidden teams 162507, 198440
  bat_first_win = case_when(
    result_applied_to == team_batting_id_1 ~ TRUE,
    result_applied_to == team_batting_id_2 ~ FALSE,
    TRUE ~ NA))

# ==== the winning margin, result calculation - used to look at result_applied_to as well ====
K.matches <- K.matches %>% mutate(
  Margin = ifelse(inns_this_match == 2 & !is.na(runs_2) & result=="W", case_when(
    runs_1 > runs_2 ~ runs_1 - runs_2,
    runs_1 < runs_2 ~ no_of_players - 1 - wickets_2,
    TRUE ~ NA
  ), NA),
  Margin = ifelse(Margin < 1, NA, Margin),
  BatInnsWinner = case_when(
    is.na(result_applied_to) ~ NA,
    result_applied_to == team_batting_id_1 ~ 1,
    result_applied_to == team_batting_id_2 ~ 2,
    TRUE ~ NA
  ),
  `Full Result` = ifelse(is.na(Margin), NA, case_when( #TODO check this is actully winning team, fails on ?DLS
    runs_1 > runs_2 ~ 
      paste(team_batting_name_1,"win by", Margin, "runs"),
    runs_1 < runs_2 ~
      paste(team_batting_name_2 ,"win by", Margin, "wickets"),
    TRUE ~ NA
  )),
  `Result Text` = ifelse(is.na(`Full Result`), Result_, `Full Result`))  
  #select(match_id, `Match Summary`, `Full Result`, Margin, BatInnsWinner, Result_, result, `Result Text`)

# ==== try to complete the no_of_overs data ====

K.matches <- K.matches %>% left_join(A.inferredovers, by = "competition_id")

K.matches <- K.matches %>% rowwise() %>%
  mutate(guess_ovs = max(ceiling(0.2 * decimal_overs_1) / 0.2, 
                         ceiling(0.2 * decimal_overs_2) / 0.2,
                         0,
                         na.rm = TRUE),
        no_of_overs = case_when(is.na(no_of_overs) & !is.na(inferred_overs) ~ inferred_overs,
                                is.na(no_of_overs) & !is.na(guess_ovs) ~ guess_ovs,
                                TRUE ~ no_of_overs), 
        no_of_overs = replace_values(no_of_overs, 0 ~ NA))

# ==== tidy ground names ====

# make lookup from the config file
our_ground_lookup <- stack(conf$home_ground_names) %>% 
  rename(ground_nickname = values, ground_id = ind) %>%
  mutate(ground_id = as.numeric(as.character(ground_id)))

K.matches <- K.matches %>%
  left_join(our_ground_lookup, by = "ground_id") %>%
  mutate(Ground = coalesce(ground_nickname, Ground),
         Place = case_when(
           is.na(ground_id) ~ NA_character_,
           is.na(ground_nickname)     ~ "Other",
           TRUE             ~ ground_nickname
         )) %>%
  select(-ground_nickname)

# ====  unstack the player data, pluck captains and keepers ====
#TODO think about a version for just the current season and last
K.matchplayers <- K.matches %>% #select(-innings) 
  select(match_id, players, 
         `Match Summary`,
         `Home Club`, `Away Club`,
         `Home Side`, `Away Side`,
         Date, Type, Ground, `Result Text`,
         home_club_id, away_club_id)  %>%
  #select(players, match_id) %>% 
  mutate(  ht = list(pluck(players,1,1)),
           at = list(pluck(players,2,2))) %>% 
  #select(-players) %>% 
  filter_out(length(ht) == 0 & length(at) == 0) %>% 
  mutate(ht = ifelse(length(ht)==0,NA,list(cbind(team="home", ht))),
         at = ifelse(length(at)==0,NA,list(cbind(team="away", at))),
         stk = list(bind_rows("home" = ht, "away" = at, .id = "team"))) %>% 
  select(-ht, -at, -players) %>% unnest(stk) %>% 
  rename(Name = player_name, Position = position,
         Capt = captain, `W-K` = wicket_keeper) %>%
  mutate(    `Playing For` = case_when(
    team == "home" ~ `Home Club`,
    team == "away" ~ `Away Club`,
    TRUE ~ NA),
    playing_club = case_when(
      team == "home" ~ home_club_id,
      team == "away" ~ away_club_id,
      TRUE ~ NA)) %>%
  relocate(team, .after=`W-K`)

K.matchroles <- K.matchplayers %>%
  pivot_longer(
    cols = any_of(c("Capt", "W-K")),
    names_to = "role_name",
    values_to = "is_role"
  ) %>%
  filter(is_role == TRUE) %>%
  #mutate(role_label = if_else(role_name == "captain", "capt", "wk")) %>%
  pivot_wider(
    id_cols = match_id,
    names_from = c(team, role_name),
    values_from = Name,
    values_fn = first
  ) %>% 
  rename(homecapt = home_Capt,
         homekeeper = `home_W-K`,
         awaycapt = away_Capt,
         awaykeeper = `away_W-K`
         )

K.matches <- K.matches %>% 
  left_join(K.matchroles, by = "match_id")

K.matchplayers <- K.matchplayers %>% 
  left_join(K.matchroles, by = "match_id")

# ==== make lookups of competitions and leagues ====

Y.compos2 <- K.matches %>% select(competition_id, competition_name, 
                                       league_id, league_name,
                                  Type, no_of_overs) %>% distinct() 
#TODO add 'current' and 'ours' filters

Y.leagues2 <- K.matches %>% select(league_id, league_name) %>% distinct() 
#TODO add 'current' and 'ours' filters

# ==== make tables of players from the matches they appear in ====

# a thing to check for duplicate names on one id -- this should return empty tibble
K.matchplayers %>% select(player_id, Name) %>% distinct() %>% 
  group_by(player_id) %>% add_count(Name) %>% filter(n>1)

# make a lookup for players without and with link to club ids
Y.players2 <- K.matchplayers %>% select(player_id, Name) %>% distinct() 

Y.playersclub2 <- K.matchplayers %>% select(player_id, Name, playing_club) %>% 
  distinct() %>% rename(club_id = playing_club)

# ==== unnest innings data ====

K.inningses <- K.matches %>% select(-players) %>% unnest(innings) %>% 
  group_by(match_id) %>%
  mutate(inns_of_match = row_number(),
         inns_this_match = max(row_number())) %>%
  ungroup()

K.matches <- K.matches %>% select(-players, -innings) 

# ==== OLD: write out merged old/new years, and tidy up====
# save(B.matches, B.inningsesX, B.matchplayers, B.matchcompos, E.players, E.playersNC, file="./data/coredata.RData")
# load("./data/coredata.RData")

# ==== more parsing - initialise for that ====
R.dismissed <- c("b", "ct", "handled ball", "hit roof", "hit wicket", "lbw",
                 "obstructing the field", "retired out", "run out", "st",
                 "timed out") # use this to filter how out into yes/no

R.nths <- c("first", "second", "third", "fourth", "fifth", "sixth",
            "seventh", "eighth","ninth", "tenth")

R.unusual_dismissals <- c("handled ball", "hit ball twice", "obstructing the field", "timed out", "hit wicket")

## a function to round averages
avRn <- function(x) floor(x*100)/100

# function for lowest x never made
ltnm <- function(foo, start=1) {min(setdiff(seq(start:max(foo, na.rm = TRUE)+1), pull(foo)))}

