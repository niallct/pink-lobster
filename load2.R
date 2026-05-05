# new loader for the all-league data
# TODO figure out the best order for plucking captains, inns data to matches and unstacking
# ==== initialise ====
library(jsonlite)
library(yaml)
library(tidyverse)

# load the configs
conf <- yaml.load_file("./config/config.yaml")
conf$last_year <- as.character(as.integer(conf$year_of_interest) - 1)
A.inferredovers <- read_tsv("./config/compo-to-over.tsv", comment = "#",
                            col_types = list(competition_id="i", inferred_overs="i", infer_balls_per_over="i"))
A.captains <- read_tsv("./config/captains.tsv", col_type = list(year = "i", team_id = "i", `Nominal Captain` = "c"))
source("headers-def.R")

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

# a function to concatenate the how out lines
makeHowOut <- function(ho, bname="Unsure", fname="Unsure" ){
  case_when( #TODO implement ct & b
    ho %in% c("b") ~ paste(ho, bname),
    ho %in% c("lbw", "hit roof", "hit wicket") ~ paste(ho, "b", bname),
    ho %in% c("ct", "st") ~ paste(ho, fname, "b", bname),
    ho == "run out" ~ paste("run out", fname),
    ho %in% c("not out", "did not bat", "absent", "handled ball", "obstructing the field",
              "retired not out", "retired out", "timed out" ) ~ ho,
    TRUE ~ "" )
}

# some reference things
R.dismissed <- c("b", "ct", "handled ball", "hit roof", "hit wicket", "lbw",
                 "obstructing the field", "retired out", "run out", "st",
                 "timed out") # use this to filter how out into yes/no

R.nths <- c("first", "second", "third", "fourth", "fifth", "sixth",
            "seventh", "eighth","ninth", "tenth")

R.unusual_dismissals <- c("handled ball", "hit ball twice", "obstructing the field", "timed out", "hit wicket")

# a function to round averages
avRn <- function(x) floor(x*100)/100

# function for lowest x never made
ltnm <- function(foo, start=1) {min(setdiff(seq(start:max(foo, na.rm = TRUE)+1), pull(foo)))}

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
save(K.matches.all, file="./data/bigK.RData")
#load("./data/bigK.RData")

# ==== make friendlier data ==== 
# do this bit on the all matches for consistency and good things to filter
K.matches.all <- K.matches.all %>% mutate(
  actuallyDate = as.Date(match_date, "%Y-%m-%dT%H:%M:%S"),
  Date = format(actuallyDate, format = "%d %b %Y"),
  Yr = as.integer(format(actuallyDate, format = "%Y")), 
  `Day of Year` = yday(actuallyDate)
) %>% mutate(
  our_team_id = case_when(
    home_team_id %in% names(conf$our_teams_names) ~ home_team_id,
    away_team_id %in% names(conf$our_teams_names) ~ away_team_id,
    TRUE ~ NA),
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
         score = paste0(runs, "/", wickets),
         fullscore = paste0(team_batting_name, ": ", runs, "/", wickets, " (", overs, ")")
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

K.matches$result_match <- fct_relevel(K.matches$result_match, c("HW", "AW", "D", "T", "A", "C", "?", ""))
K.matches$result_club <- fct_relevel(K.matches$result_club, c("W", "L", "D", "T", "A", "C", "?", "n/a"))

# ---- results summarising ----
A.resultssumm <- as_tibble(K.matches %>% filter(is_circle) %>% select(Yr, result_club) %>% table()) %>%
  pivot_wider(names_from="result_club", values_from = "n") %>% mutate(P = rowSums(select(., -Yr))) %>%
  select(all_of(intersect(H.oxeye, names(.)))) #TODO this looks hacky, fix?

A.resultssummXI <- as_tibble(K.matches %>% filter(is_circle) %>%
                               select(Yr, result_club, `Our Team`) %>% table()) %>%
  pivot_wider(names_from="result_club", values_from = "n") %>% 
  mutate(P = rowSums(select(., -c(Yr, `Our Team`)))) %>%filter(P != 0) %>%
  select(all_of(intersect(H.oxeye.XI, names(.))))

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

K.matches <- K.matches %>% left_join(A.inferredovers, by = "competition_id") #TODO fix the many-many?

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

# ==== unstack the player data, pluck captains and keepers ====
#TODO think about a version for just the current season and last
#merge in the nominal captain for our matches
K.matches <- K.matches %>% left_join(A.captains, by=c("Yr" = "year", "our_team_id"="team_id"))

K.matchplayers <- K.matches %>% #select(-innings) 
  select(match_id, players, 
         `Match Summary`,
         `Home Club`, `Away Club`,
         `Home Side`, `Away Side`,
         home_team_name, away_team_name,
         Date, Type, Ground, `Result Text`,
         `Nominal Captain`, our_team_id, `Our Team`,
         home_club_id, away_club_id, actuallyDate, Yr,
         is_circle, is_sphere)  %>%
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

# ==== make lookups of competitions, grounds, leagues ====

Y.compos2 <- K.matches %>% select(competition_id, competition_name, league_id, 
                                  league_name, Type, is_circle, Yr) %>%
  drop_na(competition_id) %>%  distinct() %>% 
  summarise(.by = c(competition_id, competition_name, league_id, league_name, Type) ,
            RecentYear = max(Yr),
            current = RecentYear==conf$year_of_interest,
            ours = as.logical(max(is_circle))) #TODO add currentlyours

Y.grounds2 <- K.matches %>% select(ground_id, Ground) %>%
  rename(ground_name = Ground) %>% distinct() #TODO capture association to club, and whether current

Y.leagues2 <- K.matches %>% select(league_id, league_name, is_circle, Yr) %>% 
  drop_na(league_id) %>%  distinct() %>% 
  summarise(.by = c(league_id, league_name),
            RecentYear = max(Yr),
            current = RecentYear==conf$year_of_interest,
            ours = as.logical(max(is_circle))) #TODO add currentlyours

# ---- dataset size, start, end --------------------

K.dates <- list(
  our_latest = K.inningses %>% filter(is_circle == TRUE) %>%
    slice_max(actuallyDate, n = 1, with_ties = FALSE) %>% select(Date) %>% pull(Date),
  our_earliest = K.inningses %>% filter(is_circle == TRUE) %>% 
    slice_min(actuallyDate, n = 1, with_ties = FALSE) %>% select(Date) %>% pull(Date),
  all_latest = K.inningses  %>% 
    slice_max(actuallyDate, n = 1, with_ties = FALSE) %>% select(Date) %>% pull(Date),
  all_earliest = K.inningses %>%
    slice_min(actuallyDate, n = 1, with_ties = FALSE) %>% select(Date) %>% pull(Date),
  count_all = nrow(K.inningses),
  count_us = nrow(K.inningses %>% filter(is_circle==TRUE))
)

# ==== make tables of players from the matches they appear in ====

# a thing to check for duplicate names on one id -- this should return empty tibble
K.matchplayers %>% select(player_id, Name) %>% distinct() %>% 
  group_by(player_id) %>% add_count(Name) %>% filter(n>1)

# make a lookup for players without and with link to club ids
Y.plrs <- K.matchplayers %>% select(player_id, Name) %>% distinct()

Y.players2 <- K.matchplayers %>% select(player_id, Name, Yr) %>% distinct() %>% 
  summarise( .by =c(player_id, Name) , RecentYear = max(Yr))

Y.players.club2 <- K.matchplayers %>% select(player_id, Name, playing_club, Yr) %>% 
  distinct() %>% rename(club_id = playing_club) %>% 
  summarise( .by =c(player_id, club_id, Name) , RecentYear = max(Yr))

Y.players.current2 <- Y.players2 %>% filter(RecentYear >= conf$last_year)

# Identify possible cases of our duplicate people
A.duplicates <- Y.players.club2 %>%
  filter(club_id == conf$club_of_interest) %>%
  group_by(Name) %>%
  filter(n() > 1) %>%
  arrange(Name) %>%
  ungroup()

write.csv(A.duplicates, "output/dupes.csv")

# ---- our players and cap numbers ----
Y.players.ours2 <- Y.players.club2 %>% filter(club_id==conf$club_of_interest, !is.na(player_id))
write.csv(Y.players.ours2, file="output/playerslist.csv", row.names = FALSE)

Y.players.ourcurrent2 <- Y.ourplayers2 %>% filter(RecentYear >= conf$last_year)
write.csv(Y.players.ourcurrent2, file="output/players-recent.csv", row.names = FALSE)

Y.capnumbers2 <- K.matchplayers %>%  #F.fielding.us
  filter(!Name %in% c('Unsure', 'T.B.C'), playing_club==conf$club_of_interest) %>%
  group_by(player_id) %>% slice_min(actuallyDate, with_ties = FALSE) %>% ungroup() %>%
  arrange(actuallyDate) %>%
  select(Name, Date, `Debut Match` = `Match Summary`, `Debut Ground` = Ground) %>%
  tibble::rowid_to_column("Cap Number")

# ==== lookups for clubs and teams ====

Y.clubs2 <- bind_rows(K.matches %>% select(home_club_id, `Home Club`) %>% 
                   rename(club_id = home_club_id, 
                          club_name = `Home Club`), 
                 K.matches %>% select(away_club_id, `Away Club`) %>%
                   rename(club_id = away_club_id, 
                          club_name = `Away Club`)) %>% distinct() %>% drop_na()

Y.teams2 <-  rbind(K.matches %>% select(home_team_id, home_team_name, home_club_id, `Home Club`) %>% 
                    rename(team_id = home_team_id, 
                           team_name = home_team_name,
                           club_id = home_club_id,
                           club_name = `Home Club`), 
                  K.matches %>% select(away_team_id, away_team_name, away_club_id, `Away Club`) %>%
                    rename(team_id = away_team_id, 
                           team_name = away_team_name,
                           club_id = away_club_id,
                           club_name = `Away Club`)) %>%  distinct() %>% drop_na()


# ==== unnest and format innings data ====

K.inningses <- K.matches %>% select(-players) %>% unnest(innings) %>% 
  group_by(match_id) %>%
  mutate(`Inns of match` = row_number(),
         inns_this_match = max(row_number())) %>%
  ungroup() %>% 
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
  ) %>% 
  mutate(
    `Batting Team` = case_when(
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
    `Batting Captain` = case_when(
      batting_team_id == home_team_id ~ homecapt,
      batting_team_id == away_team_id ~ awaycapt,
      TRUE ~ NA
    ),
    `Fielding Captain` = case_when(
      batting_team_id == home_team_id ~ awaycapt,
      batting_team_id == away_team_id ~ homecapt,
      TRUE ~ NA
    ),
    `Keeper` = case_when(
      batting_team_id == home_team_id ~ awaykeeper,
      batting_team_id == away_team_id ~ homekeeper,
      TRUE ~ NA
    ),
    bat_first = case_when( #TODO do this better, somehow
      `Inns of match` == 1 ~ TRUE,
      TRUE ~ FALSE
    ),
    `Oppo Score` = case_when( #TODO sort this for 4-inns matches
      batting_team_id == team_batting_id_1 ~ score_2,
      batting_team_id == team_batting_id_2 ~ score_1
    )) %>% 
  mutate(
         `Runs/Wkt` = Total / W,
         Score = paste(Total, W, sep="/"),
         opptotal = as.integer(str_split_i(`Oppo Score`, "/", 1)) ,
         extras_proportion = Extras / Total,
         `% Extras` = paste(format(Extras * 100 / Total, digits = 1),"%"),
         Ven = as.factor(case_when(
           home_team_id == batting_team_id ~ "H",
           away_team_id == batting_team_id ~ "A",
           TRUE ~ as.character(NA)
         )),
         # competition_type = as.factor(competition_type),
         match_type = as.factor(match_type),
         Res = as.factor(case_when(
           result_match %in% c("A", "D", "T", "C")  ~ result_match,
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
         )  )

# reorder the results and venue factors
K.inningses$Res <- fct_relevel(K.inningses$Res, c("W", "L", "D", "T", "A"))
K.inningses$Ven <- fct_relevel(K.inningses$Ven, c("H", "A"))

# ---- batting --------
# unstack the individual batting performance data

K.batting <- K.inningses %>%
  select(-c(bowl,fow)) %>%
  rowwise() %>% 
  mutate(bat = list(as.data.frame(bat)) )%>% unnest(bat)

K.batting <- K.batting %>%
  rename(Pos = position,
         Name = batsman_name,
         "How Out" = how_out,
         Runs = runs,
         "4" = fours,
         "6" = sixes,
         Balls = balls
  ) %>%
  mutate(
    Team = `Batting Team`,
    Brys = as.integer(`4`) + as.integer(`6`),
    SR = ifelse(Balls == 0 & Runs != 0, NA, format(round(Runs * 100 / Balls, 2), nsmall = 2)),
    contribpc = ifelse(Total > 0 & Total >= Runs, Runs / Total, NA),
    Contrib = ifelse(Total == 0, NA, paste0(format((Runs * 100)/Total , digits = 1, scientific=FALSE),"%")),
    RunsWBF =  ifelse(is.na(Balls), NA, Runs),
    FullOut = makeHowOut(`How Out`, bowler_name, fielder_name)
  )

# ---- bowling ------ 
# unstack the individual bowling performance data

K.bowling <- K.inningses %>% # if we need to force the overs format, do it here
  select(-c(bat,fow)) %>%
  rowwise() %>% 
  mutate(bowl = list(as.data.frame(bowl)))%>% unnest(bowl) %>% 
  mutate(.by=c(match_id, `Inns of match`), bowlpos = row_number())

K.bowling <- K.bowling %>%
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
    Team = `Fielding Team`,
    Res = as.factor(case_when(
      Res %in% c("A", "T", "C", "D", "?") ~ Res,
      Res == "W" ~ "L", # yes, reverse this, because previously it was from perspective of batting team
      Res == "L" ~ "W",
      TRUE ~ NA
    )),
    Analy = paste(W, R, sep="-"),
    Avg = ifelse(W == 0, NA, avRn(R / W)),
    ovs.o = as.numeric(ifelse(is.na(str_split_i(O, "\\.", 1)),0,str_split_i(O, "\\.", 1))),
    ovs.b = as.numeric(ifelse(is.na(str_split_i(O, "\\.", 2)),0,str_split_i(O, "\\.", 2))),
    BB = ifelse(is.na(O), NA, (ovs.o * 6 ) + ovs.b + Wd + NB), #TODO tidy this
    SRx = ifelse(is.na(BB), NA, ifelse( BB == 0, NA, avRn(BB / W) ) ),
    SR = ifelse(is.na(SRx), NA, as.numeric(SRx)),
    Econ = ifelse((ovs.o + ovs.b/6) ==0, NA, as.numeric( avRn(R  / (ovs.o + ovs.b/6)))  ),
    ExtPc = ifelse(is.na(R), NA, avRn(100 * as.numeric( (Wd + NB) / R))),
    MdnPc = ifelse(is.na(O), NA, avRn(100 * as.numeric( M / O )))
  ) %>% select(-SRx)

# ---- fielding ------------
# pull our the individual fielding performance data from the batting table

A.fielddis <- K.batting %>%
  filter(`How Out` %in% c("ct", "st", "run out") & !is.na(fielder_id)) %>%
  summarise(.by = c(fielder_id, match_id, `Inns of match`),
            dis = n(),
            catches=sum(`How Out` == 'ct'),
            stumpings=sum(`How Out` == 'st'),
            runouts=sum(`How Out` == 'run out')
  )  %>%  arrange(desc(dis)) 

K.matchfielders <- K.matchplayers %>% #nb this was .fielding; field dis are per match not per inns
  left_join(A.fielddis %>% summarise(.by = c(fielder_id, match_id),
                                     catches=sum(catches),
                                     stumpings=sum(stumpings),
                                     runouts=sum(runouts)),
            by=c("match_id", "player_id"="fielder_id")) %>% 
  mutate(
    Ct = ifelse(is.na(catches), 0, catches),
    Std = ifelse(is.na(stumpings), 0, stumpings),
    RO = ifelse(is.na(runouts), 0, runouts),
    Dis = Ct + Std,
    "Batting Club" = case_when(
      team == "home" ~ `Away Club`,
      team == "away" ~ `Home Club`  ),
    "Fielding Club" =  case_when(
      team == "away" ~ `Away Club`,
      team == "home" ~ `Home Club`  ),
    "Batting Team" = case_when(
      team == "home" ~ away_team_name,
      team == "away" ~ home_team_name   ),
    "Fielding Team" =  case_when(
      team == "away" ~ away_team_name,
      team == "home" ~ home_team_name   ),
    "fielding_club_id" =  case_when(
      team == "away" ~ away_club_id,
      team == "home" ~ home_club_id   ),
    "batting_club_id" =  case_when(
      team == "home" ~ away_club_id,
      team == "away" ~ home_club_id   ),
    us_fielding = fielding_club_id == as.numeric(conf$club_of_interest),
  ) %>% select(-catches, -stumpings, -runouts)

# ---- fall-of-wicket --------------
#runs, wickets, batsman_out_name, batsman_out_id, batsman_in_name, batsman_in_id, batsman_in_runs

K.fow <- K.inningses %>%
  select(-c(bat, bowl)) %>%
  rowwise() %>%
  mutate(fow = list(as.data.frame(fow)) )%>% unnest(fow) %>% drop_na(runs) %>%
  mutate( Fall = paste(wickets, runs, sep="/"), is_no = FALSE) %>%
  rename( `Wkt` = wickets,
          `Batter Out` = batsman_out_name,
          `Batter Rem` = batsman_in_name
  ) %>%
  group_by(match_id, `Inns of match`) %>%
  mutate(Partnership = runs - lag(runs, default = 0)) %>% ungroup()

#the not-out partnerships at the end of an inns which already has fow
K.fow <- K.fow %>%
  group_by(match_id, `Inns of match`) %>% slice_max(Wkt) %>%
  filter(Wkt < (no_of_players - 1), (Wkt + 1)  > W, Total > 0) %>% #  suppress this if all out or no inns total
  mutate(
    Partnership = Total - runs, #TODO fix why this gives negative results?
    is_no = TRUE,
    runs = Total,
    Wkt = Wkt + 1,
    `Batter Out` = "*", #TODO collect the no-o batter name
    batsman_out_id = NA,
    Fall = ""  #was the score, but that's odd
  ) %>% rbind(K.fow) %>% ungroup()

#a not-out first wicket partnership - no existing useful fow table here 

zxc <- K.inningses %>% filter(W==0, Total > 0) %>% #select(match_id, , `Inns of match`, bat, Total) %>%
  hoist(bat, foo=2) %>% hoist(foo, bat1=1, bat2=2) %>% select(-foo, -bat, -bowl, -fow) %>%
  drop_na(bat1, bat2) %>%
  mutate(Fall = "",
         Wkt = 1,
         runs = Total, #yes we need this as well as partnership
         Partnership = Total,
         is_no=TRUE,
         batsman_in_id=NA,
         batsman_out_id=NA,
         batsman_in_runs=NA) %>%
  rename(`Batter Out` = bat1,
         `Batter Rem` = bat2) # TODO prolly check there's no duplictates when we rbind

K.fow <- K.fow %>% rbind(zxc) %>% mutate(
  Partnership = ifelse(Partnership <0, NA, Partnership), # remove nonsense
  Partnership = ifelse(Partnership > Total, NA, Partnership),
  PartnershipRaw = Partnership, 
  Partnership = as.character(ifelse(is_no == TRUE, paste(PartnershipRaw, "*", sep=""), PartnershipRaw))
)
rm(zxc)

# ---- interesting things, count them ----

intf1 <- K.batting %>% select(match_id, `Inns of match`, Runs, Balls, `How Out`) %>%
  group_by(match_id, `Inns of match`) %>% 
  summarise(topscore = ifelse(sum(!is.na(Runs))==0, 0, max(Runs, na.rm = TRUE)),
            botscore = ifelse(sum(!is.na(Runs))==0, 0, min(Runs, na.rm = TRUE)),
            numvalidbf = sum( !is.na(Balls) & 
                                (Balls > 0 |  (Balls == 0 & Runs == 0 & !`How Out` %in%  c("b", "ct", "lbw")) )  ),
            numbats = sum(!is.na(Runs)),
            numcaught = sum(`How Out`=="ct", na.rm=TRUE),
            numfielddis = sum(`How Out` %in% c('ct', 'st','run out'), na.rm=TRUE),
            numtens = ifelse(sum(!is.na(Runs))==0, 0, sum(Runs>=10, na.rm = TRUE)),
            numcenturies = ifelse(sum(!is.na(Runs))==0, 0, sum(Runs>=100, na.rm = TRUE)),
            numfifties = ifelse(sum(!is.na(Runs))==0, 0, sum(Runs>=50, na.rm = TRUE))
            )%>%  ungroup() # previously rj this back to batting, innings tables

intf2 <- K.bowling %>% select(match_id, `Inns of match`, W, R, M) %>%
  group_by(match_id, `Inns of match`) %>% 
  summarise(minwkts = ifelse(sum(!is.na(W)) < 2, 0, min(W, na.rm = TRUE)), # if one bowler, is dubious
            numfourfs = ifelse(sum(!is.na(W))==0, 0, sum(W>=4, na.rm = TRUE)),
            numfivefs = ifelse(sum(!is.na(W))==0, 0, sum(W>=5, na.rm = TRUE)),
            `Wicket-takers` = ifelse(sum(!is.na(W))==0, 0, sum(W>=1, na.rm = TRUE)),
            maxrunsccd = ifelse(sum(!is.na(R))==0, 0, max(R, na.rm = TRUE)),
            nummdns = sum(M),
            numbowlsmdn =  ifelse(sum(!is.na(M))==0, 0, sum(M>=1, na.rm = TRUE)),
            `Bowlers used` = n(),
            tbr = sum(R) # total bowlers runs, use this to remove partial scorecards
            )%>%   ungroup() 

xcv <- K.bowling %>% filter (O !="0") %>% 
  summarise(.by = c(match_id, `Inns of match`, Analy), BWIA = n()) %>%  
  slice_max(by = c(match_id, `Inns of match`), BWIA, with_ties = FALSE) %>%
  rename(`SharedAnaly` = Analy)

A.matchfeatures <- left_join(intf1, intf2, by=c("match_id", "Inns of match")) %>% 
  left_join(xcv, by=c("match_id", "Inns of match"))

rm(xcv, intf1, intf2) # tidy up

# ==== make fielding averages ====
makefieldsumm <- function(fielddata) {
  x <- fielddata %>%
    summarise(.by=c(player_id, Name, fielding_club_id),
              WKCt = sum(Ct[`W-K`]),
              OFCt = sum(Ct[`W-K`==F]),
              Ct = sum(Ct),
              Std = sum(Std), 
              RO = sum(RO), 
              WKM = sum(`W-K`),
              OFM = sum(`W-K`==F),
              M = n()) %>%
    mutate(dispermatch = (Ct + Std  + RO)/M,
           ctpermoutf = OFCt/OFM,
           ctpermwk = WKCt/WKM,
           `Dis/M` = avRn(dispermatch),
           `OF Ct/M` = avRn(ctpermoutf),
           `WK Ct/M` = avRn(ctpermwk)
    )
  
  return(x)
}

K.fieldsumm.ty <- K.matchfielders %>% filter(Yr == conf$year_of_interest) %>% 
  drop_na(Name)%>% makefieldsumm() 

K.fieldsumm <-  K.matchfielders %>% drop_na(Name) %>% 
  makefieldsumm()  

# ==== make batting averages ====
# the function #TODO tidy this up
makeBatAvgs <- function(batdata, mode='c') { #MODES: c group by player and club, p player only, a no grouping
  
  # batting avg table also includes fielding:
  if(mode=='c'){ adF <- batdata %>% group_by(fielder_id, fielding_club_id) }
  else  if(mode=='a'){adF <- batdata}
  else {adF <- batdata %>% group_by(fielder_id) }
  
  adamField <- adF %>% summarise(
    Ct = sum(`How Out` == "ct"),
    Std = sum(`How Out` == "st"),
    RO = sum(`How Out` == "run out")) %>%  ungroup()
  
  zxc <- batdata %>%  filter(!is.na(Runs)) %>%  mutate(RunsO = Runs) 
  
  if(mode=='c'){ zxc <- zxc %>% group_by(batsman_id, batting_club_id) }
  else  if(mode=='a'){zxc <- zxc}
  else {zxc <- zxc %>% group_by(batsman_id) }
  
  xcv <- zxc %>%  summarise(
    Runs = sum(Runs),
    Inns = sum(!is.na(RunsO)),
    NO = sum((`How Out` == "not out" | `How Out` == "retired not out") & !is.na(`How Out`)),
    Avg = avRn(Runs / (Inns - NO)),
    HSraw = max(RunsO),
    adsN = sum(RunsO == HSraw & `How Out` %in% c("not out", "retired not out")) ,
    HS = as.character(ifelse(adsN == 0, HSraw, paste(HSraw, "*", sep=""))),
    SR = avRn(sum(RunsWBF, na.rm = TRUE) * 100 / sum(Balls, na.rm = TRUE)),
    `50` = sum(RunsO >= 50 & RunsO < 100),
    `100` = sum(RunsO >= 100),
    BF = sum(Balls, na.rm = TRUE),
    `4s` = sum(`4`, na.rm = TRUE),
    `6s` = sum(`6`, na.rm = TRUE)  ) %>%
    ungroup() 
  
  if(mode=='c'){ xcv <- xcv %>%   left_join(adamField, by = c("batsman_id" = "fielder_id", 
                                                              "batting_club_id" = "fielding_club_id"))  %>%
    rename(club_id=batting_club_id) %>% 
    mutate( is_us = case_when(
      club_id == conf$club_of_interest ~ TRUE,
      club_id != conf$club_of_interest ~ FALSE)) %>% 
    left_join(Y.clubs) %>% rename(`Club` = club_name)}
  else if(mode=='a'){xcv <- xcv}
  else {xcv <- xcv %>%   left_join(adamField, by = c("batsman_id" = "fielder_id"))}
  
  if(mode!="a"){outputBatAvgs <- xcv %>%
    left_join(Y.plrs, by = c("batsman_id" = "player_id")) %>%
    mutate(
      Ct = replace_na(Ct, 0),
      Std = replace_na(Std, 0),
      RO = replace_na(RO,0)
    ) } else {outputBatAvgs <- xcv}
  
  return(outputBatAvgs)
}

K.batavg.ty <- K.batting %>% filter(Yr == conf$year_of_interest) %>% drop_na(Name) %>% makeBatAvgs()
K.batavg <- K.batting %>% drop_na(Name) %>% makeBatAvgs()

# ==== make bowling averages ====
# the function #TODO tidy this
makeBowlAvgs <- function(bowldata, mode="c") { #MODES: c group by player and club, *p player only, a no grouping
  
  if(mode=='c'){ zxc <- bowldata %>% group_by(bowler_id, fielding_club_id) %>%
    filter(!is.na(bowler_id))  }
  else  if(mode=='a'){zxc <- bowldata }
  else {zxc <- bowldata %>% group_by(bowler_id) %>%
    filter(!is.na(bowler_id)) }
  
  foo <- zxc %>%  slice_max(W) %>% 
    slice_min(R, n=1, with_ties = F) %>% ungroup()
  
  if(mode=='a'){adamBestBowl  <- foo %>% select(Analy)}
  else if(mode=='c'){adamBestBowl  <- foo %>% select(c(bowler_id, Analy, fielding_club_id))}
  else {adamBestBowl  <- foo %>% select(c(bowler_id, Analy))}
  
  xcv <- zxc %>%
    summarise(
      `5wi` = sum(W>=5),
      M = sum(M),
      R = sum(R),
      W = sum(W),
      BB = sum(BB, na.rm = TRUE),
      Avg = as.numeric(format(round(R / W, 2), nsmall = 2)),
      Econ = avRn((R * 6 / BB)),
      SR = avRn(BB / W),
      Inns = sum(!is.na(O)),
      O = paste(BB %/% 6, BB %% 6, sep=".")  ) %>%
    ungroup()
  
  if(mode=='c'){ outputbowlAvg <- xcv %>%
    left_join(adamBestBowl, by = join_by(bowler_id, fielding_club_id)) %>%
    rename(Best = Analy) %>%
    left_join(Y.plrs, by = c("bowler_id" = "player_id")) %>%
    rename(club_id=fielding_club_id) %>%
    mutate( is_us = case_when(
      club_id == conf$club_of_interest ~ TRUE,
      club_id != conf$club_of_interest ~ FALSE
    )) %>% left_join(Y.clubs) %>% rename(`Club` = club_name) }
  else {if(mode=='a'){outputbowlAvg <- xcv %>% cbind(adamBestBowl) %>% rename(Best = Analy)}
    else {outputbowlAvg <- xcv %>%
      left_join(adamBestBowl, by = join_by(bowler_id)) %>%
      rename(Best = Analy) %>%
      left_join(Y.plrs, by = c("bowler_id" = "player_id") ) } }
  
  return(outputbowlAvg)
}

K.bowlavg.ty <- K.bowling %>% filter(Yr == conf$year_of_interest) %>% drop_na(Name) %>% makeBowlAvgs()
K.bowlavg <- K.bowling %>% drop_na(Name) %>% makeBowlAvgs()

# ==== make the matches table to have match headers only ====

K.matches <- K.matches %>% select(-players, -innings) 

# ==== filtered sets for the club of interest ====
F.batting.us <- filter(K.batting, us_batting==TRUE)
F.batting.us.ty <- filter(K.batting, us_batting==TRUE, Yr == conf$year_of_interest)
F.batting.circle <- filter(K.batting, is_circle==TRUE) 
F.batting.sphere <- filter(K.batting, is_sphere==TRUE) 

F.batavg.us <- filter(K.batavg, club_id == conf$club_of_interest)
F.batavg.us.ty <- filter(K.batavg.ty, club_id == conf$club_of_interest)

F.fow.us <- filter(K.fow, us_batting==TRUE)
F.fow.us.ty <- filter(K.fow, us_batting==TRUE, Yr == conf$year_of_interest)
F.fow.circle <- filter(K.fow,is_circle==TRUE) 
F.fow.sphere <- filter(K.fow,is_sphere==TRUE) 

F.bowling.us <- filter(K.bowling, us_fielding==TRUE)
F.bowling.us.ty <- filter(K.bowling, us_fielding==TRUE, Yr == conf$year_of_interest)
F.bowling.circle <- filter(K.bowling,is_circle==TRUE) 
#F.bowling.circle.ty <- filter(K.bowling,is_circle==TRUE, Yr == conf$year_of_interest) 
F.bowling.sphere <- filter(K.bowling,is_sphere==TRUE) 
F.bowling.sphere.ty <- filter(K.bowling,is_sphere==TRUE, Yr == conf$year_of_interest) 

F.bowlavg.us <-  filter(K.bowlavg, club_id == conf$club_of_interest)
F.bowlavg.us.ty <- filter(K.bowlavg.ty, club_id == conf$club_of_interest)

F.inningses.us <- filter(K.inningses,us_batting==TRUE) 
F.inningses.us.ty <- filter(K.inningses,us_batting==TRUE, Yr == conf$year_of_interest) 
F.inningses.them <- filter(K.inningses,us_fielding==TRUE) 
F.inningses.them.ty <- filter(K.inningses,us_fielding==TRUE, Yr == conf$year_of_interest) 
F.inningses.circle <- filter(K.inningses,is_circle==TRUE) 
F.inningses.circle.ty <- filter(K.inningses,is_circle==TRUE, Yr == conf$year_of_interest) 
F.inningses.sphere <- filter(K.inningses,is_sphere==TRUE) 

F.fielding.us <- filter(K.matchfielders, us_fielding==TRUE) 
F.fielding.us.ty <- filter(K.matchfielders, us_fielding==TRUE, Yr == conf$year_of_interest) 
F.fielding.circle <- filter(K.matchfielders,is_circle==TRUE) 
F.fielding.sphere <- filter(K.matchfielders,is_sphere==TRUE) 

F.fieldsumm.us.ty <- filter(K.fieldsumm.ty, fielding_club_id == conf$club_of_interest) 
F.fieldsumm.us <- filter(K.fieldsumm, fielding_club_id == conf$club_of_interest) 

#F.joinavgs.us <- filter(B.joinAvgs, is_us==TRUE)

# F.allround.us <- filter(B.allround, is_us==TRUE)
# F.allround.us.ty <- filter(B.allround, is_us==TRUE, Yr == conf$year_of_interest)
# F.allround.circle <- filter(B.allround, is_circle==TRUE)
# F.allround.circle.ty <- filter(B.allround, is_circle==TRUE, Yr == conf$year_of_interest)
# F.allround.sphere <- filter(B.allround, is_sphere==TRUE)
# F.allround.sphere.ty <- filter(B.allround, is_sphere==TRUE, Yr == conf$year_of_interest)

F.matches.circle <- filter(K.matches,is_circle==TRUE) 
F.matches.circle.ty <- filter(K.matches,is_circle==TRUE,  Yr == conf$year_of_interest)
F.matches.sphere <- filter(K.matches,is_sphere==TRUE) 
F.matches.sphere.ty <- filter(K.matches,is_sphere==TRUE, Yr == conf$year_of_interest) 

# ==== write out useful tables ==== 
save(K.matches, K.inningses, K.matchplayers, K.batting, K.bowling, K.matchfielders, A.matchfeatures, file="./data/newcoredata.RData")
save(F.batting.circle, F.bowling.circle, F.matches.circle, F.inningses.circle, F.batavg.us, F.bowlavg.us, file="./data/parsed.RData") # for the quiz, a temp thing probably
# load("./data/coredata.RData")
