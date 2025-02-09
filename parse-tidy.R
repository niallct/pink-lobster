# ==== initialise ====
library(yaml)
library(tidyverse)

## load the config
conf <- yaml.load_file("./config/config.yaml")

## load in the prepared R objects from the 'load.R' script
load("./data/Bmatches")
load("./data/BinningsesX")
load("./data/Bmatchplayers")
load("./data/Bmatchcompos")
load("./data/Eplayers")
load("./data/EplayersNC")

# ==== some handy things to refer to ====
R.dismissed <- c("b", "ct", "handled ball", "hit roof", "hit wicket", "lbw",
                 "obstructing the field", "retired out", "run out", "st",
                 "timed out") # use this to filter how out into yes/no

## a function to round averages
avRn <- function(x) floor(x*100)/100
# was trunc(x*100, signif=2 )/100

conf$last_year <- as.character(as.integer(conf$year_of_interest) - 1)

R.nths <- c("first", "second", "third", "fourth", "fifth", "sixth",
            "seventh", "eighth","ninth", "tenth")

R.unusual_dismissals <- c("handled ball", "hit ball twice", "obstructing the field", "timed out", "hit wicket")

# ==== add club into the player match list ====
# don't forget this will get deleted later when it gets made into fielding table
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

B.inningses <- B.inningsesX %>%
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

rm(B.inningsesX)

B.inningses <- B.inningses %>%
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
    `Oppo Score` = case_when(
      batting_team_id == firstinnsbattid ~ secondinnsscore,
      batting_team_id == secondinnsbattid ~ firstinnsscore
    ))


B.inningses <- mutate(B.inningses, 
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

#read in who is supposed to be captain, then match
A.captains <- read_tsv("./config/captains.tsv", col_type = list(year = "i", team_id = "i", `Nominal Captain` = "c"))
B.inningses <- B.inningses %>% left_join(A.captains, by=c("Yr" = "year", "batting_team_id"="team_id"))

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

#a function to concatenate the how out lines

makeHowOut <- function(ho, bname="Unsure", fname="Unsure" ){
  case_when(
    ho %in% c("b") ~ paste(ho, bname),
    ho %in% c("lbw", "hit roof", "hit wicket") ~ paste(ho, "b", bname),
    ho %in% c("ct", "st") ~ paste(ho, fname, "b", bname),
    ho == "run out" ~ paste("run out", fname),
    ho %in% c("not out", "did not bat", "absent", "handled ball", "obstructing the field",
                     "retired not out", "retired out", "timed out" ) ~ ho,
    TRUE ~ "blah"    )
}

B.batting <- B.batting %>%
  mutate(
    Team = `Batting Team`,
    Brys = as.integer(`4`) + as.integer(`6`),
    SR = ifelse(Balls == 0 & Runs != 0, NA, format(round(Runs * 100 / Balls, 2), nsmall = 2)),
    contribpc = ifelse(Total == 0, NA, Runs / Total),
    Contrib = ifelse(Total == 0, NA, paste(format((Runs * 100)/Total , digits = 1),"%")),
    RunsWBF =  ifelse(is.na(Balls), NA, Runs),
    FullOut = makeHowOut(`How Out`, bowler_name, fielder_name)
    # FullOut = case_when(
    #   `How Out` %in% c("b") ~ paste(`How Out`, bowler_name),
    #   `How Out` %in% c("lbw", "hit roof", "hit wicket") ~ paste(`How Out`, "b", bowler_name),
    #   `How Out` %in% c("ct", "st") ~ paste(`How Out`, fielder_name, "b", bowler_name),
    #   `How Out` == "run out" ~ paste("ro", fielder_name),
    #   `How Out` %in% c("not out", "did not bat", "absent", "handled ball", "obstructing the field",
    #                    "retired not out", "retired out", "timed out" ) ~ `How Out`,
    #   TRUE ~ "blah"    )
  )

# interesting things in batting, count them

B.batting <- B.batting %>% select(match_id, `Inns of match`, Runs) %>%
  group_by(match_id, `Inns of match`) %>% 
  summarise(topscore = ifelse(sum(!is.na(Runs))==0, 0, max(Runs, na.rm = TRUE)),
            botscore = ifelse(sum(!is.na(Runs))==0, 0, min(Runs, na.rm = TRUE)),
            numbats = sum(!is.na(Runs)) ) %>% ungroup() %>% right_join(B.batting)

B.inningses <- B.batting %>% select(match_id, `Inns of match`, Runs, `How Out`) %>%
  group_by(match_id, `Inns of match`) %>% 
  summarise(topscore = ifelse(sum(!is.na(Runs))==0, 0, max(Runs, na.rm = TRUE)),
            botscore = ifelse(sum(!is.na(Runs))==0, 0, min(Runs, na.rm = TRUE)),
            numbats = sum(!is.na(Runs)),
            numcaught = sum(`How Out`=="ct", na.rm=TRUE),
            numfielddis = sum(`How Out` %in% c('ct', 'st','run out'), na.rm=TRUE),
            numtens = ifelse(sum(!is.na(Runs))==0, 0, sum(Runs>=10, na.rm = TRUE)),
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
    Team = `Fielding Team`,
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
B.inningses <- B.bowling %>% select(match_id, `Inns of match`, W, R, M) %>%
  group_by(match_id, `Inns of match`) %>% 
  summarise(minwkts = ifelse(sum(!is.na(W)) < 2, 0, min(W, na.rm = TRUE)), # if one bowler, is dubious
            numfourfs = ifelse(sum(!is.na(W))==0, 0, sum(W>=4, na.rm = TRUE)),
            numfivefs = ifelse(sum(!is.na(W))==0, 0, sum(W>=5, na.rm = TRUE)),
            `Wicket-takers` = ifelse(sum(!is.na(W))==0, 0, sum(W>=1, na.rm = TRUE)),
            maxrunsccd = ifelse(sum(!is.na(R))==0, 0, max(R, na.rm = TRUE)),
            nummdns = sum(M),
            `Bowlers used` = n()
          #  numfifties = ifelse(sum(!is.na(Runs))==0, 0, sum(Runs>=50, na.rm = TRUE))
            )%>% 
  ungroup() %>% right_join(B.inningses)

B.inningses <- B.bowling %>% group_by(match_id, `Inns of match`, Analy, fielding_club_id) %>%
                filter (O !="0") %>% arrange(desc(Analy)) %>%
                summarise(BWIA = n()) %>% ungroup() %>%  
            group_by(match_id, `Inns of match`, fielding_club_id) %>% slice_max(BWIA, with_ties = FALSE) %>%
                ungroup() %>% rename(`SharedAnaly` = Analy) %>% right_join(B.inningses)

 
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

E.fieldsumm <- B.fielding %>% filter(Yr == conf$year_of_interest) %>% makefieldsumm() %>% drop_na(Name)
  
B.fieldsumm <- B.fielding %>% makefieldsumm() %>% drop_na(Name)
          

# ---- fall-of-wicket --------------
#runs, wickets, batsman_out_name, batsman_out_id, batsman_in_name, batsman_in_id, batsman_in_runs

B.fow <- B.inningses %>%
  select(-c(bat, bowl)) %>%
  rowwise() %>%
  mutate(fow = list(as.data.frame(fow)) )%>% unnest(fow) %>% drop_na(runs) %>%
  mutate( Fall = paste(wickets, runs, sep="/")) %>%
  rename( `Wkt` = wickets,
          `Batter Out` = batsman_out_name,
          `Batter Rem` = batsman_in_name) %>%
  group_by(match_id, `Inns of match`) %>%
  mutate(Partnership = runs - lag(runs, default = 0)) %>% ungroup()

#the not-out partnerships at the end of an inns
B.fow <- B.fow %>%   group_by(match_id, `Inns of match`) %>% slice_max(Wkt) %>%
  mutate(
    Partnership = Total - runs,
    runs = Total,
    Wkt = Wkt + 1,
    `Batter Out` = "*",
    batsman_out_id = NA,
    Fall = Score
  ) %>% rbind(B.fow) %>% ungroup()


# ---- allrounder -----

# some sort of allrounder omnitable
# TODO figure out what to do in two-inns matches
# TODO explore full join to the fielding table

B.allround <- full_join(B.batting %>% filter(innings_number==1),
                 B.bowling%>% filter(innings_number==1),
                 by = c("match_id" = "match_id", "batsman_id" = "bowler_id",
                        "Match Summary", "Date", "Result", "Type", "Name", "actuallyDate" ),
                 keep = F, na_matches = "never", multiple = "first") %>%
  left_join(B.fielding %>% select(match_id, player_id, Ct, Std, Capt, "W-K"),
            by=c("match_id", "batsman_id" = "player_id"), keep=F, na_matches= "never" , multiple = "first") %>%
  select("Match Summary", "Date", "Result", "Type", "Name",
         player_id = batsman_id,
         "Ground" = "Ground.x", "Club" = "Batting Club.x", "Team" = "Team.x",
         "Res" = "Res.x",
         "Oppo" = "Batting Club.y",
         "Runs", "Balls", "BatSR" = "SR.x", "How Out", "4", "6", "Contrib",
         "O", "M", "R", "W" = "W.y", "Avg", "BowlSR" = "SR.y", "Econ", "Analy",
         "Ct", "Std", "Capt", "W-K", "is_us" = "us_batting.x",
         "is_circle" = "is_circle.x", "is_sphere" = "is_sphere.x", 
         "batting_club_id" = "batting_club_id.x", "Ven" = "Ven.x",
        # "fielding_club_id" = "fielding_club_id.y", 
         "Yr" = "Yr.x", "actuallyDate")


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


# ==== lookup tables ====

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
    league_id %in% B.matches[B.matches$Yr==conf$year_of_interest,]$league_id ~ TRUE,
    TRUE ~ FALSE),
    ours = case_when(
      league_id %in% B.matches[B.matches$is_circle==TRUE,]$league_id ~ TRUE,
      TRUE ~ FALSE)
  )

Y.compos <- B.matchcompos %>% select(competition_id, competition_name, competition_type, 
                                     no_of_overs, league_id, league_name) %>% distinct() %>%
  mutate(current = case_when(
    competition_id %in% B.matches[B.matches$Yr==conf$year_of_interest,]$competition_id ~ TRUE,
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



# ---- batting averages ------
# the function
makeBatAvgs <- function(batdata) {
  
  # batting avg table also includes fielding:
  adamField <- batdata %>%
    group_by(fielder_id, fielding_club_id) %>%
    summarise(
      Ct = sum(`How Out` == "ct"),
      Std = sum(`How Out` == "st"),
      RO = sum(`How Out` == "run out")
    ) %>%
    ungroup()
  
  outputBatAvgs <- batdata %>%
    filter(!is.na(Runs)) %>%
    mutate(RunsO = Runs) %>%
    group_by(batsman_id, batting_club_id) %>%
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
    left_join(adamField, by = c("batsman_id" = "fielder_id", 
                                "batting_club_id" = "fielding_club_id")) %>%
   left_join(E.playersNC, by = c("batsman_id" = "player_id")) %>%
    #select(-club_id) %>%
    rename(club_id=batting_club_id) %>%
    mutate( is_us = case_when(
      club_id == conf$club_of_interest ~ TRUE,
      club_id != conf$club_of_interest ~ FALSE),
        Ct = replace_na(Ct, 0),
        Std = replace_na(Std, 0),
        RO = replace_na(RO,0)
    ) %>% left_join(Y.clubs) %>% rename(`Club` = club_name)
    
  
  return(outputBatAvgs)
}

# subset just this year
#A.bat <- B.batting %>%
#  filter(Yr == conf$year_of_interest, !is.na(Runs))

A.bat <- B.batting %>%
  filter(Yr == conf$year_of_interest)

E.batAvg.TY <- makeBatAvgs(A.bat) %>% drop_na(Name)

# now all years
#A.bat <- B.batting %>%
#  filter(!is.na(Runs))

B.batAvg <- makeBatAvgs(B.batting) %>% drop_na(Name)

# Remove the temporary table
rm(A.bat)

# ---- bowling averages --------------------------------------------

# the function
makeBowlAvgs <- function(bowldata) {
  
  adamBestBowl <- bowldata %>% 
    select(c(bowler_id, R, W, Analy, fielding_club_id)) %>%
    filter(!is.na(bowler_id)) %>% 
    group_by(bowler_id, fielding_club_id) %>% 
    slice_max(W) %>% 
    slice_min(R, n=1, with_ties = F) %>%
    select(c(bowler_id, Analy))
  
  outputbowlAvg <- bowldata %>%
    group_by(bowler_id, fielding_club_id) %>%
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
      Inns = sum(!is.na(O)),
      O = paste(BB %/% 6, BB %% 6, sep="."),

    ) %>%
    ungroup() %>%
    left_join(adamBestBowl, by = join_by(bowler_id, fielding_club_id)) %>%
    rename(Best = Analy) %>%
    left_join(E.playersNC, by = c("bowler_id" = "player_id")) %>%
    rename(club_id=fielding_club_id) %>%
     mutate( is_us = case_when(
      club_id == conf$club_of_interest ~ TRUE,
      club_id != conf$club_of_interest ~ FALSE
    )) %>% left_join(Y.clubs) %>% rename(`Club` = club_name)
  
  return(outputbowlAvg)
}

# make a subset of just this year
A.bowl <- B.bowling %>%
  filter(Yr == conf$year_of_interest)

E.bowlAvg.TY <- makeBowlAvgs(A.bowl) %>% drop_na(Name)

# now all years
B.bowlAvg <- makeBowlAvgs(B.bowling) %>% drop_na(Name)

# Remove the temporary table
rm(A.bowl)


# ---- a joined avgs table ----

B.joinAvgs <- full_join(B.batAvg, B.bowlAvg, by = c("club_id", "batsman_id"="bowler_id"),
                        keep = F, na_matches = "never", relationship = "many-to-many") %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  select(Name= Name.x, 
         Runs, Inns = Inns.x, NO, BatAvg = Avg.x, HS, BatSR = SR.x, `50`, `100`, BF, `4s`, `6s`,
         Ct, Std, RO,
         O, M, R, W, BB, BwlAvg = Avg.y, `5wi`, Econ, BwlSR = SR.y, Best,
         club_id, is_us=is_us.x)

# ---- results summarising ----

H.oxeye <- c("Yr", "P", "W", "L", "D", "T", "A")

A.resultssumm <- as_tibble(B.matches %>% filter(is_circle) %>% select(Yr, result_club) %>% table()) %>%
  pivot_wider(names_from="result_club", values_from = "n") %>% mutate(P = rowSums(select(., -Yr))) %>%
  select(all_of(intersect(H.oxeye, names(.))))

H.oxeye.XI <- c("Yr", "Our Team", "P", "W", "L", "D", "T", "A")

A.resultssummXI <- as_tibble(B.matches %>% filter(is_circle) %>% select(Yr, result_club, `Our Team`) %>% table()) %>%
  pivot_wider(names_from="result_club", values_from = "n") %>% mutate(P = rowSums(select(., -c(Yr, `Our Team`)))) %>%
  filter(P != 0) %>%
  select(all_of(intersect(H.oxeye.XI, names(.))))


# === Identify possible cases of our duplicate people ====
duplicates <- Y.players %>%
  filter(player_club== conf$club_of_interest) %>%
  group_by(Name) %>%
  filter(n() > 1) %>%
  arrange(Name) %>%
  ungroup()

write.csv(duplicates, "dupes.csv")

# ==== Sets of headers to select the nicest things ====

## ---- match level ----
H.mallow <- c("Match Summary", "Date", "Result",
              "Type", "Ground", "Home Side", "Away Side",
              "Match Aggregate", "Match Overs")

H.phlox <- c("Match Summary", "Date", "Result",
             "Type", "Ground")

H.hyacinth <- c("Match Summary", "Date", "Result")

H.freesia <- c("Home Club", "Away Club", "Date", "Ground", "Match Aggregate",
                 "Match Overs")

## ---- innings level ----

H.impatiens <- c("Match Summary", "Date", "Result",
                 "Type", "Ground", #"Home Side", "Away Side",
                 "Batting Side", "Score", "Ovs",  "Fielding Side", "Oppo Score",
                 "Extras", "Byes" ,"Leg byes", "Wides", "No-balls", "% Extras", 
                 "Inns of match", 
                 "Res", "Ven")


H.dianthus  <- c("Batting Side", "Score", "Ovs", "Date", "Fielding Side", "Ground", "Res")
H.rudbeckia <- c("Batting Side", "Score", "Ovs", "Date", "Fielding Side", "Ground", "Res", "Extras")
H.wolfsbane <- c("Batting Side", "Score", "Ovs", "Date", "Fielding Side", "Ground", "Res", "Byes")
H.peony     <- c("Batting Side", "Score", "Ovs", "Date", "Fielding Side", "Ground", "Res", "Bowlers")


## ---- batting level ----
H.begonia <- c("Match Summary", "Date", "Result",
               "Type", "Ground", #"Home Side", "Away Side",
               "Batting Side", "Score", "Ovs",  "Fielding Side", "Oppo Score",
               #"Extras", "Byes" ,"Leg byes", "Wides", "No-balls", "% Extras", 
               "Inns of match", 
               "Res", "Ven",
               "Name", "Runs", "Balls", "SR", "How Out", "4", "6", "Contrib")

H.aspidistra <-  c("Name", "Runs", "Balls", "SR", "Date", "Type", "Fielding Club", "Ground", "Ven")
H.comfrey <-     c("Batting Side", "Runs", "Balls", "SR", "Date",         "Fielding Club", "Ground",       "Res")
H.lily <-        c("Name", "Runs", "Balls", "SR", "Date", "Team", "Fielding Club", "Ground",       "Res")
H.lotus <-       c("Name", "Runs", "Balls",       "Date", "Team", "Fielding Club", "Ground")
H.hibiscus <-    c("Name", "Runs", "Balls", "SR", "Date", "Team", "Fielding Club", "Ground", "4", "6")
H.safflower <-   c("Name", "Runs", "Balls", "SR", "Date", "Batting Club", "Fielding Club", "Ground", "How Out") 
H.thistle <-     c("Name", "Runs", "Balls", "SR", "Date",          "Fielding Club", "Oppo Score", "Contrib")

## ---- partnership ----

H.primrose <- c("Date", "Batting Team", "Fielding Side", "Wkt", "Partnership", "Batter Out", "Batter Rem", "Ven")
H.periwinkle <- c("Date", "Batting Team", "Fielding Side", "Wkt", "Partnership", "Batter Out", "Batter Rem", 
                  "Score", "Oppo Score")

## ---- bowling ----

H.lilac <- c("Match Summary", "Date", "Result",
             "Type", "Ground", #"Home Side", "Away Side",
             "Batting Side", "Score", "Ovs",  "Fielding Side", "Oppo Score",
             #"Extras", "Byes" ,"Leg byes", "Wides", "No-balls", "% Extras", 
             "Inns of match", 
             "Res", "Ven",
             "Name", "O", "M", "R", "W", "Avg", "SR", "Econ")

H.cyclamen <-  c("Name", "O", "M", "R", "W", "Date",  "Team", "Batting Club", "Ground", "Res")
H.buttercup <-  c("Fielding Side", "O", "M", "R", "W", "Date", "Batting Club", "Ground", "Res")
H.lupin <-      c("Name", "O", "M", "R", "W", "Date", "Team", "Batting Club", "Ground", "Wd", "NB")


## ---- fielding ----
H.foxglove <- c("Match Summary", "Date", "Result",
                "Type", "Ground", #"Home Side", "Away Side",
                # "Batting Side", "Score", "Ovs",  "Fielding Side", "Oppo Score",
                #"Extras", "Byes" ,"Leg byes", "Wides", "No-balls", "% Extras", 
               # "Inns of match", #TODO add these
              #  "Res", "Ven", ##TODO add these
                "Name", "Ct", "Std")

H.plum <-   c("Name", "Ct", "Std", "RO", "Date", "Fielding Club", "Batting Club", "Ground") 

## ---- allrounder ----
H.waterlily <- c("Match Summary", "Date", "Result",
                 "Type", "Ground", 
                 "Name", "Club", "Runs", "Balls", "BatSR", "How Out", "4", "6", "Contrib",
                 "O", "M", "R", "W", "Avg", "BowlSR", "Econ", "Ct", "Std", "Capt", "W-K") 

H.violet <- c("Match Summary", "Date",
              "Ground", 
              "Name", "Runs", "Balls", 
              "O", "M", "R", "W", "Ct", "Std") 


## ---- just playing ----
H.pansy <- c("Match Summary", "Date", "Result",
             "Type", "Ground", "Home Side", "Away Side",
             #could add firstinns and secondinns set if nicely renamed
             #"Match Aggregate", "Match Overs"
             "Position", "Name", "Capt", "W-K", "team", "Playing For")

# agapanthus, alyssum, amaranth, azalea, bluebell, buddleia, carnation, chrysanthemum, clover,  cowslip, crocus, daffodil, dahlia, daisy, delphinium, echinacea, feverfew, fuchsia, gardenia, geranium, gerbera, hollyhock, honeysuckle, hydrangea, ipomoea, iris, jasmine, lavender, lotus, magnolia, marigold, narcissus, nigella, orchid, pampas, poppy, snapdragon, snowdrop, sunflower, sweetpea,tulip, verbena, wisteria, zinna

## ---- averages and totals ----
H.aster <-     c("Name", "Runs", "Inns", "NO", "Avg", "HS", "SR", "50", "100", "Ct", "Std", "RO")
H.asterC <-     c("Name", "Club", "Runs", "Inns", "NO", "Avg", "HS", "SR", "50", "100", "Ct", "Std", "RO")
H.buckthorn <- c("Name", "Runs", "Inns", "NO", "Avg", "HS", "6s", "4s")
H.clematis <-  c("Name", "O", "M", "R", "W", "5wi", "Avg", "Econ", "SR", "Best")
H.clematisC <-  c("Name", "Club", "O", "M", "R", "W", "5wi", "Avg", "Econ", "SR", "Best")
H.petuina <- c("Name", "Runs", "BatAvg", "W", "BwlAvg", "Ct", "Std")
H.rose <- c("Name", "Season",  "Runs",   "Wkts", "Dis", "Matches")
H.crocosmia <-   c("Name", "M", "Ct", "Std", "RO", "Dis/Inns")
H.mugwort <- c("Name", "n")
H.mimosa <- c("Name", "n", "Innings")
H.yarrow <- c("Season", "Name", "Matches")
H.heather <- c("Season", "Name", "Total", "Matches")
H.heatherAv <- c("Season", "Name", "Avg", "Matches", "Wkts")
H.gladiolus <- c('Date', 'Batting Side', 'Fielding Side', 'Name', "Mode")

# ==== Pretty output tables ====

W.matches <- B.matches %>% select(all_of(H.mallow))
W.matchplayers <- B.fielding %>% select(all_of(H.pansy))
W.inningses <- B.inningses %>% select(all_of(H.impatiens))
W.batting <- B.batting %>% select(all_of(H.begonia))
W.bowling <- B.bowling %>% select(all_of(H.lilac))
W.fielding <- B.fielding %>% select(all_of(H.foxglove))
W.allround <- B.allround %>% select(all_of(H.waterlily))


# ==== filtered sets for the club of interest ====
F.batting.us <- filter(B.batting, us_batting==TRUE)
F.batting.circle <- filter(B.batting,is_circle==TRUE) 
F.batting.sphere <- filter(B.batting,is_sphere==TRUE) 

F.batting.us.ty <- filter(B.batting, us_batting==TRUE, Yr == conf$year_of_interest)

F.batavg.us <- filter(B.batAvg, club_id == conf$club_of_interest)
F.batavg.us.ty <- filter(E.batAvg.TY, club_id == conf$club_of_interest)

F.fow.us <- filter(B.fow, us_batting==TRUE)
F.fow.us.ty <- filter(B.fow, us_batting==TRUE, Yr == conf$year_of_interest)
F.fow.circle <- filter(B.fow,is_circle==TRUE) 
F.fow.sphere <- filter(B.fow,is_sphere==TRUE) 


F.bowling.us <- filter(B.bowling, us_fielding==TRUE)
F.bowling.circle <- filter(B.bowling,is_circle==TRUE) 
F.bowling.sphere <- filter(B.bowling,is_sphere==TRUE) 

F.bowling.us.ty <- filter(B.bowling, us_fielding==TRUE, Yr == conf$year_of_interest)
#F.bowling.circle.ty <- filter(B.bowling,is_circle==TRUE, Yr == conf$year_of_interest) 
F.bowling.sphere.ty <- filter(B.bowling,is_sphere==TRUE, Yr == conf$year_of_interest) 

F.bowlavg.us <-  filter(B.bowlAvg, club_id == conf$club_of_interest)
F.bowlavg.us.ty <- filter(E.bowlAvg.TY, club_id == conf$club_of_interest)

F.inningses.us <- filter(B.inningses,us_batting==TRUE) 
F.inningses.us.ty <- filter(B.inningses,us_batting==TRUE, Yr == conf$year_of_interest) 
F.inningses.them <- filter(B.inningses,us_fielding==TRUE) 
F.inningses.them.ty <- filter(B.inningses,us_fielding==TRUE, Yr == conf$year_of_interest) 
F.inningses.circle <- filter(B.inningses,is_circle==TRUE) 
F.inningses.circle.ty <- filter(B.inningses,is_circle==TRUE, Yr == conf$year_of_interest) 
F.inningses.sphere <- filter(B.inningses,is_sphere==TRUE) 

F.fielding.us <- filter(B.fielding, us_fielding==TRUE) 
F.fielding.us.ty <- filter(B.fielding, us_fielding==TRUE, Yr == conf$year_of_interest) 
F.fielding.circle <- filter(B.fielding,is_circle==TRUE) 
F.fielding.sphere <- filter(B.fielding,is_sphere==TRUE) 

F.fieldsumm.us.ty <- filter(E.fieldsumm, fielding_club_id == conf$club_of_interest) 
F.fieldsumm.us <- filter(B.fieldsumm, fielding_club_id == conf$club_of_interest) 

F.joinavgs.us <- filter(B.joinAvgs, is_us==TRUE)

F.allround.us <- filter(B.allround, is_us==TRUE)
F.allround.us.ty <- filter(B.allround, is_us==TRUE, Yr == conf$year_of_interest)
F.allround.circle <- filter(B.allround, is_circle==TRUE)
F.allround.circle.ty <- filter(B.allround, is_circle==TRUE, Yr == conf$year_of_interest)
F.allround.sphere <- filter(B.allround, is_sphere==TRUE)
F.allround.sphere.ty <- filter(B.allround, is_sphere==TRUE, Yr == conf$year_of_interest)


F.matches.circle <- filter(B.matches,is_circle==TRUE) 
F.matches.circle.ty <- filter(B.matches,is_circle==TRUE,  Yr == conf$year_of_interest)
F.matches.sphere <- filter(B.matches,is_sphere==TRUE) 
F.matches.sphere.ty <- filter(B.matches,is_sphere==TRUE, Yr == conf$year_of_interest) 

# ==== Output data ====
# zxc <- B.bowling %>% filter(Yr == conf$year_of_interest) 
# write.csv(zxc, file="./data/allbowl23.csv")

# ==== suggest-o-matic outputs ====
# A are things on the fly
# B are the BIG tables of data
# F are the FILTERED tables (this club, year)
# G is archive, E is this year only
# H are Headings
# R is reference things
# W are pretty lookuos, made in the Rmd
# Y are lookups for grounds etc
