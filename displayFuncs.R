# === FUNCTIONS FOR ALL ====

# This only provides definitions for a load of functions for displaying stats

# ==== column widths ====
# cake names for header widths #names.clubs.ground 9, dates 6
# apple,  barabrith, blackforest, , caterpillar,  cheesecake, 
# cupcake, fruitcake, stollen, jaffa, panettone, victoria, welsh

J.eccles <- function(x){x %>%
    column_spec(1, width = "9em")
}

J.banana <- function(x){x %>%
    column_spec(1:3, width="2em") %>%
    column_spec(4, width = "6em") %>%
    column_spec(5:7, width = "9em")
    #column_spec(8, width = "3em")
    }

# old phlox - match summaries
J.angel <- function(x){x %>%
    column_spec(1, width = "24em") %>%
    column_spec(2, width = "6em") %>%
    column_spec(3, width = "10em") %>%
    column_spec(5, width = "9em")
}

# old hyacinth - short match list
J.honey <- function(x){ x %>%
    column_spec(1, width = "25em") %>%
    column_spec(2, width = "6em") %>%
    column_spec(3, width = "12em")
}

# old freesia - match aggregate runs/ovs
# J.lamington <- function(x) {x %>%
#     column_spec(1:2, width = "10em") %>%
#     column_spec(3, width = "6em") %>%
#     column_spec(4, width = "9em")
# }
J.lamington <- function(x) {x %>%
    column_spec(1, width = "18em") %>%
    column_spec(2, width = "6em") %>%
    column_spec(3, width = "9em")
}

#old lily - batting performance
J.lemon <- function(x){ x %>%
    column_spec(1, width = "9em") %>%
    column_spec(5, width = "6em") %>%
    column_spec(6:8, width = "9em") 
}

J.carrot <- function(x){ x %>%
    column_spec(1, width = "9em") %>%
    column_spec(5, width = "6em") %>%
    column_spec(6, width = "4em") %>%
    column_spec(7:8, width = "9em") 
}

J.carrot2 <- function(x){ x %>%
    column_spec(2, width = "9em") %>%
    column_spec(6, width = "6em") %>%
    column_spec(7, width = "4em") %>%
    column_spec(8:9, width = "9em") 
}

J.chocolate <- function(x){ x %>%
    column_spec(1, width = "9em") %>%
    column_spec(5, width = "6em") %>%
    column_spec(6:7, width = "9em")
}
#old cyclamen - bowling performances
J.parkin <- function(x){ x %>%
    column_spec(1, width = "9em") %>%
    column_spec(6, width = "6em") %>%
    column_spec(7, width = "4em") %>%
    column_spec(8:9, width = "9em")
}

J.cupcake <- function(x){ x %>%
    column_spec(5, width = "6em") %>%
    column_spec(6:8, width = "9em")
}

#old dianthus - innings summary
J.madeira <- function(x) { x %>%
    column_spec(1, width = "12em") %>%
    column_spec(4, width = "6em") %>%
    column_spec(5:6, width = "12em")
}

#old primrose - partnerships
J.battenberg <- function(x) { x %>%
    column_spec(2, width = "6em") %>%
    column_spec(3, width = "4em") %>%
    column_spec(4, width = "9em") %>%
    column_spec(6:7, width = "9em") 
}

J.battenberg2 <- function(x) { x %>%
    column_spec(1, width = "6em") %>%
    column_spec(2, width = "4em") %>%
    column_spec(3, width = "9em") %>%
    column_spec(6:7, width = "9em") 
}

# old violet - all rounder
J.gingerbread <- function(x) { x %>%
    column_spec(1, width = "18em") %>%
    column_spec(2, width = "6em") %>%
    column_spec(3:4, width = "8em")
    }

# ==== batting ====
D.batavg.bestbatavgs <- function(df, cap, qual=conf$qualify_inns_avgs){df %>% 
    filter( Inns >= qual )%>% arrange(desc(Avg)) %>%
    select(all_of(H.aster)) %>%
    D.showtable(cap)}

D.batavg.mostruns <- function(df, cap, howmany=10000, qual=1){df %>%
    filter( Inns >= qual)%>%
    arrange(desc(Runs)) %>%
    slice_max(Runs, n=howmany, with_ties = FALSE) %>%
    select(all_of(H.aster)) %>%
    D.showtable(cap)}

W.bat.avgsteam <- function(theteamname, theteam, df, limit=0.5*conf$default_list_length) { df %>% 
    filter(batting_team_id == theteam ) %>% 
    makeBatAvgs() %>% drop_na(Name) %>%
    filter( Inns >= conf$qualify_inns_avgs)%>%
    slice_max(Avg, n=limit) %>%
    select(all_of(H.aster)) %>%
    mutate(Team = theteamname, .before=`Name`) }

W.bat.fiftiesteam <- function(theteamname, theteam, df, limit=0.5*conf$default_list_length) { df %>% 
    filter(batting_team_id == theteam ) %>% 
    makeBatAvgs() %>% drop_na(Name) %>%
   # filter( Inns >= conf$qualify_inns_avgs)%>%
    arrange(desc(Runs)) %>%
    slice_max(`50`, n=limit) %>%
    select(all_of(H.aster)) %>%
    mutate(Team = theteamname, .before=`Name`) }

# D.bat.totalrunsbytype <- function(df, cap, thistype="League") {df %>%
#     select(batsman_id, Runs, Type, Name) %>%
#     group_by(Type, batsman_id, Name) %>% summarise(n = sum(Runs, na.rm = TRUE)) %>%
#     ungroup() %>% group_by(Type) %>% 
#     slice_max(n, n = conf$default_list_length) %>% filter(Type==thistype) %>%
#     ungroup() %>%
#     select(-batsman_id, -Type)  %>%
#     arrange(desc(n)) %>% 
#     select(all_of(H.mugwort)) %>%
#     D.showtable(paste(cap, thistype))}

W.bat.totalrunsbytype <- function(df, thistype="League", min=35) {df %>%
    select(batsman_id, Runs, Type, Name) %>%
    group_by(Type, batsman_id, Name) %>% summarise(n = sum(Runs, na.rm = TRUE)) %>%
    ungroup() %>% group_by(Type) %>% 
    slice_max(n, n = conf$default_list_length) %>% 
    filter(Type==thistype, n>=min) %>%
    ungroup() %>%
    select(-batsman_id, -Type)  %>%
    arrange(desc(n)) %>% 
    select(all_of(H.mugwort)) %>%
    mutate(Type = thistype, .before=`Name`) }

W.bat.lowavgmanyruns <- function(df, min=1000) {df %>% 
  filter(Runs >= min) %>% arrange(Avg) %>%
  select(all_of(H.aster))}

D.bat.lowavgmanyruns <- function(df, cap, min=1000){df %>%
    W.bat.lowavgmanyruns(min=min) %>%
    D.showtable(cap)}

D.bat.fifties <- function(df, cap){df %>% filter(Runs>=50) %>%
    arrange(desc(Runs)) %>%  
    select(all_of(H.lily)) %>%
    D.showtable(cap) %>% J.carrot}

D.bat.fifties2 <- function(df, cap){df %>% filter(Runs>=50) %>%
    arrange(desc(Runs)) %>%  
    select(all_of(H.comfrey)) %>%
    D.showtable(cap) %>% J.chocolate}

D.bat.milestone <- function(df, cap, ms=50){df %>% filter(Runs>=ms) %>%
    arrange(desc(Runs)) %>%  
    select(all_of(H.lily)) %>%
    D.showtable(cap) %>% J.carrot}

D.bat.fiftieslosing <- function(df, cap, ms=50){df %>% filter(Runs>=ms & Res!="W") %>%
    slice_max(actuallyDate, n=conf$default_list_length) %>%
    select(all_of(H.lily)) %>%
    D.showtable(cap) %>% J.carrot}

D.bat.fiftiestailend <- function(df, cap, ms=50){df %>%
    filter(Runs >=ms & Pos %in% c(9,10,11) ) %>% 
    #slice_max(Runs, n=conf$default_list_length) %>%
    select(all_of(H.lily)) %>%
    D.showtable(cap) %>% J.carrot}

# D.bat.batmsteam <- function(df, cap, ms=50, team, limit=conf$default_list_length) { df %>% 
#     filter(Runs >=ms & batting_team_id == team ) %>% 
#     slice_max(actuallyDate, n=limit) %>%
#     select(all_of(H.lily)) %>%
#     D.showtable(paste(cap, "for", names(which(conf$teams_of_interest == team)))) %>% 
#     J.carrot}

W.bat.msteam <- function(theteamname, theteam, df, ms=50, limit=conf$default_list_length) { df %>% 
    filter(Runs >=ms & batting_team_id == theteam ) %>% 
    slice_max(actuallyDate, n=limit) %>%
    select(all_of(H.lily)) %>%
    mutate(Team = theteamname, .before=`Name`) }

D.bat.batmsdebut <- function(df, cap, ms=50){df %>%
    filter(!is.na(Runs)) %>%
    group_by(batsman_id, Name) %>%  slice_min(actuallyDate, n=1) %>% ungroup() %>%
    filter(Runs>=ms) %>%
    arrange(desc(actuallyDate)) %>%
    select(all_of(H.lily)) %>%
    D.showtable(cap) %>% J.carrot}

D.bat.consecbatms <- function(df, cap, ms=50) {df%>%
    select(batsman_id, actuallyDate, Runs, Yr, batting_club_id,
                           Name, Balls, SR, Date, `Fielding Club`, Ground, Ven, 
                           Team, Res) %>%
  filter(!is.na(Runs)) %>%
  arrange(batsman_id, actuallyDate) %>%  # Sort by player and date
  group_by(batsman_id) %>%  # Group by player
  mutate(
    milestone = Runs >= ms,  # Flag if runs >= 100
    next_match_is_milestone = lead(milestone, default = FALSE)  # Check if next match is also 100+
  ) %>%
  filter(milestone & (next_match_is_milestone | lag(milestone, default = FALSE))) %>%  # Keep both current and lag match if they are 100+
  ungroup() %>% arrange(Name, desc(actuallyDate))%>%
    select(all_of(H.lily)) %>%
    D.showtable(cap) %>% J.carrot}

# D.bat.highestscorebytype <- function(df, cap, thistype="League", min=35) {df %>%
#   filter(Type == thistype, Runs >=min) %>%
#     slice_max(Runs, n=conf$default_list_length) %>%
#     select(all_of(H.lily)) %>%
#     D.showtable(paste(cap, thistype, "matches")) %>% J.carrot}

W.bat.highestscorebytype <- function(df, thistype="League", min=35) {df %>%
    filter(Type == thistype, Runs >=min) %>%
    slice_max(Runs, n=conf$default_list_length) %>%
    select(all_of(H.lily)) %>%
    mutate(Type = thistype, .before=`Name`) }

# D.bat.highestscorebyteam <- function(df, cap, team, min=35) {df %>%
#     filter(batting_team_id == team , Runs >=min) %>%
#     slice_max(Runs, n=conf$default_list_length) %>%
#     select(all_of(H.lily)) %>%
#     D.showtable(paste(cap, "for", names(which(conf$teams_of_interest == team)))) %>%
#     J.carrot}

W.bat.highestscorebyteam <- function(theteamname, theteam, df, min=35) {df %>%
    filter(batting_team_id == theteam , Runs >=min) %>%
    slice_max(Runs, n=0.5*conf$default_list_length) %>%
    select(all_of(H.lily)) %>%
    mutate(Team = theteamname, .before=`Name`) }

D.bat.highestscore <- function(df, cap, min=35) {df %>%
    filter(Runs >=min) %>%
    slice_max(Runs, n=conf$default_list_length) %>%
    select(all_of(H.lily)) %>%
    D.showtable(cap) %>% J.carrot}

D.bat.longinns <- function(df, cap, min=1000){df %>%
  filter(Runs <= min) %>% slice_max(Balls, n=conf$default_list_length) %>%
    select(all_of(H.lily)) %>%
    D.showtable(cap) %>% J.carrot}

D.bat.mostballsfewestruns <- function(df,cap) {df %>% 
  filter(numvalidbf == numbats, Res!="A", Balls > 1) %>% #the >1 is still a fudge
  group_by(match_id, `Inns of match`) %>%
  slice_max(Balls) %>%
  ungroup() %>% slice_min(Runs, n=0.5*conf$default_list_length) %>%
    select(all_of(H.lily)) %>%
    D.showtable(cap) %>% J.carrot}

D.bat.fewestballsmostruns <- function(df,cap) {df %>% 
    filter(numvalidbf == numbats, Res!="A", Balls > 1) %>% #the >1 is still a fudge
    group_by(match_id, `Inns of match`) %>%
    slice_min(Balls) %>%
    ungroup() %>% slice_max(Runs, n=0.5*conf$default_list_length) %>%
    select(all_of(H.lily)) %>%
    D.showtable(cap) %>% J.carrot}

#Most runs by batter who faced most balls of the Innings is not as interesting

D.batavg.highSR <- function(df, cap){ df %>%
    filter(!is.na(Name) & BF >= conf$qualify_balls_faced) %>%
    slice_max(SR, n=conf$default_list_length)%>%
    select(all_of(H.aster)) %>%
    D.showtable(cap)}
    
D.bat.highSRinns <- function(df, cap, min=35){ df %>%
    filter(Runs >= min) %>%
    slice_max(SR, n=conf$default_list_length) %>%
    select(all_of(H.lily)) %>%
    D.showtable(cap) %>% J.carrot}

D.bat.highSRcent <- function(df, cap){ df %>%
  filter(Runs>100) %>% arrange(desc(SR)) %>% select(all_of(H.begonia)) %>%
  D.showtable(cap) %>% J.carrot}

D.bat.most6inns <- function(df, cap){df %>%
      slice_max(`6`, n=conf$default_list_length) %>%
    select(all_of(H.hibiscus)) %>%
    D.showtable(cap) %>% J.carrot}



D.bat.mostBryinns <- function(df, cap, min=12){df %>%
     W.bat.mostBryinns(min) %>%
     D.showtable(cap)%>% J.carrot}

W.bat.mostBryinns <- function(df, min=12){df %>%
    filter((`6` +`4`) >= min) %>%
      slice_max((`6`+`4`), n=0.5*conf$default_list_length) %>%
      select(all_of(H.hibiscus)) } 
 
 
 # W.bat.highestscorebyteam <- function(theteamname, theteam, df, min=35) {df %>%
 #     filter(batting_team_id == theteam , Runs >=min) %>%
 #     slice_max(Runs, n=0.5*conf$default_list_length) %>%
 #     select(all_of(H.lily)) %>%
 #     mutate(Team = theteamname, .before=`Name`) }
 # 
 
# D.bat.mostBryinnsteam <- function(df, cap, team) { df %>% 
#     filter(batting_team_id == team ) %>% 
#     D.bat.mostBryinns(paste(cap, "for", names(which(conf$teams_of_interest == team))))}

W.bat.mostBryinnsteam <- function(theteamname, theteam, df, min=12) { df %>% 
    filter(batting_team_id == theteam ) %>% 
    W.bat.mostBryinns(min)}


D.batavg.centurycount <- function(df, cap, min=1){df %>%
  filter(!is.na(Name)) %>%
    filter(`100` >= min) %>%
      slice_max(`100`, n=conf$default_list_length) %>%
    select(all_of(H.aster)) %>%
    D.showtable(cap)}


D.bat.primaries <- function(df, cap){df%>% 
    filter(Runs == 0  & Balls<= 1 & `How Out` %in% R.dismissed) %>%
    arrange(desc(actuallyDate)) %>%
    select(all_of(H.lily)) %>%
    D.showtable(cap) %>% J.carrot}

# D.duckcount2 <- function(df, cap){df %>%
#     filter(Name!="Unsure", Runs==0, `How Out` %in% R.dismissed ) %>%
#     group_by(Name) %>% tally() %>% slice_max(n, n=conf$default_list_length) %>%
#     H.F.plain(cap, H.mugwort)}

D.bat.duckcount <- function(df, cap){ df %>%
 filter(Name!="Unsure", `How Out` %in% R.dismissed, !is.na(Runs)) %>%
  group_by(batsman_id, Name) %>%
  summarise(n = sum(Runs==0), Innings = sum(!is.na(Runs))) %>% ungroup() %>%
  slice_max(n, n=conf$default_list_length) %>%
    select(all_of(H.mimosa)) %>%
    D.showtable(cap)}

# D.bat.primarycount <- function(df, cap){df %>%
#     filter(Name!="Unsure", Balls <=1,  Runs==0, `How Out` %in% R.dismissed ) %>%
#     group_by(Name) %>% tally() %>% slice_max(n, n=conf$default_list_length) %>%
#     H.F.plain(cap, H.mugwort)}

D.bat.ducklength <- function(df, cap){df %>%
    filter( Name!="Unsure", Runs==0, `How Out` %in% R.dismissed, Balls >=6) %>%
    slice_max(Balls, n=conf$default_list_length) %>% 
    select(all_of(H.lily), `How Out`) %>%
    D.showtable(cap) %>% J.carrot}

D.batavg.sixcount <- function(df, cap){ df %>%
  filter(!is.na(`4s`) & !is.na(`6s`)) %>%
    arrange(desc(`6s`), desc(`4s`)) %>%
    slice_max(`6s`, n=conf$default_list_length) %>%
    select(all_of(H.buckthorn)) %>%
    D.showtable(cap)}

D.bat.carriedbat <- function(df, cap) {df %>%
    filter(Pos %in% c("1","2"), `How Out`=="not out", Res %in% c('W', 'L')) %>% 
    arrange(desc(Runs)) %>%
    slice_max(actuallyDate, n=conf$default_list_length) %>%
    select(all_of(H.lily)) %>%
    D.showtable(cap) %>% J.carrot}

D.bat.tailendtop <- function(df, cap) {df %>%
    filter(Pos %in% c("10", "11"), Runs==topscore) %>% 
    arrange(desc(Runs)) %>%
    slice_max(actuallyDate, n=conf$default_list_length) %>%
    select(all_of(H.lily)) %>%
    D.showtable(cap) %>% J.carrot}

D.bat.topscorercount <- function(df, cap){df %>%
    filter(Name!="Unsure", Runs == topscore) %>%
    group_by(Name) %>% tally() %>% slice_max(n, n=conf$default_list_length) %>%
    select(all_of(H.mugwort)) %>%
    D.showtable(cap)}

D.batavg.nearms <- function(df, cap, myclub) {df %>%
  filter(club_id == myclub & !is.na(Name) &
                      batsman_id %in% Y.currentplayers$player_id) %>% 
  filter((Runs >= 400 & Runs < 500) | (Runs >= 900 & Runs < 1000) |
         (Runs >= 1900 & Runs < 2000)) %>%
  arrange(desc(Runs)) %>%
    select(all_of(H.aster)) %>%
    D.showtable(cap)}

D.bat.maybatms <- function(df, cap, ms=200){df %>%
  filter(months(actuallyDate) %in% c("April", "May") ) %>%
  group_by(batsman_id, Name, Yr) %>%
  summarise(TRuns = sum(Runs), `Matches` = n()) %>% ungroup() %>% drop_na() %>%
  select(-batsman_id) %>% rename(Season = Yr, Total = TRuns) %>%
  filter(Total >= ms) %>% arrange(desc(Total)) %>%
    select(all_of(H.heather)) %>%
    D.showtable(cap)}

D.bat.500season <- function(df, cap, ms=500){df %>%
    filter(!is.na(Runs), Name !="Unsure") %>%
    group_by(batsman_id, Yr, Name) %>% summarise(`Total` = sum(Runs), `Matches` = n()) %>% 
    ungroup() %>% #FIXME this is probably innings, not matches, but need to strip out NAs
    rename(Season = Yr) %>% select(-batsman_id) %>% 
    filter(Total >= ms) %>% arrange(desc(Total)) %>%
    # slice_max(Total, n=conf$default_list_length) %>% 
    select(all_of(H.heather)) %>%
    D.showtable(cap)}

D.bat.nobdry.season <- function(df, cap){df %>%
    filter(!is.na(`4`), Name !="Unsure") %>%
    group_by(batsman_id, Yr, Name) %>% summarise(`Total` = sum(`4` + `6`), `Matches` = n()) %>% 
    ungroup() %>% #FIXME this is probably innings, not matches, but need to strip out NAs
    filter(Total == 0) %>% arrange(desc(Yr)) %>%
    rename(Season = Yr) %>% select(-batsman_id) %>% 
    slice_max(Matches, n=conf$default_list_length, with_ties = FALSE) %>%
    select(all_of(H.heather)) %>%
    D.showtable(cap)}

D.bat.quickest500 <- function(df, cap, ms=500){ df %>%
    filter(!is.na(Runs), !is.na(batsman_id)) %>%
    select(Yr, actuallyDate, batsman_id, Name, Runs) %>% 
    group_by(Yr, batsman_id) %>%     # Group by year and player
    arrange(actuallyDate) %>%              # Sort by date within each player
    mutate(cumulative_runs = cumsum(Runs)) %>%  # Calculate cumulative runs for each player
    mutate(`Date Reached` = format(ymd(actuallyDate), "%e %b"),
           `Day of Year` = yday(actuallyDate)) %>%
    filter(cumulative_runs >= ms) %>%  # Filter players who reach 500 runs
    slice(1) %>%  # Get the first match for each player to reach 500
    ungroup() %>%
    arrange(`Day of Year`) %>%
    slice_min(`Day of Year`, n=conf$default_list_length) %>%
    select(Name, `Date Reached`, `Yr`) %>%
    D.showtable(cap)}

D.bat.most50season <- function(df, cap, ms=50){ df %>%
    filter( !is.na(Runs)) %>%
    group_by(batsman_id, Yr, Name) %>% summarise(`Total` = sum(Runs>=ms), `Matches` = n()) %>% ungroup() %>%
    rename(Season = Yr) %>% select(-batsman_id) %>% 
    slice_max(Total, n=conf$default_list_length) %>% 
    select(all_of(H.heather)) %>%
    D.showtable(cap)}

D.bat.mostbryseason <- function(df, cap){df %>%
    filter(!is.na(`4`), !is.na(`6`)) %>%
    group_by(batsman_id, Yr, Name) %>% summarise(`Total` = sum(`4`) + sum(`6`), `Matches` = n()) %>% ungroup() %>%
    rename(Season = Yr) %>% select(-batsman_id) %>% 
    slice_max(Total, n=conf$default_list_length) %>% 
    select(all_of(H.heather)) %>%
    D.showtable(cap)}
    
# D.bat.mostbryseason2 <- function(df, cap){  df %>%
#     filter(!is.na(`4`), Name !="Unsure") %>%
#     group_by(batsman_id, Yr, Name) %>% summarise(`Total` = sum(`4`), `Matches` = n()) %>% 
#     ungroup() %>% #FIXME this is probably innings, not matches, but need to strip out NAs
#     rename(Season = Yr) %>% select(-batsman_id) %>% 

D.bat.bannerman <- function(df, cap, ms=0.6, min=10){df %>%
    filter(contribpc >= ms & Runs >= min) %>% arrange(desc(contribpc)) %>%
    select(all_of(H.thistle)) %>%
    D.showtable(cap) %>% J.carrot}

D.bat.outscoringoppo <- function(df, cap){ df %>%
    filter(Runs >= opptotal, opptotal != 0) %>% arrange(desc(actuallyDate)) %>%
    select(all_of(H.thistle)) %>%
    D.showtable(cap) %>% J.carrot}

D.bat.outscoringotherteam <- function(dfinns, dfbat, cap, howmany=conf$default_list_length) {
  tobeat <- dfinns %>% filter(Total > 0, Res!="A") %>% group_by(`actuallyDate`)%>% 
    slice_min(Total) %>%
    select(`Our Team`, actuallyDate, Total, Score) %>% ungroup()
  
  scores <- dfbat %>% filter(Name !='Unsure') %>% select(Name, actuallyDate, Runs, `Our Team`, Date)
  
  full_join(tobeat, scores, by='actuallyDate') %>% filter(Runs > Total) %>%
    arrange(desc(actuallyDate)) %>%
    slice_max(actuallyDate, n=howmany, with_ties = F) %>%
    select(Name, `Batting for` = `Our Team.y`, Runs, 
           `Team outscored` = `Our Team.x`, Score, Date) %>%
    D.showtable(cap)
  
}

D.bat.unusualdiss <- function(df, cap){ df %>%
    filter(`How Out` %in% R.unusual_dismissals) %>% arrange(desc(actuallyDate)) %>%
    select(all_of(H.safflower)) %>%
    D.showtable(cap) %>% J.lemon}

D.bat.samedismissal <- function(df, cap) {df %>%
    group_by(match_id, `Inns of match`, Date, `Match Summary`, Result, actuallyDate) %>% 
    filter(`How Out` %in% R.dismissed) %>%
    select(match_id, `Inns of match`, `How Out`, Date, `Match Summary`, Result, actuallyDate) %>%
    count(`How Out`) %>% ungroup() %>% 
    arrange(desc(actuallyDate)) %>%
    select(`Match Summary`, Date,  Result, `How Out`, n)  %>%
    slice_max(n, n=conf$default_list_length, with_ties = F) %>%
    D.showtable(cap)
    }

# ==== partnerships ====

D.fow.highpship <- function(df, cap="", targ=200){df %>% 
    filter(Partnership >= targ) %>%
    arrange(desc(Partnership)) %>% 
    select(all_of(H.primrose)) %>%
    D.showtable(cap) %>% J.battenberg2}

D.fow.dompship <- function(df, cap="", frac=0.8){df %>%
    filter(Partnership > (frac* Total)) %>%
    arrange(desc(Partnership)) %>%
    select(all_of(H.periwinkle)) %>%
    D.showtable(cap) %>% J.battenberg2}

D.fow.pshipoveropp <- function(df, cap="", top=1000){df %>%
    filter(Partnership > opptotal, opptotal > 0) %>%
    arrange(desc(Partnership)) %>%
    slice_max(actuallyDate, n=top) %>%
    select(all_of(H.periwinkle)) %>%
    D.showtable(cap) %>% J.battenberg2}
 
# D.fow.pshiptopwkt <- function(df, cap="", thewkt=1) {df %>% 
#     filter(Partnership >= 25) %>%
#     select(all_of(H.primrose), Ven) %>%
#     group_by(Wkt) %>% slice_max(Partnership, n=5) %>%
#     filter(Wkt==thewkt) %>%
#     select(all_of(H.primrose)) %>%
#     D.showtable(paste(cap, "for the", R.nths[[thewkt]], "wicket")) %>%
#     J.battenberg}

W.fow.pshiptopwkt <- function(df, thewkt=1) {df %>% 
    filter(Partnership >= 25) %>%
    select(all_of(H.primrose), Ven) %>%
    group_by(Wkt) %>% slice_max(Partnership, n=5) %>%
    ungroup() %>%
    filter(Wkt==thewkt) %>%
    select(all_of(H.primrose), -`Wkt`) %>%
    mutate(Wicket = str_to_title(R.nths[[thewkt]]), .before=`Date`) }

W.fow.pshiptopwktXI <- function(theteamname, theteam, df){df %>%
    filter(batting_team_id == theteam) %>%
    map(1:10, W.fow.pshiptopwkt,  df=.) %>%
    bind_rows()}

# D.fow.pshiptopwktXI <- function(df, cap="", thewkt=1, theteam="1st XI") {df %>%
#     filter(Partnership >= 20) %>%
#     select(all_of(H.primrose), Ven) %>%
#     group_by(Wkt, `Batting Team`) %>% slice_max(Partnership, n=5) %>%
#     filter(Wkt==thewkt, `Batting Team`==theteam) %>%
#     select(all_of(H.primrose)) %>%
#     D.showtable(paste(cap, "for the", theteam,  R.nths[[thewkt]], "wicket"))%>%
#     J.battenberg2}

# ====  bowling =====

D.bowlavg.best <- function(df, cap) {df %>% 
    filter(Inns >= conf$qualify_inns_avgs) %>% arrange(Avg, desc(W)) %>%
    select(all_of(H.clematis)) %>%
    D.showtable(cap)}

D.bowlavg.best2 <- function(df, cap) {df %>% 
  filter(!is.na(Name) & BB >= conf$qualify_balls_bowled) %>%
  slice_min(Avg, n=conf$default_list_length)%>%
    select(all_of(H.clematis)) %>%
    D.showtable(cap)}

D.bowlavg.wkt <- function(df, cap, howmany=10000) {df %>% 
    filter(W>0) %>%
     arrange(desc(W), R) %>%
    slice_max(W, n=howmany, with_ties = FALSE) %>%
    select(all_of(H.clematis)) %>%
    D.showtable(cap)}

D.bowlavg.mdn <- function(df, cap, howmany=10000) {df %>% 
    filter(M>0) %>%
    arrange(desc(M), Econ) %>%
    slice_max(M, n=howmany, with_ties = FALSE) %>%
    select(all_of(H.clematis)) %>%
    D.showtable(cap)}

W.bowl.highavgmanywkts <- function(df, min=100) {df %>% 
    filter(W >= min) %>% arrange(desc(Avg)) %>%
    select(all_of(H.clematis))}

D.bowl.highavgmanywkts <- function(df, cap, min=100){df %>%
    W.bowl.highavgmanywkts(min=min) %>%
    D.showtable(cap)}


D.bowlavg.econ <- function(df, cap){df %>% 
  filter(!is.na(Name) & BB >= conf$qualify_balls_bowled) %>%
  slice_min(Econ, n=conf$default_list_length)%>%
    select(all_of(H.clematis)) %>%
    D.showtable(cap)}
  
D.bowlavg.sr <- function(df, cap){df %>% 
  filter(!is.na(Name) & BB >= conf$qualify_balls_bowled) %>%
  slice_min(SR, n=conf$default_list_length)%>%
    select(all_of(H.clematis)) %>%
    D.showtable(cap)}

D.bowlavg.most5wi <- function(df, cap){df %>%
    filter(!is.na(Name)) %>%
    arrange(Avg) %>%
    slice_max(`5wi`, n=conf$default_list_length, with_ties = FALSE)%>%
    select(all_of(H.clematis)) %>%
    D.showtable(cap)}

W.bowl.bestavgbytype <- function(df, thistype="League", min=conf$qualify_balls_bowled) {df %>%
    filter(Type==thistype) %>%
    makeBowlAvgs() %>%
    filter(!is.na(Name) & BB >= min) %>%
    arrange(Avg, desc(W)) %>%
    slice_min(Avg, n=conf$default_list_length, with_ties = FALSE)%>%
    select(all_of(H.clematis)) %>%
    mutate(Type = thistype, .before=`Name`) }

    # select(batsman_id, Runs, Type, Name) %>%
    # group_by(Type, batsman_id, Name) %>% summarise(n = sum(Runs, na.rm = TRUE)) %>%
    # ungroup() %>% group_by(Type) %>% 
    # slice_max(n, n = conf$default_list_length) %>% 
    # 
    # ungroup() %>%
    # select(-batsman_id, -Type)  %>%
    # arrange(desc(n)) %>% 
    # select(all_of(H.mugwort)) %>%
    # }



D.bowl.fourfors <- function(df, cap) {df %>%
    filter(W>=4) %>% arrange(desc(W), R) %>%
    select(all_of(H.cyclamen)) %>%
    D.showtable(cap) %>% J.parkin}

D.bowl.fiveforlose <- function(df, cap, ms=5){df %>%
    filter(W >=ms & Res!="W" ) %>%
    slice_max(actuallyDate, n=conf$default_list_length) %>%
    select(all_of(H.cyclamen)) %>%
    D.showtable(cap) %>% J.parkin}

D.bowl.consecbowlms <- function(df, cap, ms=4) { df %>%
  select(bowler_id, actuallyDate, Yr, fielding_club_id,
             Name, O, M, R, W, Date, `Team`, `Batting Club`, Ground, 
             Res, Ven) %>%
    filter(!is.na(W)) %>%
    arrange(bowler_id, actuallyDate) %>%  # Sort by player and date
    group_by(bowler_id) %>%  # Group by player
    mutate(
      milestone = W >= ms,  # Flag if runs >= 100
      next_match_is_milestone = lead(milestone, default = FALSE)  # Check if next match is also 100+
    ) %>%
    filter(milestone & (next_match_is_milestone | lag(milestone, default = FALSE))) %>%  # Keep both current and lag match if they are 100+
    ungroup() %>% arrange(desc(actuallyDate)) %>%
    select(all_of(H.cyclamen)) %>%
    D.showtable(cap) %>% J.parkin}

# D.bowl.msteam <- function(df, cap, ms=4, team) { df %>%
#  filter(W >=ms & fielding_team_id == team) %>% 
#     slice_max(actuallyDate, n=conf$default_list_length) %>%
#     select(all_of(H.cyclamen)) %>%
#     D.showtable(paste(cap, "for", names(which(conf$teams_of_interest == team))) )%>%
#     J.parkin}

W.bowl.msteam <- function(theteamname, theteam, df, ms=4) { df %>%
    filter(W >=ms & fielding_team_id == theteam) %>% 
    slice_max(actuallyDate, n=conf$default_list_length) %>%
    select(all_of(H.cyclamen)) %>%
    mutate(Team = theteamname, .before=`Name`) }

D.bowl.msdebut <- function(df, cap, ms=4){df %>%
    filter(!is.na(W)) %>%
    group_by(bowler_id, Name) %>%  slice_min(actuallyDate, n=1) %>% ungroup() %>%
    filter(W>=ms) %>%
    arrange(desc(actuallyDate)) %>%
    select(all_of(H.cyclamen)) %>%
    D.showtable(cap) %>% J.parkin}

D.bowl.nowkts <- function(df, cap){ df %>%
     filter(W ==0 ) %>%
    slice_max(R, n=conf$default_list_length) %>%
    select(all_of(H.cyclamen)) %>%
    D.showtable(cap) %>% J.parkin}

D.bowl.allmaidens <- function(df, cap, min=2){df %>%
    filter(O == M, O >= min) %>%
    arrange(desc(actuallyDate)) %>%
    select(all_of(H.cyclamen)) %>%
    D.showtable(cap) %>% J.parkin}

D.bowl.mostmaidens <- function(df, cap, min=2){df %>%
    filter(M >= min) %>%
    arrange(desc(actuallyDate)) %>%
    slice_max(M, n=conf$default_list_length, with_ties = F) %>%
    select(all_of(H.cyclamen)) %>%
    D.showtable(cap) %>% J.parkin}

D.bowl.cheap5for <- function(df, cap, ms=5, min=10){df %>%
    filter(R < min, W >=ms, Name !="Unsure") %>% arrange(desc(W), R) %>%
    slice_max(W, n=conf$default_list_length, with_ties = F) %>%
    select(all_of(H.cyclamen)) %>%
    D.showtable(cap) %>% J.parkin}

D.bowl.bestanalysis <- function(df, cap){ df %>%
    arrange(desc(W), R) %>%
    slice_max(W, n=conf$default_list_length, with_ties = F) %>%
    select(all_of(H.cyclamen)) %>%
    D.showtable(cap) %>% J.parkin}

D.bowl.bestanalysis2 <- function(df, cap){ df %>%
    arrange(desc(W), R) %>%
    slice_max(W, n=conf$default_list_length, with_ties = F) %>%
    select(all_of(H.cyclamen2)) %>%
    D.showtable(cap) %>% J.parkin %>% column_spec(7, width = "9em")}

# D.bowl.bestanalysistype <- function(df, cap, thistype){ df %>%
#     filter(Type == thistype) %>% arrange(desc(W), R) %>%
#     slice_max(W, n=conf$default_list_length, with_ties = F) %>%
#     select(all_of(H.cyclamen)) %>%
#     D.showtable(paste(cap, "for", thistype)) %>% J.parkin}

W.bowl.bestanalysistype <- function(df, thistype){ df %>%
    filter(Type == thistype) %>% arrange(desc(W), R) %>%
    slice_max(W, n=conf$default_list_length, with_ties = F) %>%
    select(all_of(H.cyclamen)) %>%
    mutate(Type = thistype, .before=`Name`)}

# D.bowl.mostwktstype <- function(df, cap, thistype, min=5) { df %>%
#       select(bowler_id, W, Type, Name) %>% 
#       group_by(Type, bowler_id, Name) %>% summarise(n = sum(W, na.rm=TRUE)) %>%
#       filter(Type == thistype, n>=min) %>%
#         ungroup() %>%
#         slice_max(n, n=conf$default_list_length) %>%
#     select(all_of(H.mugwort)) %>%
#     D.showtable(paste(cap, "for", thistype))}

W.bowl.mostwktstype <- function(df, thistype, min=5) { df %>%
    select(bowler_id, W, Type, Name) %>% 
    group_by(Type, bowler_id, Name) %>% summarise(n = sum(W, na.rm=TRUE)) %>%
    filter(Type == thistype, n>=min) %>%
    ungroup() %>%
    slice_max(n, n=conf$default_list_length) %>%
    select(all_of(H.mugwort)) %>% mutate(Type = thistype, .before=`Name`)
  }

D.bowlavg.highavgwith5for <- function(df, cap){ df %>%
    filter(`5wi` >= 1) %>%
    slice_max(Avg, n=conf$default_list_length)%>%
    select(all_of(H.clematis)) %>%
    D.showtable(cap)}
    
D.allround.keeperbowling <- function(df, cap) {df %>%
    filter(`W-K` == TRUE, O >0) %>%
    arrange(desc(W), R) %>%
    slice_max(W, n=conf$default_list_length, with_ties = F) %>%
    rename(`Fielding Side` = Club,
           `Batting Club` = Oppo,) %>%
    select(all_of(H.cyclamen)) %>%
    D.showtable(cap) %>% J.parkin}

D.bowlavg.nearbowlms <- function(df, cap, myclub){ df %>%
    
    filter(club_id == myclub& !is.na(Name) & 
                           bowler_id %in% Y.currentplayers$player_id) %>% 
    filter((W >= 45 & W < 50) |(W >= 90 & W < 100) | (W >= 240 & W < 250)) %>%
    arrange(desc(W)) %>%
    select(all_of(H.clematis)) %>%
    D.showtable(cap)}

D.bowl.mostwktseason <- function(df, cap){df %>%
    filter(!is.na(W)) %>%
    group_by(bowler_id, Yr, Name) %>% summarise(`Total` = sum(W), `Matches` = n()) %>% ungroup() %>%
    rename(Season = Yr) %>% select(-bowler_id) %>% 
    slice_max(Total, n=conf$default_list_length) %>% 
    select(all_of(H.heather)) %>%
    D.showtable(cap)}

D.bowl.bestavgseason <- function(df, cap, min=conf$qualify_inns_avgs){df %>%
    filter(fielding_club_id== conf$club_of_interest) %>%
    group_by(bowler_id, Yr, Name) %>%
    summarise(`Avg` = avRn(sum(R)/sum(W)), `Matches` = n(), Wkts = sum(W)) %>% 
    ungroup() %>% rename(Season = Yr) %>% filter(Matches >= min) %>% 
    select(-bowler_id) %>% 
    slice_min(Avg, n=conf$default_list_length) %>% 
    select(all_of(H.heatherAv)) %>%
    D.showtable(cap)}

D.bowl.fast40 <- function(df, cap, ms=40){df %>%
    filter(!is.na(W), !is.na(bowler_id)) %>%
    select(Yr, actuallyDate, bowler_id, Name, W) %>% 
    group_by(Yr, bowler_id) %>% 
    arrange(actuallyDate) %>%              # Sort by date within each player
    mutate(cumulative_wkts = cumsum(W)) %>%  # Calculate cumulative runs for each player
    mutate(`Date Reached` = format(ymd(actuallyDate), "%e %b"),
           `Day of Year` = yday(actuallyDate)) %>%
    filter(cumulative_wkts >= ms) %>%  # Filter players who reach 40 wkts
    slice(1) %>%  # Get the first match for each player to reach 40 
    ungroup() %>%
    arrange(`Day of Year`) %>%
    select(Name, `Date Reached`, `Yr`) %>%
    D.showtable(cap)}

D.bowl.manyextras <- function(df, cap, ms=50){df %>%
    filter(ExtPc >= ms) %>%
    arrange(desc(actuallyDate)) %>% slice_head(n=conf$default_list_length) %>%
    select(all_of(H.lupin)) %>%
    D.showtable(cap) %>% J.parkin}

W.bowl.tailbowlwkt <- function(df, ms=2, thresh=9){df %>%
    filter(W >=ms & bowlpos >= thresh ) %>% 
    arrange(desc(actuallyDate)) %>%
    select(all_of(H.cyclamen))} 

D.bowl.tailbowlwkt <- function(df, cap, ms=2, thresh=9){df %>%
    W.bowl.tailbowlwkt(ms=ms, thresh=thresh) %>%
    D.showtable(cap) %>% J.carrot}


# ==== match level ====

W.inns.winmargin <- function(df, whowon=1, marg=250, op= `>=`) {df %>%
    filter(BatInnsWinner == whowon, Res == "W", op(Margin, .env$marg)) %>%
    arrange(desc(actuallyDate)) %>% select(all_of(H.phlox))}

D.inns.winmargin <- function(df, cap="Innings winning margins", whowon=1, marg=250, op= `>=`){
  W.inns.winmargin(df, whowon, marg, op) %>%
    D.showtable(cap) %>% J.angel}

W.inns.dayagg <- function(df){df %>%
    group_by(batting_club_id, Date, `Batting Club`) %>%
    summarise(`Aggregate Runs` = sum(Total),
              `No of Inns` = n()) %>%
    arrange(desc(`Aggregate Runs`)) %>% ungroup() %>%
    select(-batting_club_id) %>%
    slice_head(n=conf$default_list_length)}

D.inns.dayagg <- function(df, cap){df %>%
    W.inns.dayagg() %>%
    D.showtable(cap)}



D.resultsumm.resultsty <- function(df, cap) {df %>%
    filter(Yr==conf$year_of_interest) %>%
    arrange(desc(P), desc(W)) %>%
    D.showtable(cap)}

D.resultsumm.results <- function(df, cap) {df %>%
  arrange(desc(Yr)) %>%
    D.showtable(cap)}

D.resultsumm.mostwins <- function(df, cap) {df %>%
    slice_max(W, n=conf$default_list_length) %>%
    D.showtable(cap)}

D.match.ties <- function(df, cap){df %>%
  filter(Result =="Tied") %>% arrange(desc(actuallyDate)) %>% 
    select(all_of(H.phlox)) %>%
    D.showtable(cap) %>% J.angel }
  # H.F.phloxfam(cap, H.phlox)}

D.match.matchview <- function(df, cap) {df %>%
    arrange(desc(actuallyDate)) %>%
    select(all_of(H.phlox)) %>%
    D.showtable(cap) %>% J.angel }

D.match.highmatchagg <- function(df, cap){ df %>%
    slice_max(`Match Aggregate`, n=conf$default_list_length) %>% 
    select(all_of(H.freesia)) %>%
    D.showtable(cap) %>% J.lamington }

D.match.lowmatchagg <- function(df, cap){df %>%
  filter(!is.na(result_applied_to)
                     & `Match Aggregate` > 0 ) %>% 
  slice_min(`Match Aggregate`, n=conf$default_list_length) %>% 
    select(all_of(H.freesia)) %>%
    D.showtable(cap) %>% J.lamington }

D.match.highmatchaggexts <- function(df, cap){ df %>%
    slice_max(`Match Aggregate Extras`, n=conf$default_list_length) %>% 
    select(all_of(H.freesia), `Match Aggregate Extras`, -`Match Overs`) %>%
    D.showtable(cap) %>% J.lamington }

D.match.short <- function(df, cap){ df %>%
    filter(!is.na(result_applied_to) & 
           `Match Aggregate` > 0 & matchAggDecOvs > 0) %>%
    slice_min(`matchAggDecOvs`, n=conf$default_list_length) %>% 
    select(all_of(H.freesia)) %>%
    D.showtable(cap) %>% J.lamington }

# D.inns.winmargin <- function(df, cap, whowon=1, marg=250, op= `>=`) {df %>%
#   filter(BatInnsWinner == whowon, Res == "W", op(Margin, .env$marg)) %>%
#   arrange(desc(actuallyDate)) %>%
#   H.F.hyacinthfam(cap, H.phlox)}


# ==== innings ====

W.inns.quotients <- function(dfU, dfT) {
  
  a.QRunsF <- dfU %>%
    select(Total) %>% sum(na.rm = TRUE)
  
  a.QInnsF <- dfU %>%
    select(Total) %>% count() %>% pull()
  
  a.QWktsF <- dfU %>%
    select(W) %>% sum(na.rm = TRUE)
  
  a.QRunsA <- dfT %>%
    select(Total) %>% sum(na.rm = TRUE)
  
  a.QInnsA <- dfT %>%
    select(Total) %>% count() %>% pull()
  
  a.QWktsA <- dfT %>%
    select(W) %>% sum(na.rm = TRUE)
  
  a.Quots <- tribble (~Role, ~Inns, ~Runs, ~Wkts, ~`R/W`, ~`R/I`, ~`W/I`,
                      "Batting", a.QInnsF, a.QRunsF, a.QWktsF, avRn(a.QRunsF/a.QWktsF), avRn(a.QRunsF/a.QInnsF), avRn(a.QWktsF/a.QInnsF),
                      "Bowling", a.QInnsA, a.QRunsA, a.QWktsA, avRn(a.QRunsA/a.QWktsA), avRn(a.QRunsA/a.QInnsA), avRn(a.QWktsA/a.QInnsA))
  
  
  a.Quots} 

D.inns.quotients <- function(dfU, dfT, cap) {
 W.inns.quotients(dfU, dfT) %>%
    D.showtable(cap)  }


D.inns.teamquotients <- function(df, cap, theteam){
  us <- df %>% dplyr::filter(batting_team_id==theteam)
  them <- df %>% filter(fielding_team_id==theteam)
  D.inns.quotients(us, them, paste(cap, "for", names(which(conf$teams_of_interest == theteam))))}
#nb that one calls another D function

W.inns.allteamquotients <- function(theteamname, theteam, df){
  us <- df %>% dplyr::filter(batting_team_id==theteam)
  them <- df %>% filter(fielding_team_id==theteam)
  W.inns.quotients(us, them) %>% mutate(Team = theteamname, .before=`Role`)
}

D.inns.recentinns <- function(df, cap){df %>%
   slice_max(actuallyDate, n=conf$default_list_length) %>% 
    select(all_of(H.hyacinth)) %>%
    D.showtable(cap) %>% J.honey }

D.inns.highinnstotal <- function(df, cap){df %>%
  slice_max(Total, n=conf$default_list_length ) %>% 
    select(all_of(H.dahlia)) %>%
    D.showtable(cap) %>% J.madeira}

D.inns.highinnstotal2 <- function(df, cap){df %>%
    slice_max(Total, n=conf$default_list_length ) %>% 
    select(all_of(H.dianthus)) %>%
    D.showtable(cap) %>% J.madeira}

W.inns.highinnstotalbyteam <- function(theteamname, theteam, df) {df %>%
    filter(batting_team_id == theteam) %>%
    slice_max(Total, n=conf$default_list_length) %>%
    select(all_of(H.dianthus)) }
    #mutate(Team = theteamname, .before=`Name`) }

D.inns.lowinnstotal <- function(df, cap){df %>%
    filter(Res !="A"  & Res !="C" & Total != 0) %>%
  slice_min(Total, n=conf$default_list_length ) %>% 
    select(all_of(H.dahlia)) %>%
    D.showtable(cap) %>% J.madeira}

D.inns.lowinnstotal2 <- function(df, cap){df %>%
    filter(Res !="A"  & Res !="C" & Total != 0) %>%
    slice_min(Total, n=conf$default_list_length ) %>% 
    select(all_of(H.dianthus)) %>%
    D.showtable(cap) %>% J.madeira}

D.inns.high2ndinns <- function(df, cap){df %>%
filter(innings_number==1 & `Inns of match` == 2) %>%
  slice_max(Total, n=conf$default_list_length ) %>% 
    select(all_of(H.dahlia)) %>%
    D.showtable(cap) %>% J.madeira}

D.inns.high2ndinns2 <- function(df, cap){df %>%
    filter(innings_number==1 & `Inns of match` == 2) %>%
    slice_max(Total, n=conf$default_list_length ) %>% 
    select(all_of(H.dianthus)) %>%
    D.showtable(cap) %>% J.madeira}

D.inns.highforone <- function(df, cap, ms=1){df %>%
  filter(W <= ms) %>% slice_max(Total, n=conf$default_list_length ) %>% 
  select(all_of(H.dianthus)) %>% 
    select(all_of(H.dianthus)) %>%
    D.showtable(cap) %>% J.madeira}

D.inns.highchase <- function(df, cap){df %>%
    filter(`Inns of match` == 2 & Res == "W") %>%
    slice_max(Total, n=conf$default_list_length) %>%
    select(all_of(H.dianthus)) %>%
    D.showtable(cap) %>% J.madeira}

D.inns.highlosing <- function(df, cap){df %>%
    filter( Res == "L") %>%
    slice_max(Total, n=conf$default_list_length) %>% 
    select(all_of(H.dianthus)) %>%
    D.showtable(cap) %>% J.madeira}

D.inns.lowhighestscore <- function(df, cap){df %>%
    filter(topscore>1, Res %in% c("W", "L", "T")) %>%
    slice_min(topscore, n=conf$default_list_length) %>%
    select(topscore, all_of(H.rudbeckia)) %>%
    D.showtable(cap)}

D.inns.lowtotalallovs <- function(df, cap) {df %>% 
  filter(Ovs == no_of_overs, W < 10) %>%
  slice_min(Total, n = conf$default_list_length) %>%
  select(all_of(H.hyacinth), Total) %>%
    D.showtable(cap) %>% J.honey}

D.inns.lowquot <- function(df, cap){df %>%
   filter( `Runs/Wkt`!=0) %>%
    slice_min(`Runs/Wkt`, n=conf$default_list_length ) %>%
    select(all_of(H.hyacinth)) %>%
    D.showtable(cap) %>% J.honey }

D.inns.highquot <- function(df, cap){df %>%
    filter( `Runs/Wkt`!=0) %>%
    arrange(desc(`Runs/Wkt`), desc(Total)) %>%
    slice_max(`Runs/Wkt`, n=conf$default_list_length ) %>%
    select(all_of(H.hyacinth)) %>%
    D.showtable(cap) %>% J.honey }

D.inns.mostextras <- function(df, cap){df %>%
    slice_max(Extras, n=conf$default_list_length ) %>% 
    select(all_of(H.rudbeckia)) %>%
    D.showtable(cap) %>% J.madeira}

D.inns.mostextras2 <- function(df, cap){df %>%
    slice_max(Extras, n=conf$default_list_length ) %>% 
    select(all_of(H.dahlia), Extras) %>%
    D.showtable(cap) %>% J.madeira}

D.inns.tossresult <- function(df, cap){ df %>%
    drop_na(`Our toss result`) %>%
    select(actuallyDate, Result, `Our toss result`,
                  Date, `Match Summary`, Res) %>%
    count(`Our toss result`, Res) %>%
    pivot_wider(names_from = Res, values_from = n) %>%
    D.showtable(cap)}

D.inns.manybowlers <- function(df, cap, ms=10, howmany=conf$default_list_length){df %>%
    filter(`Bowlers used` >= ms) %>%
    slice_max(actuallyDate, n=howmany) %>%
  #  arrange(desc(actuallyDate)) %>%
    rename(Bowlers = `Bowlers used`) %>%
    select(all_of(H.peony)) %>%
    D.showtable(cap) %>% J.madeira}

D.inns.manywkttakers <- function(df, cap, ms=7){df %>%
   filter(`Wicket-takers` >= ms) %>%
    arrange(desc(actuallyDate)) %>% rename(Bowlers = `Wicket-takers`) %>%
    select(all_of(H.peony)) %>%
    D.showtable(cap) %>% J.madeira  }

D.inns.allbowlersmaiden <- function(df, cap, howmany=conf$default_list_length ){df %>%
    filter(numbowlsmdn == `Bowlers used`) %>%
    slice_max(actuallyDate, n=howmany) %>%
    rename(Bowlers = `Bowlers used`) %>%
    select(all_of(H.peony)) %>%
    D.showtable(cap) %>% J.madeira  }


D.inns.samebowlanaly <- function(df, cap, ms=3){df %>%
    filter(BWIA >= ms) %>%
    arrange(desc(actuallyDate)) %>% rename(Bowlers = BWIA) %>%
    select(all_of(H.peony)) %>%
    D.showtable(cap) %>% J.madeira  }

D.inns.hightotal4bowl <- function(df, cap){df %>%
    filter(`Bowlers used` == 4) %>% arrange(desc(Total)) %>% 
    rename(`Bowlers` = `Bowlers used`) %>% 
    slice_max(Total, n=conf$default_list_length ) %>%
    select(all_of(H.peony)) %>%
    D.showtable(cap) %>% J.madeira  }

D.inns.hightotalnocent <- function(df, cap, ms=100){df %>%
    filter(topscore < ms) %>% 
    slice_max(Total, n=conf$default_list_length ) %>%
    select(all_of(H.hyacinth)) %>%
    D.showtable(cap) %>% J.honey }

D.inns.manycentury <- function(df, cap, min=2){df %>%
     filter(numcenturies >= min) %>% 
    arrange(desc(actuallyDate)) %>%
    select(all_of(H.hyacinth)) %>%
    D.showtable(cap) %>% J.honey }

D.inns.manyfifty <- function(df, cap, min=3){df %>%
    filter(numfifties >= min) %>% 
    arrange(desc(actuallyDate)) %>%
    select(all_of(H.hyacinth)) %>%
    D.showtable(cap) %>% J.honey }

D.inns.allbatscontrib <- function(df, cap, score=10, num=6){df %>%
    filter(botscore >= score  & numbats >= num) %>% 
    arrange(desc(actuallyDate)) %>%
    select(all_of(H.hyacinth)) %>%
    D.showtable(cap) %>% J.honey }

D.inns.lowindivid <- function(df, cap, score=15){df %>%
    filter(Res == "W", topscore != 0,  topscore <= score) %>% 
    arrange(desc(actuallyDate)) %>%
    select(all_of(H.hyacinth)) %>%
    D.showtable(cap) %>% J.honey }

D.inns.lowindividhightotal <- function(df, cap, score=15){df %>%
    filter(topscore > 1,  topscore <= score) %>% 
    slice_max(Total, n=conf$default_list_length) %>%
    select(topscore, all_of(H.rudbeckia)) %>%
    D.showtable(cap)}

D.inns.feastfamine <- function(df, cap){df %>%
    filter(topscore>50 & numtens==numfifties) %>% 
    slice_max(actuallyDate, n=conf$default_list_length) %>%
    select(topscore, all_of(H.rudbeckia)) %>%
    D.showtable(cap)}

D.inns.many4for <- function(df, cap, min=2){df %>%
    filter(numfourfs >= min) %>% 
    slice_max(actuallyDate, n=conf$default_list_length ) %>%
    select(all_of(H.hyacinth)) %>%
    D.showtable(cap) %>% J.honey }

W.inns.all2for <- function(df, min=2){df %>%
  filter(minwkts >= min) %>% 
  slice_max(actuallyDate, n=conf$default_list_length ) %>%
  select(all_of(H.hyacinth))}

D.inns.all2for <- function(df, cap, min=2){df %>%
    W.inns.all2for(min=min) %>%
    D.showtable(cap) %>% J.honey}

D.inns.equitbowl <- function(df, cap, tot=250, mx=50){df %>%
    filter(Total > tot & maxrunsccd < mx) %>%
    arrange(desc(actuallyDate)) %>%
    select(all_of(H.hyacinth)) %>%
    D.showtable(cap) %>% J.honey }

D.inns.nobowlextras <- function(df, cap){df %>%
    filter( (Byes + `Leg byes`+ Wides + `No-balls`)==Extras, 
            Wides==0, `No-balls`==0) %>% 
    slice_max(actuallyDate, n=conf$default_list_length) %>%
    select(all_of(H.rudbeckia)) %>%
    D.showtable(cap)}

D.inns.lowfailchase <- function(df, cap){df %>%
    filter(`Inns of match` == 2, Res == "L", Total > 0) %>% 
  slice_min(firstinnstotal, n=conf$default_list_length) %>%
  select(`Match Summary`, Date, Score,  `Oppo Score`) %>%
    D.showtable(cap)}

# Many players of the same name
R.bad_names <- c("Unsure", "-", "A", "Dob", "T.B.C")
D.inns.sharednames <- function(df, cap){df %>%  
  select(Name, match_id, `Inns of match`, Date, `Match Summary`, Result, actuallyDate) %>%
  group_by(match_id, `Inns of match`,Date, `Match Summary`, Result, actuallyDate) %>%
  mutate(lastname = word(Name, -1)) %>%
  filter(!lastname %in% R.bad_names) %>% count(lastname) %>%
  arrange(desc(actuallyDate)) %>%
  ungroup() %>%
  select(`Match Summary`, Date, Result, Name = lastname, n) %>%
  slice_max(n, n = 3) %>%
    D.showtable(cap) %>% J.honey}


# ==== allrounder =====

D.allround.matchdouble <- function(df, cap, Rns=30, Wms=3) {df %>%
    filter(Runs >= Rns & W >= Wms) %>% 
    arrange(desc(actuallyDate)) %>%
    select(all_of(H.violet)) %>%
    D.showtable(cap) %>% J.gingerbread}

D.allround.5duckdebut <- function(df, cap, ms=4){df %>%
    filter(!is.na(W)) %>%
    group_by(player_id, Name) %>%  slice_min(actuallyDate, n=1) %>% ungroup() %>%
    filter(W>=ms, Runs==0) %>%
    arrange(desc(actuallyDate)) %>%
    select(all_of(H.violet)) %>%
    D.showtable(cap) %>% J.gingerbread()}

D.allround.fielddouble <- function(df, cap, Rns=50, D=3) {df %>%
    filter(Runs >= Rns & (Ct+ Std) >= D) %>% 
  arrange(desc(actuallyDate)) %>%
    select(all_of(H.violet)) %>%
    D.showtable(cap) %>% J.gingerbread}

D.allround.consecms <- function(df, cap, Rns=40, Wms = 4) { df %>%
    filter( !is.na(W), !is.na(Runs)) %>%
    arrange(player_id, actuallyDate) %>%  # Sort by player and date
    group_by(player_id) %>%  # Group by player
    mutate(
      milestone = (W >= Wms & Runs >= Rns) , # Flag if runs >= 100
      next_match_is_milestone = lead(milestone, default = FALSE)  # Check if next match is also 100+
    ) %>%
    filter(milestone & (next_match_is_milestone | lag(milestone, default = FALSE))) %>%  # Keep both current and lag match if they are 100+
    ungroup() %>% arrange(desc(actuallyDate)) %>%
    select(Name, Date, Runs, Analy, Ct, Oppo, Ground) %>%
    D.showtable(cap)}

D.ar.seasondouble <- function(dfA, dfB, dfC, cap, msA=500, msB=20, msC=10){ 
  full_join(
    dfA %>%
      group_by(batsman_id, Yr, Name) %>% summarise(`Total` = sum(Runs, na.rm = TRUE), `Matches` = n()) %>% ungroup() %>%
      rename(Season = Yr) ,
    dfB %>%
      group_by(bowler_id, Yr, Name) %>% summarise(`Total` = sum(W, na.rm = TRUE), `Matches` = n()) %>% ungroup() %>%
      rename(Season = Yr),
    by = c("Season", "batsman_id" = "bowler_id"), 
    keep = F, na_matches = "never") %>%
    mutate(Matches = pmax(Matches.x, Matches.y, na.rm=TRUE)) %>%
    select(Name = Name.x, Season, Runs = Total.x, Wkts = Total.y, Matches, batsman_id) %>%
    
    
    full_join(dfC %>% 
                group_by(player_id, Yr, Name) %>% summarise(`Total` = sum(Dis, na.rm = TRUE), `Matches` = n()) %>% ungroup() %>% 
                rename(Season = Yr),by = c("Season", "batsman_id" = "player_id"), 
              keep = F, na_matches = "never") %>%
    mutate(Matches = pmax(Matches.x, Matches.y, na.rm=TRUE)) %>%
    select(Name = Name.x, Season, Runs, Wkts, Dis = Total,  Matches) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    filter(Runs >= msA, Wkts >= msB, Dis >= msC)  %>%
    arrange(desc(Runs)) %>%
    select(all_of(H.rose)) %>%
    D.showtable(cap) }

D.joinav.careerdouble <- function(df, cap, msBat, msBowl, msDis){df %>%
    filter(Runs >= msBat & W >= msBowl & (Ct+ Std) >= msDis) %>% 
    select(all_of(H.petuina)) %>%
    D.showtable(cap)}

# ===== fielding ====


D.fielding.mostcatchesinns <- function(df, cap, howmany=conf$default_list_length, min=2){df %>%
  arrange(desc(actuallyDate)) %>%
    filter(Ct >= min) %>%
  slice_max(Ct, n=howmany, with_ties = F ) %>% 
    select(all_of(H.plum)) %>%
    D.showtable(cap) %>% J.lemon}


D.fielding.mostcatchesinns2 <- function(df, cap, howmany=conf$default_list_length, min=2){df %>%
    arrange(desc(actuallyDate)) %>%
    filter(Ct >= min) %>%
    slice_max(Ct, n=howmany, with_ties = F ) %>% 
    select(all_of(H.poppy)) %>%
    D.showtable(cap) %>% J.lemon}


D.fieldsumm.mostcatches <- function(df, cap, howmany=conf$default_list_length, min=3){df %>%
    arrange(desc(Ct)) %>% 
    filter(Ct >= min) %>%
    slice_max(Ct, n=howmany, with_ties = F) %>%
    select(all_of(H.crocosmia)) %>%
    D.showtable(cap)}

# D.fielding.totalcatchbytype <- function(df, cap, thistype="League") {df %>%
#     select(player_id, Ct, Type, Name) %>%
#     group_by(Type, player_id, Name) %>% summarise(n = sum(Ct, na.rm = TRUE)) %>%
#     ungroup() %>% group_by(Type) %>% 
#     slice_max(n, n = conf$default_list_length) %>% filter(Type==thistype) %>%
#     ungroup() %>%
#     select(-player_id, -Type)  %>%
#     arrange(desc(n)) %>% 
#     select(all_of(H.mugwort)) %>%
#     D.showtable(paste(cap, thistype))}

W.fielding.totalcatchbytype <- function(df, thistype="League") {df %>%
    select(player_id, Ct, Type, Name) %>%
    group_by(Type, player_id, Name) %>% summarise(n = sum(Ct, na.rm = TRUE)) %>%
    ungroup() %>% group_by(Type) %>% 
    slice_max(n, n = conf$default_list_length) %>% filter(Type==thistype) %>%
    ungroup() %>%
    select(-player_id, -Type)  %>%
    arrange(desc(n)) %>% 
    select(all_of(H.mugwort)) %>%
    mutate(Type = thistype, .before=`Name`)}

D.fieldsumm.mostdisperinns <- function(df, cap, min=conf$qualify_inns_avgs) { df %>%
    filter( M >= min, Ct > 0) %>%
  arrange(desc(`Dis/Inns`)) %>%
    slice_head(n=conf$default_list_length) %>%
    select(all_of(H.crocosmia)) %>%
    D.showtable(cap)}

D.fielding.mostcatchseason <- function(df, cap){df %>%
    group_by(player_id, Yr, Name) %>% 
    summarise(`Total` = sum(Ct), `Matches` = n()) %>% ungroup() %>% 
    rename(Season = Yr) %>% select(-player_id) %>% 
    slice_max(Total, n=conf$default_list_length) %>% 
    select(all_of(H.heather)) %>%
    D.showtable(cap)}


# ==== wicket-keeping =====

D.inns.nobyes <- function(df, cap, ms=150){ df %>%
    filter(Total >= ms & Byes == 0) %>% arrange(desc(Total))%>% 
    select(all_of(H.dianthus), Keeper) %>% #was wolfsbane
    D.showtable(cap) %>% J.madeira  }
 
D.fielding.mostblahinns <- function(df, cap, param, min=2){ df %>%
    filter(`W-K` == TRUE) %>%
    arrange(desc(actuallyDate)) %>% filter(.data[[param]] >= min) %>%
    slice_max(.data[[param]], n=conf$default_list_length, with_ties = F ) %>% 
    select(all_of(H.plum)) %>%
    D.showtable(cap) %>% J.lemon}

D.fielding.mostblahinns2 <- function(df, cap, param, min=2){ df %>%
    filter(`W-K` == TRUE) %>%
    arrange(desc(actuallyDate)) %>% filter(.data[[param]] >= min) %>%
    slice_max(.data[[param]], n=conf$default_list_length, with_ties = F ) %>% 
    select(all_of(H.poppy)) %>%
    D.showtable(cap) %>% J.lemon}

D.fielding.mostblah <- function(df, cap, param, qual=3){ df %>%
    filter(`W-K` == TRUE) %>%
    group_by(player_id, Name) %>% summarise(n = sum(.data[[param]])) %>% ungroup() %>%
    filter(n >= qual) %>% arrange(-n) %>%
    select(all_of(H.mugwort)) %>%
    D.showtable(cap)}
    
D.fielding.mostblahseason <- function(df, cap, param){df %>%
    filter(`W-K` == TRUE) %>%
    group_by(player_id, Yr, Name) %>% 
    summarise(`Total` = sum(.data[[param]]), `Matches` = n()) %>% ungroup() %>% 
    rename(Season = Yr) %>% select(-player_id) %>% 
    slice_max(Total, n=conf$default_list_length) %>% 
    select(all_of(H.heather)) %>%
    D.showtable(cap)}

W.fielding.totalWKdisbytype <- function(df, thistype="League") {df %>%
    filter(`W-K` == TRUE) %>%
    select(player_id, Dis, Type, Name) %>%
    group_by(Type, player_id, Name) %>% summarise(n = sum(Dis, na.rm = TRUE)) %>%
    ungroup() %>% group_by(Type) %>% 
    slice_max(n, n = conf$default_list_length) %>% filter(Type==thistype) %>%
    ungroup() %>%
    select(-player_id, -Type)  %>%
    arrange(desc(n)) %>% 
    select(all_of(H.mugwort)) %>%
    mutate(Type = thistype, .before=`Name`)}

D.bat.amesfreeman <- function(df, cap){ 
  
  zxcA <- df %>% filter(`How Out` == "st", fielder_name!="Unsure", bowler_name!="Unsure", bowler_name!=fielder_name) %>% select(all_of(H.begonia), fielder_name, bowler_name) %>%
    mutate(Mode = paste("st ", fielder_name, " b ", bowler_name))
  
  zxcB <- df %>% filter(`How Out` == "st", fielder_name!="Unsure", bowler_name!="Unsure", bowler_name!=fielder_name) %>% select(all_of(H.begonia), fielder_name, bowler_name) %>%
    mutate(hoB = paste("st ", bowler_name, " b ", fielder_name))
  
  A.amesfreeman <- inner_join(zxcA, zxcB, by=c("Mode"= "hoB"), suffix= c("","y")) %>% 
    select('Date', 'Batting Side', 'Fielding Side', 'Name', "Mode")
  
  rm(zxcA, zxcB)
  
  A.amesfreeman %>% 
    select(all_of(H.gladiolus)) %>%
    D.showtable(cap)}

D.fielding.mostappsseason <- function(df, cap, sort="Appearances"){ df %>%
    filter(!Name %in% c("T.B.C", "Unsure")) %>%
    group_by(player_id, Yr, Name) %>% 
    summarise(`Appearances` = n(),
              `As captain` = sum(Capt),
              `As keeper` = sum(`W-K`), 
              `As capt/w-k` = sum(Capt & `W-K`)) %>% ungroup() %>% 
    rename(Season = Yr) %>% select(-player_id) %>% 
    slice_max(.data[[sort]], n=2*conf$default_list_length) %>% 
    D.showtable(cap)}
#    H.F.plainALL(cap)}

D.fielding.appsalltime <- function(df, cap, sort="Appearances") { df %>%
  filter(!Name %in% c("T.B.C", "Unsure")) %>%
  group_by(player_id, Name) %>% 
  summarise(`Appearances` = n(),
            `As captain` = sum(Capt),
            `As keeper` = sum(`W-K`), 
            `As capt/w-k` = sum(Capt & `W-K`)) %>% ungroup() %>% 
  select(-player_id) %>% 
  slice_max(.data[[sort]], n=2*conf$default_list_length) %>% 
  D.showtable(cap)}

# ==== ground ====

D.inns.groundmostruns <- function(df, cap){ df %>%
    group_by(ground_id, Ground) %>% 
    summarise(TRuns = sum(Total, na.rm = TRUE)) %>%
    ungroup() %>% arrange(-TRuns) %>% drop_na() %>% select(n = TRuns, Name =Ground) %>%
    slice_max(n, n=conf$default_list_length) %>%
    select(all_of(H.mugwort)) %>%
    D.showtable(cap)}

D.inns.groundhighteam <- function(df, cap){df %>%
    group_by(ground_id) %>%
    slice_max(Total) %>% ungroup() %>% drop_na(Ground) %>%
    slice_max(Total, n=conf$default_list_length) %>%
    select(all_of(H.phlox)) %>%
    D.showtable(cap) %>% J.angel }

D.bat.groundhighindiv <- function(df, cap){ df %>%
    group_by(ground_id) %>%
    slice_max(Runs) %>% ungroup() %>% drop_na(Ground) %>%
    slice_max(Runs, n=conf$default_list_length) %>%
    select(all_of(H.lily)) %>%
    D.showtable(cap) %>% J.carrot}

D.bat.groundhighindivno50 <- function(df, cap){ df %>%
    group_by(ground_id) %>%
    slice_max(Runs) %>% ungroup() %>% drop_na(Ground) %>% 
    filter(Runs < 50) %>% arrange(Runs) %>%
    select(all_of(H.lily)) %>%
    D.showtable(cap) %>% J.carrot}
