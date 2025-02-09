# === FUNCTIONS FOR ALL ====

# This only provides definitions for a load of functions for displaying stats

# ==== batting ====
D.batavg.bestbatavgs <- function(df, cap){df %>% 
    filter( Inns >= conf$qualify_inns_avgs)%>% arrange(desc(Avg)) %>%
    H.F.plain(cap, H.aster)}

D.batavg.mostruns <- function(df, cap, howmany=10000){df %>%
    arrange(desc(Runs)) %>%
    slice_max(Runs, n=howmany, with_ties = FALSE) %>%
    H.F.plain(cap, H.aster)}

D.bat.totalrunsbytype <- function(df, cap, thistype="League") {df %>%
    select(batsman_id, Runs, Type, Name) %>%
    group_by(Type, batsman_id, Name) %>% summarise(n = sum(Runs, na.rm = TRUE)) %>%
    ungroup() %>% group_by(Type) %>% 
    slice_max(n, n = conf$default_list_length) %>% filter(Type==thistype) %>%
    ungroup() %>%
    select(-batsman_id, -Type)  %>%
    arrange(desc(n)) %>%  H.F.plain(paste(cap, thistype), H.mugwort)}

D.bat.fifties <- function(df, cap){df %>% filter(Runs>=50) %>%
    arrange(desc(Runs)) %>%  H.F.lilyfam(cap, H.lily) } 

D.bat.fiftieslosing <- function(df, cap, ms=50){df %>% filter(Runs>=ms & Res!="W") %>%
    slice_max(actuallyDate, n=conf$default_list_length) %>%
     H.F.lilyfam(cap, H.lily) } 

D.bat.fiftiestailend <- function(df, cap, ms=50){df %>%
    filter(Runs >=ms & Pos %in% c(9,10,11) ) %>% 
    #slice_max(Runs, n=conf$default_list_length) %>%
    H.F.lilyfam(cap, H.lily)  }

D.bat.batmsteam <- function(df, cap, ms=50, team) { df %>% 
    filter(Runs >=ms & batting_team_id == team ) %>% 
    slice_max(actuallyDate, n=conf$default_list_length) %>%
    H.F.lilyfam(paste(cap, "for", names(which(conf$teams_of_interest == team))), H.lily)} 

D.bat.batmsdebut <- function(df, cap, ms=50){df %>%
    filter(!is.na(Runs)) %>%
    group_by(batsman_id, Name) %>%  slice_min(actuallyDate, n=1) %>% ungroup() %>%
    filter(Runs>=ms) %>%
    arrange(desc(actuallyDate)) %>%
    H.F.lilyfam(cap, H.lily)}

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
  H.F.lilyfam(cap, H.lily)}

D.bat.highestscorebytype <- function(df, cap, thistype="League", min=35) {df %>%
  filter(Type == thistype, Runs >=min) %>%
    slice_max(Runs, n=conf$default_list_length) %>%
    H.F.lilyfam(paste(cap, thistype, "matches"), H.lily) }

D.bat.highestscorebyteam <- function(df, cap, team, min=35) {df %>%
    filter(batting_team_id == team , Runs >=min) %>%
    slice_max(Runs, n=conf$default_list_length) %>%
    H.F.lilyfam(paste(cap, "for", names(which(conf$teams_of_interest == team))), H.lily) }

D.bat.highestscore <- function(df, cap, min=35) {df %>%
    filter(Runs >=min) %>%
    slice_max(Runs, n=conf$default_list_length) %>%
    H.F.lilyfam(cap, H.lily) }

D.bat.longinns <- function(df, cap, min=1000){df %>%
  filter(Runs <= min) %>% slice_max(Balls, n=conf$default_list_length) %>%
  H.F.lilyfam(cap, H.lily)}

D.batavg.highSR <- function(df, cap){ df %>%
    filter(!is.na(Name) & BF >= conf$qualify_balls_faced) %>%
    slice_max(SR, n=conf$default_list_length)%>%
    H.F.plain(cap, H.aster) }

D.bat.highSRinns <- function(df, cap, min=35){ df %>%
    filter(Runs >= min) %>%
    slice_max(SR, n=conf$default_list_length) %>%
    H.F.lilyfam(cap, H.lily)}

D.bat.most6inns <- function(df, cap){df %>%
      slice_max(`6`, n=conf$default_list_length) %>%
      H.F.lilyfam(cap, H.hibiscus)}

D.bat.mostBryinns <- function(df, cap){df %>%
    slice_max((`6`+`4`), n=conf$default_list_length) %>%
    H.F.lilyfam(cap, H.hibiscus)}

D.bat.mostBryinnsteam <- function(df, cap, team) { df %>% 
    filter(batting_team_id == team ) %>% 
    D.bat.mostBryinns(paste(cap, "for", names(which(conf$teams_of_interest == team))))}

D.batavg.centurycount <- function(df, cap){df %>%
  filter(!is.na(Name)) %>%
      slice_max(`100`, n=conf$default_list_length) %>%
      H.F.plain(cap, H.aster)   }

D.bat.primaries <- function(df, cap){df%>% 
    filter(Runs == 0  & Balls<= 1 & `How Out` %in% R.dismissed) %>%
    arrange(desc(actuallyDate)) %>%
    H.F.lilyfam(cap, H.lily)}

# D.duckcount2 <- function(df, cap){df %>%
#     filter(Name!="Unsure", Runs==0, `How Out` %in% R.dismissed ) %>%
#     group_by(Name) %>% tally() %>% slice_max(n, n=conf$default_list_length) %>%
#     H.F.plain(cap, H.mugwort)}

D.bat.duckcount <- function(df, cap){ df %>%
 filter(Name!="Unsure", `How Out` %in% R.dismissed, !is.na(Runs)) %>%
  group_by(batsman_id, Name) %>%
  summarise(n = sum(Runs==0), Innings = sum(!is.na(Runs))) %>% ungroup() %>%
  slice_max(n, n=conf$default_list_length) %>%
  H.F.plain(cap, H.mimosa)}

# D.bat.primarycount <- function(df, cap){df %>%
#     filter(Name!="Unsure", Balls <=1,  Runs==0, `How Out` %in% R.dismissed ) %>%
#     group_by(Name) %>% tally() %>% slice_max(n, n=conf$default_list_length) %>%
#     H.F.plain(cap, H.mugwort)}

D.bat.ducklength <- function(df, cap){df %>%
    filter( Name!="Unsure", Runs==0, `How Out` %in% R.dismissed, Balls >=6) %>%
    slice_max(Balls, n=conf$default_list_length) %>% 
    H.F.lilyfam(cap, H.safflower)}

D.batavg.sixcount <- function(df, cap){ df %>%
  filter(!is.na(`4s`) & !is.na(`6s`)) %>%
    arrange(desc(`6s`), desc(`4s`)) %>%
    slice_max(`6s`, n=conf$default_list_length) %>%
    H.F.plain(cap, H.buckthorn)}

D.bat.carriedbat <- function(df, cap) {df %>%
    filter(Pos %in% c("1","2") & `How Out`=="not out") %>% 
    arrange(desc(Runs)) %>%
    slice_max(actuallyDate, n=conf$default_list_length) %>%
    H.F.lilyfam(cap, H.lily)}

D.bat.tailendtop <- function(df, cap) {df %>%
    filter(Pos %in% c("10", "11"), Runs==topscore) %>% 
    arrange(desc(Runs)) %>%
    slice_max(actuallyDate, n=conf$default_list_length) %>%
    H.F.lilyfam(cap, H.lily)}


D.bat.topscorercount <- function(df, cap){df %>%
    filter(Name!="Unsure", Runs == topscore) %>%
    group_by(Name) %>% tally() %>% slice_max(n, n=conf$default_list_length) %>%
    H.F.plain(cap, H.mugwort)}

D.batavg.nearms <- function(df, myclub) {df %>%
  filter(club_id == myclub & !is.na(Name) &
                      batsman_id %in% Y.currentplayers$player_id) %>% 
  filter((Runs >= 400 & Runs < 500) | (Runs >= 900 & Runs < 1000) |
         (Runs >= 1900 & Runs < 2000)) %>%
  arrange(desc(Runs)) %>% H.F.plain("Approaching career total run milestones", H.aster)}

D.bat.maybatms <- function(df, cap, ms=200){df %>%
  filter(months(actuallyDate) %in% c("April", "May") ) %>%
  group_by(batsman_id, Name, Yr) %>%
  summarise(TRuns = sum(Runs), `Matches` = n()) %>% ungroup() %>% drop_na() %>%
  select(-batsman_id) %>% rename(Season = Yr, Total = TRuns) %>%
  filter(Total >= ms) %>% arrange(desc(Total)) %>%
  H.F.plain(cap, H.heather)}

D.bat.500season <- function(df, cap, ms=500){df %>%
    filter(!is.na(Runs), Name !="Unsure") %>%
    group_by(batsman_id, Yr, Name) %>% summarise(`Total` = sum(Runs), `Matches` = n()) %>% 
    ungroup() %>% #FIXME this is probably innings, not matches, but need to strip out NAs
    rename(Season = Yr) %>% select(-batsman_id) %>% 
    filter(Total >= ms) %>% arrange(desc(Total)) %>%
    # slice_max(Total, n=conf$default_list_length) %>% 
    H.F.plain(cap, H.heather)  }

D.bat.nobdry.season <- function(df, cap){df %>%
    filter(!is.na(`4`), Name !="Unsure") %>%
    group_by(batsman_id, Yr, Name) %>% summarise(`Total` = sum(`4` + `6`), `Matches` = n()) %>% 
    ungroup() %>% #FIXME this is probably innings, not matches, but need to strip out NAs
    filter(Total == 0) %>% arrange(desc(Yr)) %>%
    rename(Season = Yr) %>% select(-batsman_id) %>% 
    slice_max(Matches, n=conf$default_list_length, with_ties = FALSE) %>%
    H.F.plain(cap, H.heather)  }

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
    select(Name, `Date Reached`, `Yr`) %>%
    H.F.plainALL(cap) }

D.bat.most50season <- function(df, cap, ms=50){ df %>%
    filter( !is.na(Runs)) %>%
    group_by(batsman_id, Yr, Name) %>% summarise(`Total` = sum(Runs>=ms), `Matches` = n()) %>% ungroup() %>%
    rename(Season = Yr) %>% select(-batsman_id) %>% 
    slice_max(Total, n=conf$default_list_length) %>% 
    H.F.plain(cap, H.heather)  }

D.bat.mostbryseason <- function(df, cap){df %>%
    filter(!is.na(`4`), !is.na(`6`)) %>%
    group_by(batsman_id, Yr, Name) %>% summarise(`Total` = sum(`4`) + sum(`6`), `Matches` = n()) %>% ungroup() %>%
    rename(Season = Yr) %>% select(-batsman_id) %>% 
    slice_max(Total, n=conf$default_list_length) %>% 
    H.F.plain(cap, H.heather)}

# D.bat.mostbryseason2 <- function(df, cap){  df %>%
#     filter(!is.na(`4`), Name !="Unsure") %>%
#     group_by(batsman_id, Yr, Name) %>% summarise(`Total` = sum(`4`), `Matches` = n()) %>% 
#     ungroup() %>% #FIXME this is probably innings, not matches, but need to strip out NAs
#     rename(Season = Yr) %>% select(-batsman_id) %>% 
#     slice_max(Total, n=conf$default_list_length)}

D.bat.bannerman <- function(df, cap, ms=0.6, min=10){df %>%
    filter(contribpc >= ms & Runs >= min) %>% arrange(desc(contribpc)) %>%
    H.F.lilyfam(cap, H.thistle)}

D.bat.outscoringoppo <- function(df, cap){ df %>%
    filter(Runs >= opptotal, opptotal != 0) %>% arrange(desc(actuallyDate)) %>%
    H.F.lilyfam(cap, H.thistle)}

D.bat.unusualdiss <- function(df, cap){ df %>%
    filter(`How Out` %in% R.unusual_dismissals) %>% arrange(desc(actuallyDate)) %>%
    H.F.lilyfam(cap, H.safflower) }

# ==== partnerships ====

D.fow.highpship <- function(df, cap="", targ=200){df %>% filter(Partnership >= targ) %>%
    arrange(desc(Partnership)) %>% H.F.primrosefam(cap, H.primrose)}

D.fow.dompship <- function(df, cap="", frac=0.8){df %>% filter(Partnership > (frac* Total)) %>%
    arrange(desc(Partnership)) %>% H.F.primrosefam(cap, H.periwinkle)}

D.fow.pshipoveropp <- function(df, cap=""){df %>% filter(Partnership > opptotal, opptotal > 0) %>%
    arrange(desc(Partnership)) %>% H.F.primrosefam(cap, H.periwinkle)}

D.fow.pshiptopwkt <- function(df, cap="", thewkt=1) {df %>% filter(Partnership >= 25) %>%
    select(all_of(H.primrose), Ven) %>%
    group_by(Wkt) %>% slice_max(Partnership, n=5) %>%
    filter(Wkt==thewkt) %>%
    H.F.primrosefam(paste(cap, "for the", R.nths[[thewkt]], "wicket"), H.primrose)}

D.fow.pshiptopwktXI <- function(df, cap="", thewkt=1, theteam="1st XI") {df %>% filter(Partnership >= 20) %>%
    select(all_of(H.primrose), Ven) %>%
    group_by(Wkt, `Batting Team`) %>% slice_max(Partnership, n=5) %>%
    filter(Wkt==thewkt, `Batting Team`==theteam) %>%
    H.F.primrosefam(paste(cap, "for the", theteam,  R.nths[[thewkt]], "wicket"), H.primrose)}

# ====  bowling =====

D.bowlavg.best <- function(df, cap) {df %>% 
    filter(Inns >= conf$qualify_inns_avgs) %>% arrange(Avg, desc(W)) %>%
    H.F.plain(cap, H.clematis)}

D.bowlavg.best2 <- function(df, cap) {df %>% 
  filter(!is.na(Name) & BB >= conf$qualify_balls_bowled) %>%
  slice_min(Avg, n=conf$default_list_length)%>%
  H.F.plain(cap, H.clematis)}

D.bowlavg.wkt <- function(df, cap, howmany=10000) {df %>% 
    filter(W>0) %>%
     arrange(desc(W), R) %>%
    slice_max(W, n=howmany, with_ties = FALSE) %>%
     H.F.plain(cap, H.clematis) }

D.bowlavg.econ <- function(df, cap){df %>% 
  filter(!is.na(Name) & BB >= conf$qualify_balls_bowled) %>%
  slice_min(Econ, n=conf$default_list_length)%>%
  H.F.plain(cap, H.clematis)}

D.bowlavg.sr <- function(df, cap){df %>% 
  filter(!is.na(Name) & BB >= conf$qualify_balls_bowled) %>%
  slice_min(SR, n=conf$default_list_length)%>%
  H.F.plain(cap, H.clematis)}

D.bowlavg.most5wi <- function(df, cap){df %>%
    filter(!is.na(Name)) %>%
    arrange(Avg) %>%
    slice_max(`5wi`, n=conf$default_list_length, with_ties = FALSE)%>%
    H.F.plain(cap, H.clematis)}

D.bowl.fourfors <- function(df, cap) {df %>%
    filter(W>=4) %>% arrange(desc(W), R) %>%
    H.F.cyclamen( cap, H.cyclamen)}

D.bowl.fiveforlose <- function(df, cap, ms=5){df %>%
    filter(W >=ms & Res!="W" ) %>%
    slice_max(actuallyDate, n=conf$default_list_length) %>%
    H.F.cyclamen(cap,H.cyclamen)}

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
    H.F.cyclamen( cap, H.cyclamen)}

D.bowl.msteam <- function(df, cap, ms=4, team) { df %>%
 filter(W >=ms & fielding_team_id == team) %>% 
    slice_max(actuallyDate, n=conf$default_list_length) %>%
    H.F.cyclamen(paste(cap, "for", names(which(conf$teams_of_interest == team))), H.cyclamen)}

D.bowl.msdebut <- function(df, cap, ms=4){df %>%
    filter(!is.na(W)) %>%
    group_by(bowler_id, Name) %>%  slice_min(actuallyDate, n=1) %>% ungroup() %>%
    filter(W>=ms) %>%
    arrange(desc(actuallyDate)) %>%
    H.F.cyclamen(cap, H.cyclamen)}

D.bowl.nowkts <- function(df, cap){ df %>%
     filter(W ==0 ) %>%
    slice_max(R, n=conf$default_list_length) %>%
    H.F.cyclamen(cap, H.cyclamen)}

D.bowl.allmaidens <- function(df, cap, min=2){df %>%
    filter(O == M, O >= min) %>%
    arrange(desc(actuallyDate)) %>%
    H.F.cyclamen(cap, H.cyclamen)}

D.bowl.cheap5for <- function(df, cap, ms=5, min=10){df %>%
    filter(R < min, W >=ms, Name !="Unsure") %>% arrange(desc(W), R) %>%
    slice_max(W, n=conf$default_list_length, with_ties = F) %>%
    H.F.cyclamen(cap , H.cyclamen)}

D.bowl.bestanalysis <- function(df, cap){ df %>%
    arrange(desc(W), R) %>%
    slice_max(W, n=conf$default_list_length, with_ties = F) %>%
    H.F.cyclamen(cap, H.cyclamen)}

D.bowl.bestanalysistype <- function(df, cap, thistype){ df %>%
    filter(Type == thistype) %>% arrange(desc(W), R) %>%
    slice_max(W, n=conf$default_list_length, with_ties = F) %>%
    H.F.cyclamen(paste(cap, "for", thistype), H.cyclamen)}

D.bowl.mostwktstype <- function(df, cap, thistype) { df %>%
      select(bowler_id, W, Type, Name) %>% 
      group_by(Type, bowler_id, Name) %>% summarise(n = sum(W, na.rm=TRUE)) %>%
      filter(Type == thistype) %>%
        ungroup() %>%
        slice_max(n, n=conf$default_list_length) %>%
        H.F.plain(paste(cap, "for", thistype), H.mugwort) }

D.bowlavg.highavgwith5for <- function(df, cap){ df %>%
    filter(`5wi` >= 1) %>%
    slice_max(Avg, n=conf$default_list_length)%>%
    H.F.plain(cap, H.clematis)}    

D.allround.keeperbowling <- function(df, cap) {df %>%
    filter(`W-K` == TRUE, O >0) %>%
    arrange(desc(W), R) %>%
    slice_max(W, n=conf$default_list_length, with_ties = F) %>%
    rename(`Fielding Side` = Club,
           `Batting Club` = Oppo,) %>%
    H.F.cyclamen(cap , H.cyclamen)}    

D.bowlavg.nearbowlms <- function(df, myclub){ df %>%
    
    filter(club_id == myclub& !is.na(Name) & 
                           bowler_id %in% Y.currentplayers$player_id) %>% 
    filter((W >= 45 & W < 50) |(W >= 90 & W < 100) | (W >= 240 & W < 250)) %>%
    arrange(desc(W)) %>%
    H.F.plain("Career total wicket milestones", H.clematis) }

D.bowl.mostwktseason <- function(df, cap){df %>%
    filter(!is.na(W)) %>%
    group_by(bowler_id, Yr, Name) %>% summarise(`Total` = sum(W), `Matches` = n()) %>% ungroup() %>%
    rename(Season = Yr) %>% select(-bowler_id) %>% 
    slice_max(Total, n=conf$default_list_length) %>% 
    H.F.plain(cap, H.heather)}

D.bowl.bestavgseason <- function(df, cap, min=conf$qualify_inns_avgs){df %>%
    filter(fielding_club_id== conf$club_of_interest) %>%
    group_by(bowler_id, Yr, Name) %>%
    summarise(`Avg` = avRn(sum(R)/sum(W)), `Matches` = n(), Wkts = sum(W)) %>% 
    ungroup() %>% rename(Season = Yr) %>% filter(Matches >= min) %>% 
    select(-bowler_id) %>% 
    slice_min(Avg, n=conf$default_list_length) %>% 
    H.F.plain(cap, H.heatherAv)}

D.bowl.fast40 <- function(df, cap, ms=40){df %>%
    filter(!is.na(W), !is.na(bowler_id)) %>%
    select(Yr, actuallyDate, bowler_id, Name, W) %>% 
    group_by(Yr, bowler_id) %>% 
    arrange(actuallyDate) %>%              # Sort by date within each player
    mutate(cumulative_wkts = cumsum(W)) %>%  # Calculate cumulative runs for each player
    mutate(`Date Reached` = format(ymd(actuallyDate), "%e %b"),
           `Day of Year` = yday(actuallyDate)) %>%
    filter(cumulative_wkts >= ms) %>%  # Filter players who reach 500 runs
    slice(1) %>%  # Get the first match for each player to reach 500
    ungroup() %>%
    arrange(`Day of Year`) %>%
    select(Name, `Date Reached`, `Yr`) %>%
    H.F.plainALL(cap)}

D.bowl.manyextras <- function(df, cap, ms=50){df %>%
    filter(ExtPc >= ms) %>%
    arrange(desc(actuallyDate)) %>% slice_head(n=conf$default_list_length) %>%
    H.F.lilyfam(cap, H.lupin)}



# ==== team ====

D.resultsumm.resultsty <- function(df, cap) {df %>%
    filter(Yr==conf$year_of_interest) %>%
    arrange(desc(P), desc(W)) %>%
    H.F.plainALL(cap)}

D.resultsumm.results <- function(df, cap) {df %>%
  arrange(desc(Yr)) %>%
  H.F.plainALL(cap)}

D.resultsumm.mostwins <- function(df, cap) {df %>%
    slice_max(W, n=conf$default_list_length) %>%
    H.F.plainALL(cap)}

D.match.ties <- function(df, cap){df %>%
  filter(Result =="Tied") %>% arrange(desc(actuallyDate)) %>% 
  H.F.phloxfam(cap, H.phlox)}

D.match.matchview <- function(df, cap) {df %>%
    arrange(desc(actuallyDate)) %>%
    H.F.phloxfam(cap, H.phlox)}

D.match.highmatchagg <- function(df, cap){ df %>%
    slice_max(`Match Aggregate`, n=conf$default_list_length) %>% 
    H.F.freesia(cap, H.freesia)}

D.match.lowmatchagg <- function(df, cap){df %>%
  filter(!is.na(result_applied_to)
                     & `Match Aggregate` > 0 ) %>% 
  slice_min(`Match Aggregate`, n=conf$default_list_length) %>% 
  H.F.freesia(cap, H.freesia)}

D.match.short <- function(df, cap){ df %>%
    filter(!is.na(result_applied_to) & 
           `Match Aggregate` > 0 & matchAggDecOvs > 0) %>%
    slice_min(`matchAggDecOvs`, n=conf$default_list_length) %>% 
    H.F.freesia(cap, H.freesia)}

# D.inns.winmargin <- function(df, cap, whowon=1, marg=250, op= `>=`) {df %>%
#   filter(BatInnsWinner == whowon, Res == "W", op(Margin, .env$marg)) %>%
#   arrange(desc(actuallyDate)) %>%
#   H.F.hyacinthfam(cap, H.phlox)}

W.inns.winmargin <- function(df, whowon=1, marg=250, op= `>=`) {df %>%
    filter(BatInnsWinner == whowon, Res == "W", op(Margin, .env$marg)) %>%
    arrange(desc(actuallyDate)) }

D.inns.winmargin <- function(df, cap, whowon=1, marg=250, op= `>=`){
  W.inns.winmargin(df, whowon, marg, op) %>%
    H.F.phloxfam(cap, H.phlox)}

W.inns.dayagg <- function(df){df %>%
    group_by(batting_club_id, Date, `Batting Club`) %>%
    summarise(`Aggregate Runs` = sum(Total),
              `No of Inns` = n()) %>%
    arrange(desc(`Aggregate Runs`)) %>% ungroup() %>%
    select(-batting_club_id) %>%
    slice_head(n=conf$default_list_length)}
  
D.inns.dayagg <- function(df, cap){df %>%
    W.inns.dayagg() %>%
    H.F.plainALL(cap)}

D.inns.quotients <- function(dfU, dfT, cap) {
    
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
    
    
    a.Quots %>% kbl(longtable = T, booktabs = T, row.names = F, caption = cap) %>%
      kable_styling(latex_options = c("repeat_header", "striped", "hold_position"), 
                    font_size = 7) }

D.inns.teamquotients <- function(df, cap, theteam){
  us <- df %>% dplyr::filter(batting_team_id==theteam)
  them <- df %>% filter(fielding_team_id==theteam)
  D.inns.quotients(us, them, paste(cap, "for", names(which(conf$teams_of_interest == theteam))))}

D.inns.recentinns <- function(df, cap){df %>%
   slice_max(actuallyDate, n=conf$default_list_length) %>% 
    H.F.hyacinthfam(cap, H.hyacinth)} #was dianthus

D.inns.highinnstotal <- function(df, cap){df %>%
  slice_max(Total, n=conf$default_list_length ) %>% 
    H.F.dianthusfam(cap, H.dianthus)}

D.inns.lowinnstotal <- function(df, cap){df %>%
    filter(Res !="A"  & Res !="C" & Total != 0) %>%
  slice_min(Total, n=conf$default_list_length ) %>% 
  H.F.dianthusfam(cap, H.dianthus)}

D.inns.high2ndinns <- function(df, cap){df %>%
filter(innings_number==1 & `Inns of match` == 2) %>%
  slice_max(Total, n=conf$default_list_length ) %>% 
  H.F.dianthusfam(cap, H.dianthus)}

D.inns.highforone <- function(df, cap, ms=1){df %>%
  filter(W <= ms) %>% slice_max(Total, n=conf$default_list_length ) %>% 
  select(all_of(H.dianthus)) %>% 
  H.F.dianthusfam(cap, H.dianthus)}

D.inns.highchase <- function(df, cap){df %>%
    filter(`Inns of match` == 2 & Res == "W") %>%
    slice_max(Total, n=conf$default_list_length) %>% 
    H.F.dianthusfam(cap, H.dianthus)}

D.inns.highlosing <- function(df, cap){df %>%
    filter( Res == "L") %>%
    slice_max(Total, n=conf$default_list_length) %>% 
    H.F.dianthusfam(cap, H.dianthus)}

D.inns.lowhighestscore <- function(df, cap){df %>%
    filter(topscore>1, Res %in% c("W", "L", "T")) %>%
    slice_min(topscore, n=conf$default_list_length) %>%
    select(topscore, all_of(H.rudbeckia)) %>%
    H.F.plainALL(cap)}

D.inns.lowquot <- function(df, cap){df %>%
   filter( `Runs/Wkt`!=0) %>%
    slice_min(`Runs/Wkt`, n=conf$default_list_length ) %>%
    H.F.hyacinthfam(cap, H.hyacinth)}

D.inns.highquot <- function(df, cap){df %>%
    filter( `Runs/Wkt`!=0) %>%
    arrange(desc(`Runs/Wkt`), desc(Total)) %>%
    slice_max(`Runs/Wkt`, n=conf$default_list_length ) %>%
    H.F.hyacinthfam(cap, H.hyacinth)}

D.inns.mostextras <- function(df, cap){df %>%
    slice_max(Extras, n=conf$default_list_length ) %>% 
    H.F.dianthusfam(cap, H.rudbeckia)}

D.inns.tossresult <- function(df, cap){ df %>%
    drop_na(`Our toss result`) %>%
    select(actuallyDate, Result, `Our toss result`,
                  Date, `Match Summary`, Res) %>%
    count(`Our toss result`, Res) %>%
    pivot_wider(names_from = Res, values_from = n) %>%
    H.F.plainALL(cap)}

D.inns.manybowlers <- function(df, cap, ms=10){df %>%
    filter(`Bowlers used` >= ms) %>%
    arrange(desc(actuallyDate)) %>%
    rename(Bowlers = `Bowlers used`) %>%
    H.F.dianthusfam(cap, H.peony)}

D.inns.manywkttakers <- function(df, cap, ms=7){df %>%
   filter(`Wicket-takers` >= ms) %>%
    arrange(desc(actuallyDate)) %>% rename(Bowlers = `Wicket-takers`) %>%
    H.F.dianthusfam(cap, H.peony)}

D.inns.samebowlanaly <- function(df, cap, ms=3){df %>%
    filter(BWIA >= ms) %>%
    arrange(desc(actuallyDate)) %>% rename(Bowlers = BWIA) %>%
    H.F.dianthusfam(cap, H.peony)}

D.inns.hightotal4bowl <- function(df, cap){df %>%
    filter(`Bowlers used` == 4) %>% arrange(desc(Total)) %>% 
    rename(`Bowlers` = `Bowlers used`) %>% 
    slice_max(Total, n=conf$default_list_length ) %>%
    select(all_of(H.peony)) %>%
    H.F.dianthusfam(cap, H.peony)}

D.inns.hightotalnocent <- function(df, cap, ms=100){df %>%
    filter(topscore < ms) %>% 
    slice_max(Total, n=conf$default_list_length ) %>%
    H.F.hyacinthfam(cap, H.hyacinth)}

D.inns.manycentury <- function(df, cap, min=2){df %>%
     filter(numcenturies >= min) %>% 
    arrange(desc(actuallyDate)) %>%
    H.F.hyacinthfam(cap, H.hyacinth)}

D.inns.manyfifty <- function(df, cap, min=3){df %>%
    filter(numfifties >= min) %>% 
    arrange(desc(actuallyDate)) %>%
    H.F.hyacinthfam(cap, H.hyacinth)}

D.inns.allbatscontrib <- function(df, cap, score=10, num=6){df %>%
    filter(botscore >= score  & numbats >= num) %>% 
    arrange(desc(actuallyDate)) %>%
    H.F.hyacinthfam(cap, H.hyacinth)}

D.inns.lowindivid <- function(df, cap, score=15){df %>%
    filter(Res == "W", topscore != 0,  topscore <= score) %>% 
    arrange(desc(actuallyDate)) %>%
    H.F.hyacinthfam(cap, H.hyacinth)}

D.inns.lowindividhightotal <- function(df, cap, score=15){df %>%
    filter(topscore > 1,  topscore <= score) %>% 
    slice_max(Total, n=conf$default_list_length) %>%
    select(topscore, all_of(H.rudbeckia)) %>%
    H.F.plainALL(cap)}

D.inns.feastfamine <- function(df, cap){df %>%
    filter(topscore>50 & numtens==numfifties) %>% 
    slice_max(actuallyDate, n=conf$default_list_length) %>%
    select(topscore, all_of(H.rudbeckia)) %>%
    H.F.plainALL(cap)}

D.inns.many4for <- function(df, cap, min=2){df %>%
    filter(numfourfs >= min) %>% 
    slice_max(actuallyDate, n=conf$default_list_length ) %>%
    H.F.hyacinthfam(cap, H.hyacinth)}

D.inns.allwkttakers <- function(df, cap, min=2){df %>%
    filter(minwkts >= min) %>% 
    slice_max(actuallyDate, n=conf$default_list_length ) %>%
    H.F.hyacinthfam(cap, H.hyacinth)}

D.inns.equitbowl <- function(df, cap, tot=250, mx=50){df %>%
    filter(Total > tot & maxrunsccd < mx) %>%
    arrange(desc(actuallyDate)) %>%
    H.F.hyacinthfam(cap, H.hyacinth)}

D.inns.nobowlextras <- function(df, cap){df %>%
    filter( (Byes + `Leg byes`+ Wides + `No-balls`)==Extras, 
            Wides==0, `No-balls`==0) %>% 
    slice_max(actuallyDate, n=conf$default_list_length) %>% select(all_of(H.rudbeckia)) %>%
    H.F.plainALL(cap)}


# ==== allrounder =====

D.allround.matchdouble <- function(df, cap, Rns=30, Wms=3) {df %>%
    filter(Runs >= Rns & W >= Wms) %>% 
    arrange(desc(actuallyDate)) %>%
    H.F.violetfam(cap, H.violet)}


D.allround.fielddouble <- function(df, cap, Rns=50, D=3) {df %>%
    filter(Runs >= Rns & (Ct+ Std) >= D) %>% 
  arrange(desc(actuallyDate)) %>%
  H.F.violetfam(cap, H.violet)}


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
    H.F.plainALL(cap)}

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
    H.F.plain("Season double", H.rose)}

D.joinav.careerdouble <- function(df, cap, msBat, msBowl, msDis){df %>%
    filter(Runs >= msBat & W >= msBowl & (Ct+ Std) >= msDis) %>% 
    H.F.plain("cap", H.petuina)}

# ===== fielding ====


D.fielding.mostcatchesinns <- function(df, cap){df %>%
  arrange(desc(actuallyDate)) %>%
  slice_max(Ct, n=conf$default_list_length, with_ties = F ) %>% 
  H.F.lilyfam(cap, H.plum)}

D.fieldsumm.mostcatches <- function(df, cap, howmany=10000){df %>%
    arrange(desc(Ct)) %>% 
    slice_max(Ct, n=howmany, with_ties = F) %>%
    H.F.plain(cap, H.crocosmia)  }

D.fielding.totalcatchbytype <- function(df, cap, thistype="League") {df %>%
    select(player_id, Ct, Type, Name) %>%
    group_by(Type, player_id, Name) %>% summarise(n = sum(Ct, na.rm = TRUE)) %>%
    ungroup() %>% group_by(Type) %>% 
    slice_max(n, n = conf$default_list_length) %>% filter(Type==thistype) %>%
    ungroup() %>%
    select(-player_id, -Type)  %>%
    arrange(desc(n)) %>%  H.F.plain(paste(cap, thistype), H.mugwort)}

D.fieldsumm.mostdisperinns <- function(df, cap, min=conf$qualify_inns_avgs) { df %>%
    filter( M >= min, Ct > 0) %>%
  arrange(desc(`Dis/Inns`)) %>%
    slice_head(n=conf$default_list_length) %>%
    H.F.plain(cap, H.crocosmia)}

D.fielding.mostcatchseason <- function(df, cap){df %>%
    group_by(player_id, Yr, Name) %>% 
    summarise(`Total` = sum(Ct), `Matches` = n()) %>% ungroup() %>% 
    rename(Season = Yr) %>% select(-player_id) %>% 
    slice_max(Total, n=conf$default_list_length) %>% 
    H.F.plain(cap, H.heather)}

# ==== wicket-keeping =====

D.inns.nobyes <- function(df, cap, ms=150){ df %>%
    filter(Total >= ms & Byes == 0) %>% arrange(desc(Total))%>% 
    H.F.dianthusfam(cap, H.wolfsbane) }

D.fielding.mostblahinns <- function(df, cap, param){ df %>%
    filter(`W-K` == TRUE) %>%
    arrange(desc(actuallyDate)) %>% filter(.data[[param]] >0) %>%
    slice_max(.data[[param]], n=conf$default_list_length, with_ties = F ) %>% 
    H.F.lilyfam(cap, H.plum)}

D.fielding.mostblah <- function(df, cap, param, qual=3){ df %>%
    filter(`W-K` == TRUE) %>%
    group_by(player_id, Name) %>% summarise(n = sum(.data[[param]])) %>% ungroup() %>%
    filter(n >= qual) %>% arrange(-n) %>%
    H.F.plain(cap, H.mugwort)}

D.fielding.mostblahseason <- function(df, cap, param){df %>%
    filter(`W-K` == TRUE) %>%
    group_by(player_id, Yr, Name) %>% 
    summarise(`Total` = sum(.data[[param]]), `Matches` = n()) %>% ungroup() %>% 
    rename(Season = Yr) %>% select(-player_id) %>% 
    slice_max(Total, n=conf$default_list_length) %>% 
    H.F.plain(cap, H.heather)  }

D.fielding.totalWKdisbytype <- function(df, cap, thistype="League") {df %>%
    filter(`W-K` == TRUE) %>%
    select(player_id, Dis, Type, Name) %>%
    group_by(Type, player_id, Name) %>% summarise(n = sum(Dis, na.rm = TRUE)) %>%
    ungroup() %>% group_by(Type) %>% 
    slice_max(n, n = conf$default_list_length) %>% filter(Type==thistype) %>%
    ungroup() %>%
    select(-player_id, -Type)  %>%
    arrange(desc(n)) %>%  H.F.plain(paste(cap, thistype), H.mugwort)}

D.bat.amesfreeman <- function(df, cap){ 
  
  zxcA <- df %>% filter(`How Out` == "st", fielder_name!="Unsure", bowler_name!="Unsure", bowler_name!=fielder_name) %>% select(all_of(H.begonia), fielder_name, bowler_name) %>%
    mutate(Mode = paste("st ", fielder_name, " b ", bowler_name))
  
  zxcB <- df %>% filter(`How Out` == "st", fielder_name!="Unsure", bowler_name!="Unsure", bowler_name!=fielder_name) %>% select(all_of(H.begonia), fielder_name, bowler_name) %>%
    mutate(hoB = paste("st ", bowler_name, " b ", fielder_name))
  
  A.amesfreeman <- inner_join(zxcA, zxcB, by=c("Mode"= "hoB"), suffix= c("","y")) %>% 
    select('Date', 'Batting Side', 'Fielding Side', 'Name', "Mode")
  
  rm(zxcA, zxcB)
  
  A.amesfreeman %>% H.F.plain(cap, H.gladiolus)
}

D.fielding.mostappsseason <- function(df, cap, sort="Appearances"){ df %>%
    filter(!Name %in% c("T.B.C", "Unsure")) %>%
    group_by(player_id, Yr, Name) %>% 
    summarise(`Appearances` = n(),
              `As captain` = sum(Capt),
              `As keeper` = sum(`W-K`), 
              `As capt/w-k` = sum(Capt & `W-K`)) %>% ungroup() %>% 
    rename(Season = Yr) %>% select(-player_id) %>% 
    slice_max(.data[[sort]], n=2*conf$default_list_length) %>% 
    H.F.plainALL(cap)}

D.fielding.appsalltime <- function(df, cap, sort="Appearances") { df %>%
  filter(!Name %in% c("T.B.C", "Unsure")) %>%
  group_by(player_id, Name) %>% 
  summarise(`Appearances` = n(),
            `As captain` = sum(Capt),
            `As keeper` = sum(`W-K`), 
            `As capt/w-k` = sum(Capt & `W-K`)) %>% ungroup() %>% 
  select(-player_id) %>% 
  slice_max(.data[[sort]], n=2*conf$default_list_length) %>% 
  H.F.plainALL(cap)}

# ==== ground ====

D.inns.groundmostruns <- function(df, cap){ df %>%
    group_by(ground_id, Ground) %>% 
    summarise(TRuns = sum(Total, na.rm = TRUE)) %>%
    ungroup() %>% arrange(-TRuns) %>% drop_na() %>% select(n = TRuns, Name =Ground) %>%
    slice_max(n, n=conf$default_list_length) %>%
    H.F.plain(cap, H.mugwort)}

D.inns.groundhighteam <- function(df, cap){df %>%
    group_by(ground_id) %>%
    slice_max(Total) %>% ungroup() %>% drop_na(Ground) %>%
    slice_max(Total, n=conf$default_list_length) %>%
    H.F.phloxfam(cap, H.phlox)}

D.bat.groundhighindiv <- function(df, cap){ df %>%
    group_by(ground_id) %>%
    slice_max(Runs) %>% ungroup() %>% drop_na(Ground) %>%
    slice_max(Runs, n=conf$default_list_length) %>%
    H.F.lilyfam(cap, H.lily)}

D.bat.groundhighindivno50 <- function(df, cap){ df %>%
    group_by(ground_id) %>%
    slice_max(Runs) %>% ungroup() %>% drop_na(Ground) %>% 
    filter(Runs < 50) %>% arrange(Runs) %>%
    H.F.lilyfam(cap, H.lily)}
