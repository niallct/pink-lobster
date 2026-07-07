# ==== average making functions ====

# the function #TODO tidy this up ' #TODO fix why so many rows K.batting %>% select(batsman_id, batting_club_id) %>%  distinct( ) %>%  drop_na(batsman_id) %>%  nrow()
makeBatAvgs <- function(batdata, mode='c') { #MODES: c group by player and club, p player only, a no grouping
  
  # batting avg table also includes fielding:
  if(mode=='c'){ adF <- batdata %>% drop_na(fielder_id, fielding_club_id) %>% group_by(fielder_id, fielding_club_id) 
  }  else  if(mode=='a'){adF <- batdata
  }  else {adF <- batdata %>% drop_na(fielder_id) %>% group_by(fielder_id) }
  
  adamField <- adF %>% summarise(
    Ct = sum(`How Out` == "ct"),
    Std = sum(`How Out` == "st"),
    RO = sum(`How Out` == "run out")) %>%  ungroup()
  
  zxc <- batdata %>% drop_na(Runs) %>%  mutate(RunsO = Runs) 
  
  if(mode=='c'){ zxc <- zxc %>% drop_na(batsman_id, batting_club_id) %>% group_by(batsman_id, batting_club_id) 
  }  else  if(mode=='a'){zxc <- zxc
  }  else {zxc <- zxc %>% drop_na(batsman_id) %>% group_by(batsman_id) }
  
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
    left_join(Y.clubs2) %>% rename(`Club` = club_name)
  }  else if(mode=='a'){xcv <- xcv
  }  else {xcv <- xcv %>%   left_join(adamField, by = c("batsman_id" = "fielder_id"))}
  
  if(mode!="a"){outputBatAvgs <- xcv %>%
    left_join(Y.plrs, by = c("batsman_id" = "player_id")) %>%
    mutate(
      Ct = replace_na(Ct, 0),
      Std = replace_na(Std, 0),
      RO = replace_na(RO,0)
    ) } else {outputBatAvgs <- xcv}
  
  return(outputBatAvgs)
}

# the function #TODO tidy this
makeBowlAvgs <- function(bowldata, mode="c") { #MODES: c group by player and club, *p player only, a no grouping
  
  if(mode=='c'){ zxc <- bowldata %>% drop_na(bowler_id, fielding_club_id) %>% group_by(bowler_id, fielding_club_id)
  }  else  if(mode=='a'){zxc <- bowldata 
  }  else {zxc <- bowldata %>% drop_na(bowler_id) %>% group_by(bowler_id)  }
  
  foo <- zxc %>%  slice_max(W) %>% 
    slice_min(R, n=1, with_ties = F) %>% ungroup()
  
  if(mode=='a'){BestBowl  <- foo %>% select(Analy)
  } else if(mode=='c'){BestBowl  <- foo %>% select(c(bowler_id, Analy, fielding_club_id))
  } else {BestBowl  <- foo %>% select(c(bowler_id, Analy))}
  
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
    left_join(BestBowl, by = join_by(bowler_id, fielding_club_id)) %>%
    rename(Best = Analy) %>%
    left_join(Y.plrs, by = c("bowler_id" = "player_id")) %>%
    rename(club_id=fielding_club_id) %>%
    mutate( is_us = case_when(
      club_id == conf$club_of_interest ~ TRUE,
      club_id != conf$club_of_interest ~ FALSE
    )) %>% left_join(Y.clubs2) %>% rename(`Club` = club_name) }
  else {if(mode=='a'){outputbowlAvg <- xcv %>% cbind(BestBowl) %>% rename(Best = Analy)}
    else {outputbowlAvg <- xcv %>%
      left_join(BestBowl, by = join_by(bowler_id)) %>%
      rename(Best = Analy) %>%
      left_join(Y.plrs, by = c("bowler_id" = "player_id") ) } }
  
  return(outputbowlAvg)
}


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