#Defintiions of column headers
# ---- full on match level ----
# define what we want as match headers -- drop umpire names and such
H.chickpea <- c("match_id", "match_date", "match_time", "game_type", "competition_type", "match_type",
                "ground_id", "ground_name", "competition_id", "competition_name", "league_id", "league_name", 
                "home_team_id", "home_team_name", "home_club_id", "home_club_name", 
                "away_team_id", "away_team_name", "away_club_id", "away_club_name", 
                "no_of_overs", "batted_first", "no_of_players", "no_of_innings",
                "toss", "result", "result_description", "result_applied_to", "toss_won_by_team_id",
                "players", "innings") #old one is morning-glory.

# ---- Results tables ----
H.oxeye <- c("Yr", "P", "W", "L", "D", "T", "A")
H.oxeye.XI <- c("Yr", "Our Team", "P", "W", "L", "D", "T", "A")
# ==== Sets of headers to select the nicest things ====
## ---- match level ----
H.mallow <- c("Match Summary", "Date", "Result",
              "Type", "Ground", "Home Side", "Away Side",
              "Home Club", "Away Club",
              "Match Aggregate", "Match Overs")

#Angel:
H.phlox <- c("Match Summary", "Date", "Result",
             "Type", "Ground")

#Honey:
H.hyacinth <- c("Match Summary", "Date", "Result")

#Lamington:
# H.freesia <- c("Home Club", "Away Club", "Date", "Ground", "Match Aggregate",
#                "Match Overs")

H.freesia <- c("Match Summary", "Date", "Ground", "Match Aggregate",
               "Match Overs")

# these next two get used in the load script so watch for duplication
H.morningglory <- c("match_id", "match_date", "ground_name", "ground_id", "competition_id","league_id",
                    "competition_type", "match_type", "no_of_overs", "batted_first",
                    "home_team_name", "home_team_id", "home_club_name", "home_club_id",
                    "away_team_name", "away_team_id", "away_club_name", "away_club_id", 
                    "toss", "result_description", "result_applied_to", "toss_won_by_team_id")

H.cornflower <- c("match_id", "match_date", "competition_id","league_id", "league_name",
                  "competition_name", "competition_type", "match_type", "no_of_overs",
                  "Result", "home_team_id", "away_team_id")

## ---- innings level ----

H.impatiens <- c("Match Summary", "Date", "Result",
                 "Type", "Ground", #"Home Side", "Away Side",
                 "Batting Side", "Score", "Ovs",  "Fielding Side", "Oppo Score",
                 "Extras", "Byes" ,"Leg byes", "Wides", "No-balls", "% Extras", 
                 "Inns of match", 
                 #"Bowlers",
                 "Res", "Ven")

#Madeira:
H.dianthus  <- c("Batting Side", "Score", "Ovs", "Date", "Fielding Side", "Ground", "Res")
H.dahlia  <- c("Batting Team", "Score", "Ovs", "Date", "Fielding Side", "Ground", "Res")
H.rudbeckia <- c("Batting Side", "Score", "Ovs", "Date", "Fielding Side", "Ground", "Res", "Extras")
#H.wolfsbane <- c("Batting Side", "Score", "Ovs", "Date", "Fielding Side", "Ground", "Res", "Byes")
H.peony     <- c("Batting Side", "Score", "Ovs", "Date", "Fielding Side", "Ground", "Res", "Bowlers")

## ---- batting level ----
H.begonia <- c("Match Summary", "Date", "Result",
               "Type", "Ground", #"Home Side", "Away Side",
               "Batting Side", "Score", "Ovs",  "Fielding Side", "Oppo Score",
               #"Extras", "Byes" ,"Leg byes", "Wides", "No-balls", "% Extras", 
               "Inns of match", "Fielding Club", "Team", "Batting Club",
               "Res", "Ven",
               "Name", "Runs", "Balls", "SR", "How Out", "4", "6", "Contrib")


#H.aspidistra <-  c("Name", "Runs", "Balls", "SR", "Date", "Type", "Fielding Club", "Ground", "Ven")
#H.lotus <-       c("Name", "Runs", "Balls", "SR", "Date", "Team", "Fielding Club", "Ground")
# H.lilyRED <-     c("Name", "Runs", "Team", "Fielding Club", "Date")

# Carrot:
H.lily <-        c("Name", "Runs", "Balls", "SR", "Date", "Team",          "Fielding Club", "Ground", "Res")
H.comfrey <-     c("Name", "Runs", "Balls", "SR", "Date", "Batting Side" ,                  "Ground", "Res")
H.hibiscus <-    c("Name", "Runs", "Balls", "SR", "Date", "Team",          "Fielding Club", "Ground", "4", "6")
H.thistle <-     c("Name", "Runs", "Balls", "SR", "Date", "Contrib",       "Fielding Club", "Oppo Score" )

#Lemon:
H.safflower <-   c("Name", "Runs", "Balls", "SR", "Date", "Batting Club", "Fielding Club", "Ground", "How Out") 

## ---- partnership ----

# Battenberg:
H.primrose <-   c("Date", "Batting Team", "Fielding Side", "Wkt", "Partnership", "Batter Out", "Batter Rem", "Ven")
H.periwinkle <- c("Date", "Batting Team", "Fielding Side", "Wkt", "Partnership", "Batter Out", "Batter Rem", "Fall",
                  "Score", "Oppo Score")

## ---- bowling ----

H.lilac <- c("Match Summary", "Date", "Result",
             "Type", "Ground", #"Home Side", "Away Side",
             "Batting Side", "Score", "Ovs",  "Fielding Side", "Oppo Score",
             #"Extras", "Byes" ,"Leg byes", "Wides", "No-balls", "% Extras", 
             "Inns of match", "Batting Club", "Fielding Club",
             "Res", "Ven",
             "Name", "O", "M", "R", "W", "Wd", "NB", "Avg", "SR", "Econ")

#Parkin:
H.cyclamen <-   c("Name", "O", "M", "R", "W", "Date",  "Team", "Batting Club", "Ground", "Res")
H.lupin <-      c("Name", "O", "M", "R", "W", "Date",  "Team", "Batting Club", "Ground", "Wd", "NB")
H.cyclamen2 <-  c("Name", "O", "M", "R", "W", "Date",  "Fielding Club", "Batting Club", "Ground", "Res")
#H.buttercup <-  c("Fielding Side", "O", "M", "R", "W", "Date", "Batting Club", "Ground", "Res")

## ---- fielding ----
H.foxglove <- c("Match Summary", "Date", "Result",
                "Type", "Ground", 
                "Fielding Club", "Batting Club", #"Home Side", "Away Side",
                # "Batting Side", "Score", "Ovs",  "Fielding Side", "Oppo Score",
                #"Extras", "Byes" ,"Leg byes", "Wides", "No-balls", "% Extras", 
                # "Inns of match", #TODO add these
                #  "Res", "Ven", ##TODO add these
                "Batting Team", "Fielding Team",
                "Name", "Ct", "Std", "RO")

#Lemon:
H.plum <-   c("Name", "Ct", "Std", "RO", "Date", "Fielding Club", "Batting Club", "Ground") 
H.poppy <-   c("Name", "Ct", "Std", "RO", "Date", "Fielding Team", "Batting Club", "Ground") 

## ---- allrounder ----
H.waterlily <- c("Match Summary", "Date", "Result",
                 "Type", "Ground", 
                 "Name", "Club", "Runs", "Balls", "BatSR", "How Out", "4", "6", "Contrib",
                 "O", "M", "R", "W", "Avg", "BowlSR", "Econ", "Ct", "Std", "Capt", "W-K") 

#Gingerbread:
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

# agapanthus, alyssum, amaranth, azalea, bluebell, buddleia, carnation, chrysanthemum, clover,  cowslip, crocus, daffodil, daisy, delphinium, echinacea, feverfew, fuchsia, gardenia, geranium, gerbera, hollyhock, honeysuckle, hydrangea, ipomoea, iris, jasmine, lavender, lotus, magnolia, marigold, narcissus, nigella, orchid, pampas, snapdragon, snowdrop, sunflower, sweetpea,tulip, verbena, wisteria, zinna

## ---- averages and totals ----
H.aster <-     c("Name",          "Runs", "Inns", "NO", "Avg", "HS", "SR", "50", "100", "Ct", "Std", "RO")
H.asterA <-     c(                "Runs", "Inns", "NO", "Avg", "HS", "SR", "50", "100")
H.asterC <-     c("Name", "Club", "Runs", "Inns", "NO", "Avg", "HS", "SR", "50", "100", "Ct", "Std", "RO")
H.buckthorn <- c("Name",          "Runs", "Inns", "NO", "Avg", "HS", "6s", "4s")
H.clematis <-  c("Name",          "O", "M", "R", "W", "5wi", "Avg", "Econ", "SR", "Best")
H.clematisA <-  c(                "O", "M", "R", "W", "5wi", "Avg", "Econ", "SR", "Best")
#H.clematisC <-  c("Name", "Club", "O", "M", "R", "W", "5wi", "Avg", "Econ", "SR", "Best")
H.petuina <- c("Name", "Runs", "BatAvg", "W", "BwlAvg", "Ct", "Std")
H.rose <- c("Name", "Season",  "Runs",   "Wkts", "Dis", "Matches")
H.crocosmia <-   c("Name", "M", "Ct", "Std", "RO", "Dis/Inns")
H.mugwort <- c("Name", "n")
H.mimosa <- c("Name", "n", "Innings")
H.yarrow <- c("Season", "Name", "Matches")
H.heather <- c("Season", "Name", "Total", "Matches")
H.heatherAv <- c("Season", "Name", "Avg", "Matches", "Wkts")
H.gladiolus <- c('Date', 'Batting Side', 'Fielding Side', 'Name', "Mode")