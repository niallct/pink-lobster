# Cricketr functions which look useful
-   batsmanDismissals
-   batsmanPerfBoxHist
-   batsmanPerfHomeAway
-   bowlerWktsRunsPlot
-   bowlerMovingAverage

## sort on data side
-   automate downloading, unzipping
-   loading player data: capt, keeper
-   dump out a list of our players -- also teams?
-   somesort of thing to autosuggest competitions of interest
-   generate team, club, ground etc lookup tables from the main match data
-   bring RO to the fielding table
-   match data: lift out home and away totals, ie to give a lovely match summary
-   something about a short result statement HW, AW, D, A, etc?

## features to file/do
-   total runs, wkts, run/wkt for, against; each XI
-   bowling stats -- % of balls tht are wd/nb, % of maidens
-   batter performance by ground
-   tossing performance
-   a lookup, all matches between CoI and an arbitrary club
-   a pretty way to show arbitrary filtered ds - bat, etc - eg with ids stripped
-   our and opponents recent innings should be matches presented in a nicer format
-   fastest fifty, hundred
-   most runs in a year
-   most wkts in a year
-   most ct, std in a year
-   fifty, four-for on debut
-   three or more hundreds in successive innings
-   Highest total without any individ centrury
-   winning with no-one in double figures
-   six (most?) fifties in an inns
-   8 (most?) wicket-takers in an inns
-   ten bowlers used
-   bowlers with same analysis in a match
-   add to outstanding analysis: O = M, B.bowling %\>% filter( is_us == TRUE, R \< 10, W \>=5)
-   fix the longest run stuff to show all tied records rather than the nth
-   consecutively losing the toss (but data are poor)
-   Batting first win %age per ground, weather
-   first inns total analysis by ground
-   one batter outscoring the whole opposition total
-   bowling though (O \>= floor(inns_ovs %/% 2)); for COI, current year
-   distribution of first-innings totals, by match type (20, 40, 45 ov), competition, ground, etc;
    -   histo of bins, logistic regression
-   personal (for this, all clubs) by XI, match type, day of week; ground, H/A
-   exclude retired players from approaching milestones
-   make it behave nicely when there is no data in chosen year of interest
-   a nicer way to present a match result summary
-   Add stars to not-outs in batting section
-   What do do about duplicate etc team records which are actually the same person?
-   Postcode of grounds
-   some sort of score / win predictor
-   Exclude youth teams (or at least mark somehow)
-   possible to get overs/balls for each partership?
-   Same player across multiple teams -- work out overall averages? playing for most sides?
-   Get date the azure set was last changed and last downloaded
-   plots -- our scores, wins, bat first, home/away
-   fix the unusual dismissals spurious data

## Wicketkeeping / allrounder
-   most wk catches
-   most catches fieldr
-   most matches as captain
-   most appearances
-   100 runs and 5 wkts
-   50 runs and 4 wkts
-   50 tuns and 3 catches

## Partnerships
-   top 5 for each wicket, home and away, by XI
-   monopolising runs in a partnership
