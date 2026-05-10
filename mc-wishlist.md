= sort new loader: compos, leagues, summary, teams, matches!; bring in good bits from parse-tidy
- do something with data last updated?
= scorecard links in the multi-milestone/interesting innings lists
= improve the match result predictor, use recorded result_applied_to
- sort out filter vs display functions
- tidy up column selection
- tidy up column widths
- add rectangle geom to curator plots
- Fewest games played by nominal captain
- Longest wait for a captain to play in a season 
- Longest run of losses, not wins, as captain 
- Winning toss Vs winning match, by captain 
- Captain playing not in their own XI
- Handle 8-ball overs properly
= fix cap numbers duplication
= globally tidy up ground names to deduplicate
- work out excluding teams from reports but include, so the result_club tool works

## acs123:
                own team  O M R W                    Opp tot   Ct
 gloucs vs notts, gloucester, june 6 & 7. Notts won by 8 wkts
 c Hallam b Gunn  29  122  9 5 11 2    T Simpson b    167  2
                                       Hemingway b Dennet
 b T G Wass       10  177                            135/2 1
 (did not bat      - )

# bigdata notes
no more list of players by club, so no files to go through. 
Build a lookup using the matchplayers table. So E.players and E.playersNC are now Y.playersclub2 and Y.players2
But that is now all using all years -- TODO add the year last played to the players tables
TODO matchcompos - work out why this is used, can it be replaced?
TODO why are there strange years in the new big ds?
Looks to be a bit messy on the abandoned/cancelled frings, using the p-c code. Could fiddle if needed.
