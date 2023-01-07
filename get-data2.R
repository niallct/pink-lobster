#Playing around getting data in
library(AzureCosmosR)
library(yaml)

conf <- yaml.load_file("./config/db-key.yaml")
dbkey = conf$key


C.endp <- cosmos_endpoint("https://mcc-play-cricket.documents.azure.com:443/", 
                        key=dbkey)

list_cosmos_databases(C.endp)

C.db.2 <- get_cosmos_database(C.endp,"play-cricket-v2")

list_cosmos_containers(C.db.2)

C.club.2 <- get_cosmos_container(C.endp, "play-cricket-v2", "clubs")
D.club.2 <- query_documents(C.club.2, "select * from clubs")

C.competition.2 <- get_cosmos_container(C.endp, "play-cricket-v2", "competitions")
D.competition.2 <- query_documents(C.competition.2, "select * from competitions")

C.ground.2 <- get_cosmos_container(C.endp, "play-cricket-v2", "grounds")
D.ground.2 <- query_documents(C.ground.2, "select * from grounds")

C.league.2 <- get_cosmos_container(C.endp, "play-cricket-v2", "leagues")
D.league.2 <- query_documents(C.league.2, "select * from leagues")

C.match.2 <- get_cosmos_container(C.endp, "play-cricket-v2", "matches")
D.match.2 <- query_documents(C.match.2, "select * from matches")

C.people.2 <- get_cosmos_container(C.endp, "play-cricket-v2", "people")
D.people.2 <- query_documents(C.people.2, "select * from people")

C.team.2 <- get_cosmos_container(C.endp, "play-cricket-v2", "teams")
D.team.2 <- query_documents(C.team.2, "select * from teams")

# clubs 561
# competitions 539
# grounds 449
# leagues 44
# matches 8611
# people 20094
# teams 1759