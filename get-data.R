#Playing around getting data in
library(AzureCosmosR)
library(yaml)

conf <- yaml.load_file("./config/db-key.yaml")
dbkey = conf$key


C.endp <- cosmos_endpoint("https://mcc-play-cricket.documents.azure.com:443/", 
                        key=dbkey)

list_cosmos_databases(C.endp)

C.db <- get_cosmos_database(C.endp,"play-cricket")

list_cosmos_containers(C.db)

C.club <- get_cosmos_container(C.endp, "play-cricket", "clubs")
D.club <- query_documents(C.club, "select * from clubs")

C.competition <- get_cosmos_container(C.endp, "play-cricket", "competitions")
D.competition <- query_documents(C.competition, "select * from competitions")

C.ground <- get_cosmos_container(C.endp, "play-cricket", "grounds")
D.ground <- query_documents(C.ground, "select * from grounds")

C.league <- get_cosmos_container(C.endp, "play-cricket", "leagues")
D.league <- query_documents(C.league, "select * from leagues")

C.match <- get_cosmos_container(C.endp, "play-cricket", "matches")
D.match <- query_documents(C.match, "select * from matches")

C.people <- get_cosmos_container(C.endp, "play-cricket", "people")
D.people <- query_documents(C.people, "select * from people")

C.team <- get_cosmos_container(C.endp, "play-cricket", "teams")
D.team <- query_documents(C.team, "select * from teams")

# clubs 561
# competitions 539
# grounds 449
# leagues 44
# matches 8611
# people 20094
# teams 1759