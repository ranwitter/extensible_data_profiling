library (RPostgreSQL)
#loads the PostgreSQL driver
drv <- dbDriver ("PostgreSQL")
#Open a connection
con <- dbConnect (drv , dbname = "wildlife" , user = "postgres" , password = "postgres" , host = "localhost")
#Listing tables
dbListTables(con)



