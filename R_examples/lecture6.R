

# load packages
library(RSQLite)
library(data.table)

# initiate the database
con_air <- dbConnect(SQLite(), "data/air.sqlite")

# import data into current R sesssion
flights <- fread("data/flights.csv")
airports <- fread("data/airports.csv")
carriers <- fread("data/carriers.csv")

# add tables to database
dbWriteTable(con_air, "flights", flights)
dbWriteTable(con_air, "airports", airports)
dbWriteTable(con_air, "carriers", carriers)



# define query
delay_query <-
     "SELECT 
year,
month, 
day,
dep_delay,
flight
FROM (flights INNER JOIN airports ON flights.origin=airports.iata) 
INNER JOIN carriers ON flights.carrier = carriers.Code
WHERE carriers.Description = 'United Air Lines Inc.'
AND airports.airport = 'Newark Intl'
ORDER BY flight
LIMIT 10;
"


# issue query
delays_df <- dbGetQuery(con_air, delay_query)
delays_df




install.packages("bigrquery")
# load packages, credentials
library(data.table)
library(DBI)

# fix vars
BILLING <- "onlinemedia-slant" # the project ID on BigQuery (billing must be enabled)
PROJECT <- "bigquery-public-data" # the project name on BigQuery
DATASET <- "google_analytics_sample"

# connect to DB on BigQuery
con <- dbConnect(
  bigrquery::bigquery(),
  project = PROJECT,
  dataset = DATASET,
  billing = BILLING
)




# run query
#query <- "SELECT * FROM `bigquery-public-data.google_analytics_sample.ga_sessions_20170801`"
query <-
  "
SELECT  totals.visits, totals.transactions, trafficSource.source, device.browser, device.isMobile, geoNetwork.city, geoNetwork.country, channelGrouping
  FROM
    `bigquery-public-data.google_analytics_sample.ga_sessions_*`
  WHERE
    _TABLE_SUFFIX BETWEEN '20160101'
    AND '20171231';
"

ga <- as.data.table(dbGetQuery(con, query, page_size=15000))





# AWS RDS---------------------

## AWS RDS ---------------

# Set up DB
# load packages
library(RMySQL)

# fix vars
RDS_ENDPOINT <- readLines("_keys/aws_rds.txt")[1]
PW <- readLines("_keys/aws_rds.txt")[2]

# connect to DB
con_rds <- dbConnect(RMySQL::MySQL(),
                     host=RDS_ENDPOINT,
                     port=3306,
                     username="admin",
                     password=PW)

# initiate a new database on the MySQL RDS instance
dbSendQuery(con_rds, "CREATE DATABASE IF NOT EXISTS air")

# disconnect and re-connect directly to the new DB
dbDisconnect(con_rds)
con_rds <- dbConnect(RMySQL::MySQL(),
                     host=RDS_ENDPOINT,
                     port=3306,
                     username="admin",
                     dbname="air",
                     password=PW)


# Query DB
# define query
delay_query <-
     "SELECT 
year,
month, 
day,
dep_delay,
flight
FROM (flights INNER JOIN airports ON flights.origin=airports.iata) 
INNER JOIN carriers ON flights.carrier = carriers.Code
WHERE carriers.Description = 'United Air Lines Inc.'
AND airports.airport = 'Newark Intl'
ORDER BY flight
LIMIT 10;
"

# issue query
delays_df <- dbGetQuery(con_rds, delay_query)
delays_df
