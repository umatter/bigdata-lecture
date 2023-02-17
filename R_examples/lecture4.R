# to install use
 devtools::install_github("cran/SparkR")

# load packages
library(SparkR)

# start session
sparkR.session()
# of if need be, specify 
# sparkR.session(sparkHome = "/home/umatter/.cache/spark/spark-3.1.2-bin-hadoop2.7")

# Import data and create a SparkDataFrame (a distributed collection of data, RDD)
flights <- read.df("data/flights.csv", source = "csv", header="true")
# inspect the object
class(flights)
head(flights)



## `SparkR`: Set data types


flights$dep_delay <- cast(flights$dep_delay, "double")
flights$dep_time <- cast(flights$dep_time, "double")
flights$arr_time <- cast(flights$arr_time, "double")
flights$arr_delay <- cast(flights$arr_delay, "double")
flights$air_time <- cast(flights$air_time, "double")
flights$distance <- cast(flights$distance, "double")


## `SparkR`: filter/select data
# filter
long_flights <- select(flights, "carrier", "year", "arr_delay", "distance")
long_flights <- filter(long_flights, long_flights$distance >= 1000)
head(long_flights)



## `SparkR`: aggregation
# aggregation: mean delay per carrier
long_flights_delays<- summarize(groupBy(long_flights, long_flights$carrier),
                                avg_delay = mean(long_flights$arr_delay))
head(long_flights_delays)


## `SparkR`: fetch result as data.frame
# Convert result back into native R object
delays <- collect(long_flights_delays)
class(delays)
delays


## SPARK & SQL IN R ------------------------------

# to install use
# devtools::install_github("cran/SparkR")

# load packages
library(SparkR)
# start session
sparkR.session()
# read data 
flights <- read.df("data/flights.csv", source = "csv", header="true")

# register the data frame as a table
createOrReplaceTempView(flights, "flights" )

# now run SQL queries on it
long_flights2 <- sql("SELECT DISTINCT carrier,
                             year, 
                             arr_delay,
                             distance
                            FROM flights 
                            WHERE 1000 <= distance")
head(long_flights2)

