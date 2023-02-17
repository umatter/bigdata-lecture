# ff intro
#SET UP --------------

# install.packages(c("ff", "ffbase"))
# load packages
library(ff)
library(ffbase)
library(pryr)

# create directory for ff chunks, and assign directory to ff 
system("mkdir ffdf")
options(fftempdir = "ffdf")

# Import data, inspect change in RAM.
mem_change(
     flights <- 
          read.table.ffdf(file="data/flights.csv",
                          sep=",",
                          VERBOSE=TRUE,
                          header=TRUE,
                          next.rows=100000,
                          colClasses=NA)
)



## Chunking data with the `ff`-package
# Inspect file chunks on disk and data structure in R environment.

# show the files in the directory keeping the chunks
list.files("ffdf")

# investigate the structure of the object created in the R environment
summary(flights)




## Memory mapping with `bigmemory` 
# SET UP ----------------

# load packages
library(bigmemory)
library(biganalytics)


# Import data, inspect change in RAM.
flights <- read.big.matrix("data/flights.csv",
                           type="integer",
                           header=TRUE,
                           backingfile="flights.bin",
                           descriptorfile="flights.desc")


## Memory mapping with `bigmemory`
#Inspect the imported data.
summary(flights)
## Memory mapping with `bigmemory`
#Inspect the object loaded into the R environment.
flights





system("mkdir ffdf")
options(fftempdir = "ffdf")

# load packages
library(ff)
library(ffbase)
library(pryr)

# fix vars
FLIGHTS_DATA <- "data/flights_sep_oct15.txt"
AIRLINES_DATA <- "data/airline_id.csv"


system.time(flights.ff <- read.table.ffdf(file=FLIGHTS_DATA,
                                          sep=",",
                                          VERBOSE=TRUE,
                                          header=TRUE,
                                          next.rows=100000,
                                          colClasses=NA))

airlines.ff <- read.csv.ffdf(file= AIRLINES_DATA,
                             VERBOSE=TRUE,
                             header=TRUE,
                             next.rows=100000,
                             colClasses=NA)



subs1.ff <- subset.ffdf(flights.data.ff, CANCELLED == 1, 
                        select = c(FL_DATE, AIRLINE_ID, 
                                   ORIGIN_CITY_NAME,
                                   ORIGIN_STATE_NM,
                                   DEST_CITY_NAME,
                                   DEST_STATE_NM,
                                   CANCELLATION_CODE))

