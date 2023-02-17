
# profiling: bench::mark() example -----

# load packages
library(bench)

# initiate variables
x <- 1:10000
z <- 1.5
# approach 1: loop
multiplication <- 
     function(x,z) {
          result <- c()
          for (i in 1:length(x)) {result <- c(result, x[i]*z)}
          return(result)
     }
result <- multiplication(x,z)
head(result)

# approach II: "R-style"
result2 <- x * z 
head(result2)


# comparison
benchmarking <- 
     mark(
          result <- multiplication(x,z),
          result2 <- x * z, 
          min_iterations = 50 
     )
benchmarking[, 4:9]

# In addition, the `bench` package provides a simple way to visualize these outputs:
plot(benchmarking, type = "boxplot")



# profiling: profvis example -----


# load package
library(profvis)

# analyse performance of several lines of code
profvis({
     x <- 1:10000
     z <- 1.5
     # approach 1: loop
     multiplication <- 
          function(x,z) {
               result <- c()
               for (i in 1:length(x)) {result <- c(result, x[i]*z)}
               return(result)
          }
     result <- multiplication(x,z)
     
     # approach II: "R-style"
     result2 <- x * z 
     head(result2) 
})




## Writing efficient R code ------

## Memory allocation and growing objects

# a) naïve implementation
sqrt_vector <- 
     function(x) {
          output <- c()
          for (i in 1:length(x)) {
               output <- c(output, x[i]^(1/2))
          }
          
          return(output)
     }

# b) implementation with pre-allocation of memory
sqrt_vector_faster <- 
     function(x) {
          output <- rep(NA, length(x))
          for (i in 1:length(x)) {
               output[i] <-  x[i]^(1/2)
          }
          
          return(output)
     }

# compare performance
# the different sizes of the vectors we will put into the two functions
input_sizes <- seq(from = 100, to = 10000, by = 100)
# create the input vectors
inputs <- sapply(input_sizes, rnorm)

# compute outputs for each of the functions
output_slower <- 
     sapply(inputs, 
            function(x){ system.time(sqrt_vector(x))["elapsed"]
            }
     )
output_faster <- 
     sapply(inputs, 
            function(x){ system.time(sqrt_vector_faster(x))["elapsed"]
            }
     )
 
# load packages
library(ggplot2)

# initiate data frame for plot
plotdata <- data.frame(time_elapsed = c(output_slower, output_faster),
                       input_size = c(input_sizes, input_sizes),
                       Implementation= c(rep("sqrt_vector", length(output_slower)),
                                         rep("sqrt_vector_faster", length(output_faster))))

# plot
ggplot(plotdata, aes(x=input_size, y= time_elapsed)) +
     geom_point(aes(colour=Implementation)) +
     theme_minimal(base_size = 18) +
     theme(legend.position = "bottom") +
     ylab("Time elapsed (in seconds)") +
     xlab("No. of elements processed") 


## Vectorization

# implementation with vectorization
sqrt_vector_fastest <- 
     function(x) {
          output <-  x^(1/2)
          return(output)
     }

# speed test
output_fastest <- 
     sapply(inputs, 
            function(x){ system.time(sqrt_vector_fastest(x))["elapsed"]
            }
     )

# load packages
library(ggplot2)

# initiate data frame for plot
plotdata <- data.frame(time_elapsed = c(output_faster, output_fastest),
                       input_size = c(input_sizes, input_sizes),
                       Implementation= c(rep("sqrt_vector_faster", length(output_faster)),
                                         rep("sqrt_vector_fastest", length(output_fastest))))

# plot
ggplot(plotdata, aes(x=time_elapsed, y=Implementation)) +
     geom_boxplot(aes(colour=Implementation),
                  show.legend = FALSE) +
     theme_minimal(base_size = 18) +
     xlab("Time elapsed (in seconds)")


## apply-type functions

# get a list of all file-paths
textfiles <- list.files("data/twitter_texts", full.names = TRUE)
# prepare loop
all_texts <- lapply(textfiles, read.csv)
# combine all in one data frame
all_texts_df <- do.call("rbind", all_texts)


## SQL basics -----------------------

## SQL basics: R reference point

# import/prepare data
econ <- read.csv("data/economics.csv")
econ$date <-  as.Date(econ$date)

# filter
econ2 <- econ["1968-01-01"<=econ$date,]

# compute yearly averages (basic R approach)
econ2$year <- lubridate::year(econ2$date)
years <- unique(econ2$year)
averages <- sapply(years, FUN = function(x) mean(econ2[econ2$year==x,"unemploy"]))
output <- data.frame(year=years, average_unemploy=averages)

# inspect the first few lines of the result
head(output)




## SQL Joins

# import data
econ <- read.csv("data/economics.csv")
econ$date <-  as.Date(econ$date)

inflation <- read.csv("data/inflation.csv")
inflation$date <- as.Date(inflation$date)

# prepare variable to match observations
econ$year <- lubridate::year(econ$date)
inflation$year <- lubridate::year(inflation$date)

# create final output
years <- unique(econ2$year)
averages <- sapply(years, FUN = function(x) {
     mean(econ2[econ2$year==x,"unemploy"]/econ2[econ2$year==x,"pop"])*100
     
} )
unemp <- data.frame(year=years,
                    average_unemp_percent=averages)

# combine via the year column
# keep all rows of econ
output<- merge(unemp, inflation[, c("year", "inflation_percent")], by="year")


# inspect output
head(output)


