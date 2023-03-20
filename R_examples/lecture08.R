
# Aggregation ----------------------

# load packages
library(ff)
library(ffbase)

# set up the ff directory (for data file chunks)
if (!dir.exists("fftaxi")){
     system("mkdir fftaxi")
}
options(fftempdir = "fftaxi")

# import the first one million observations
taxi <- read.table.ffdf(file = "data/tlc_trips.csv",
                        sep = ",",
                        header = TRUE,
                        next.rows = 100000,
                        # colClasses= col_classes,
                        nrows = 1000000
)

# inspect the factor levels
levels(taxi$Payment_Type)


# recode them
levels(taxi$Payment_Type) <- tolower(levels(taxi$Payment_Type))
taxi$Payment_Type <- ff(taxi$Payment_Type,
                        levels = unique(levels(taxi$Payment_Type)),
                        ramclass = "factor")
# check result
levels(taxi$Payment_Type)


# load packages
library(doBy)

# split-apply-combine procedure on data file chunks
tip_pcategory <- ffdfdply(taxi,
                          split = taxi$Payment_Type,
                          BATCHBYTES = 100000000,
                          FUN = function(x) {
                               summaryBy(Tip_Amt~Payment_Type,
                                         data = x,
                                         FUN = mean,
                                         na.rm = TRUE)})


# inspect the result
as.data.frame(tip_pcategory)


# add relative measure, and complete aggregation exercise
taxi$percent_tip <- (taxi$Tip_Amt/taxi$Total_Amt)*100

# recompute the aggregate stats
tip_pcategory <- ffdfdply(taxi,
                          split = taxi$Payment_Type,
                          BATCHBYTES = 100000000,
                          FUN = function(x) {
                               # note the difference here
                               summaryBy(percent_tip~Payment_Type, 
                                         data = x,
                                         FUN = mean,
                                         na.rm = TRUE)})

# show result as data frame
as.data.frame(tip_pcategory)


table.ff(taxi$Payment_Type)


# select the subset of observations only containing trips paid by
# credit card or cash
taxi_sub <- subset.ffdf(taxi, Payment_Type=="credit" | Payment_Type == "cash")
taxi_sub$Payment_Type <- ff(taxi_sub$Payment_Type,
                            levels = c("credit", "cash"),
                            ramclass = "factor")

# compute the cross tabulation
crosstab <- table.ff(taxi_sub$Passenger_Count,
                     taxi_sub$Payment_Type
)
# add names to the margins
names(dimnames(crosstab)) <- c("Passenger count", "Payment type")
# show result
crosstab



# install.packages(vcd)
# load package for mosaic plot
library(vcd)

# generate a mosaic plot
mosaic(crosstab, shade = TRUE)


