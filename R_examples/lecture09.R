## ----warning=FALSE, out.width="75%", fig.align='center'----------------------------------------------
# load package
library(ggplot2) # for plotting
library(pryr) # for profiling
library(bench) # for profiling
library(fs) # for profiling

# random numbers generation
x <- rnorm(10^6, mean=5)
y <- 1 + 1.4*x + rnorm(10^6)
plotdata <- data.frame(x=x, y=y)
object_size(plotdata)

# generate scatter plot
splot <-
     ggplot(plotdata, aes(x=x, y=y))+
     geom_point()
object_size(splot)



## ----out.width="75%", fig.align='center'-------------------------------------------------------------
mem_used()
system.time(print(splot))
mem_used()


## ----out.width="75%", fig.align='center'-------------------------------------------------------------
ggsave("splot.pdf", device="pdf", width = 5, height = 5)
file_size("splot.pdf")


## ----out.width="75%", fig.align='center'-------------------------------------------------------------
mem_used()
system.time(print(splot))
mem_used()


## ----out.width="75%", fig.align='center'-------------------------------------------------------------
ggsave("splot.pdf", device="pdf", width = 5, height = 5)
file_size("splot.pdf")


## ----out.width="75%", fig.align='center'-------------------------------------------------------------
# generate scatter plot
splot2 <-
     ggplot(plotdata, aes(x=x, y=y))+
     geom_point(pch=".")


## ----out.width="75%", fig.align='center'-------------------------------------------------------------
mem_used()
system.time(print(splot2))
mem_used()


## ----out.width="75%", fig.align='center'-------------------------------------------------------------
# install.packages("scattermore")
library(scattermore)
# generate scatter plot
splot3 <-
     ggplot()+
     geom_scattermore(aes(x=x, y=y), data=plotdata)

# show plot in interactive session
system.time(print(splot3))

# plot to file
ggsave("splot3.pdf",  device="pdf", width = 5, height = 5)
file_size("splot3.pdf")


## ----out.width="75%", fig.align='center'-------------------------------------------------------------
# generate scatter plot
splot4 <-
     ggplot(plotdata, aes(x=x, y=y))+
     geom_hex()


## ----out.width="75%", fig.align='center'-------------------------------------------------------------
mem_used()
system.time(print(splot4))
mem_used()


## ----warning=FALSE, echo=TRUE, message=FALSE---------------------------------------------------------

# SET UP----
# see 05_aggregtion_visualization.Rmd for details
# load packages
library(data.table)
library(ggplot2)

# import data into RAM (needs around 200MB)
taxi <- fread("data/tlc_trips.csv",
              nrows = 1000000)

# clean the factor levels
taxi$Payment_Type <- tolower(taxi$Payment_Type)
taxi$Payment_Type <- factor(taxi$Payment_Type, levels = unique(taxi$Payment_Type))     



## ----------------------------------------------------------------------------------------------------

# set up the canvas
taxiplot <- ggplot(taxi, aes(y=Tip_Amt, x= Fare_Amt)) 
taxiplot


## ----------------------------------------------------------------------------------------------------

# simple x/y plot
taxiplot + geom_scattermore(pointsize = 3)
     


## ----------------------------------------------------------------------------------------------------

# simple x/y plot
taxiplot + geom_scattermore(pointsize = 3, alpha=0.2)
     


## ----------------------------------------------------------------------------------------------------
# two-dimensional bins
taxiplot + geom_bin2d()


## ----------------------------------------------------------------------------------------------------

# two-dimensional bins
taxiplot +
     stat_bin_2d(geom="point",
                 mapping= aes(size = log(after_stat(count)))) +
     guides(fill = "none")
     


## ----------------------------------------------------------------------------------------------------


# compute frequency of per tip amount and payment method
taxi[, n_same_tip:= .N, by= c("Tip_Amt", "Payment_Type")]
frequencies <- unique(taxi[Payment_Type %in% c("credit", "cash"),
                           c("n_same_tip",
                             "Tip_Amt",
                             "Payment_Type")][order(n_same_tip,
                                                    decreasing = TRUE)])






## ----------------------------------------------------------------------------------------------------

# plot top 20 frequent tip amounts
fare <- ggplot(data = frequencies[1:20], aes(x = factor(Tip_Amt),
                                             y = n_same_tip)) 
fare + geom_bar(stat = "identity") 



## ----------------------------------------------------------------------------------------------------
fare + geom_bar(stat = "identity") + 
     facet_wrap("Payment_Type") 
     


## ----------------------------------------------------------------------------------------------------
# indicate natural numbers
taxi[, dollar_paid := ifelse(Tip_Amt == round(Tip_Amt,0), "Full", "Fraction"),]


# extended x/y plot
taxiplot +
     geom_scattermore(pointsize = 3, alpha=0.2, aes(color=Payment_Type)) +
     facet_wrap("dollar_paid") + 
     theme(legend.position="bottom")
     


## ----------------------------------------------------------------------------------------------------
taxi[, rounded_up := ifelse(Fare_Amt + Tip_Amt == round(Fare_Amt + Tip_Amt, 0),
                            "Rounded up",
                            "Not rounded")]
# extended x/y plot
taxiplot +
     geom_scattermore(data= taxi[Payment_Type == "credit"], 
                      pointsize = 3, alpha=0.2, aes(color=rounded_up)) +
     facet_wrap("dollar_paid") + 
     theme(legend.position="bottom")



## ----------------------------------------------------------------------------------------------------
modelplot <- ggplot(data= taxi[Payment_Type == "credit" &
                               dollar_paid == "Fraction" & 
                               0 < Tip_Amt],
                    aes(x = Fare_Amt, y = Tip_Amt))
modelplot +
     geom_scattermore(pointsize = 3, alpha=0.2, color="darkgreen") +
     geom_smooth(method = "lm", colour = "black")  + 
     theme(legend.position="bottom")


## ----------------------------------------------------------------------------------------------------
modelplot <- ggplot(data= taxi[Payment_Type == "credit" 
                               & dollar_paid == "Fraction" 
                               & 0 < Tip_Amt],
                    aes(x = Fare_Amt, y = Tip_Amt))
modelplot +
     geom_scattermore(pointsize = 3, alpha=0.2, color="darkgreen") +
     geom_smooth(method = "lm", colour = "black") +
     ylab("Amount of tip paid (in USD)") +
     xlab("Amount of fare paid (in USD)") +
     theme_bw(base_size = 18, base_family = "serif")


## ----echo = FALSE, message=FALSE, warning=FALSE------------------------------------------------------
# housekeeping
# gc()
system("rm -r fftaxi")


## ----message=FALSE, warning=FALSE--------------------------------------------------------------------
# load GIS packages
library(rgdal)
library(rgeos)


## ----message=FALSE, warning=FALSE--------------------------------------------------------------------
BASE_URL <- 
"https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/"
FILE <- "nycd_19a.zip"
URL <- paste0(BASE_URL, FILE)
tmp_file <- tempfile()
download.file(URL, tmp_file)
file_path <- unzip(tmp_file, exdir= "data")
# delete the temporary file
unlink(tmp_file)


## ----message=FALSE, warning=FALSE--------------------------------------------------------------------
# read GIS data
nyc_map <- readOGR(file_path[1], verbose = FALSE)
# have a look at the GIS data
summary(nyc_map)


## ----------------------------------------------------------------------------------------------------
# transform the projection
p <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
nyc_map <- 
  spTransform(nyc_map, p)
# check result
summary(nyc_map)


## ----warning=FALSE, message=FALSE--------------------------------------------------------------------
nyc_map <- fortify(nyc_map)


## ----------------------------------------------------------------------------------------------------
# taxi trips plot data
taxi_trips <- taxi[Start_Lon <= max(nyc_map$long) & 
                        Start_Lon >= min(nyc_map$long) &
                        End_Lon <= max(nyc_map$long) &
                        End_Lon >= min(nyc_map$long) &
                        Start_Lat <= max(nyc_map$lat) & 
                        Start_Lat >= min(nyc_map$lat) &
                        End_Lat <= max(nyc_map$lat) &
                        End_Lat >= min(nyc_map$lat) 
                        ]
taxi_trips <- taxi_trips[base::sample(1:nrow(taxi_trips), 50000)]



## ----------------------------------------------------------------------------------------------------
taxi_trips$start_time <- lubridate::hour(taxi_trips$Trip_Pickup_DateTime)


## ----------------------------------------------------------------------------------------------------
# define new variable for facets
taxi_trips$time_of_day <- "Morning"
taxi_trips[start_time > 12 & start_time < 17]$time_of_day <- "Afternoon"
taxi_trips[start_time %in% c(17:24, 0:5)]$time_of_day <- "Evening/Night"
taxi_trips$time_of_day  <- 
  factor(taxi_trips$time_of_day,
         levels = c("Morning", "Afternoon", "Evening/Night"))



## ----------------------------------------------------------------------------------------------------
# set up the canvas
locations <- ggplot(taxi_trips, aes(x=long, y=lat))
# add the map geometry
locations <- locations + geom_map(data = nyc_map,
                                  map = nyc_map,
                                  aes(map_id = id))
locations


## ----------------------------------------------------------------------------------------------------
# add pick-up locations to plot
locations + 
     geom_scattermore(aes(x=Start_Lon, y=Start_Lat),
                color="orange",
                pointsize = 1,
                alpha = 0.2)




## ----------------------------------------------------------------------------------------------------
# add drop-off locations to plot
locations +
     geom_scattermore(aes(x=End_Lon, y=End_Lat),
                color="steelblue",
                pointsize = 1,
                alpha = 0.2) +
     geom_scattermore(aes(x=Start_Lon, y=Start_Lat),
                color="orange",
                pointsize = 1,
                alpha = 0.2)
 



## ----fig.height=3, fig.width=9-----------------------------------------------------------------------

# pick-up locations 
locations +
     geom_scattermore(aes(x=Start_Lon, y=Start_Lat),
                color="orange",
                pointsize =1,
                alpha = 0.2) +
     facet_wrap(vars(time_of_day))


## ----fig.height=3, fig.width=9-----------------------------------------------------------------------

# drop-off locations 
locations +
     geom_scattermore(aes(x=End_Lon, y=End_Lat),
                color="steelblue",
                pointsize = 1,
                alpha = 0.2) +
     facet_wrap(vars(time_of_day))
 


## ----------------------------------------------------------------------------------------------------
# drop-off locations 
locations +
     geom_scattermore(aes(x=End_Lon, y=End_Lat, color = start_time),
                pointsize = 1,
                alpha = 0.2) +
     scale_colour_gradient2( low = "red", mid = "yellow", high = "red",
                             midpoint = 12)
 

