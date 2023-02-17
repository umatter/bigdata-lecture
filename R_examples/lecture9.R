





## ADDITIONAL VISUALIZATION TUTORIAL: TIME AND SPACE
# load GIS packages
library(rgdal)
library(rgeos)


## ----message=FALSE, warning=FALSE-------------------------------------------------------
# download the zipped shapefile to a temporary file, unzip
URL <- "https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nycd_19a.zip"
tmp_file <- tempfile()
download.file(URL, tmp_file)
file_path <- unzip(tmp_file, exdir= "data")
# delete the temporary file
unlink(tmp_file)



## ----message=FALSE, warning=FALSE-------------------------------------------------------
# read GIS data
nyc_map <- readOGR(file_path[1], verbose = FALSE)

# have a look at the GIS data
summary(nyc_map)


## ---------------------------------------------------------------------------------------
# transform the projection
nyc_map <- spTransform(nyc_map, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# check result
summary(nyc_map)


## ----warning=FALSE, message=FALSE-------------------------------------------------------
nyc_map <- fortify(nyc_map)


## ---------------------------------------------------------------------------------------
# taxi trips plot data
taxi_trips <- taxi[start_long <= max(nyc_map$long) & 
                        start_long >= min(nyc_map$long) &
                        dest_long <= max(nyc_map$long) &
                        dest_long >= min(nyc_map$long) &
                        start_lat <= max(nyc_map$lat) & 
                        start_lat >= min(nyc_map$lat) &
                        dest_lat <= max(nyc_map$lat) &
                        dest_lat >= min(nyc_map$lat) 
]
taxi_trips <- taxi_trips[base::sample(1:nrow(taxi_trips), 50000)]



## ---------------------------------------------------------------------------------------
taxi_trips$start_time <- hour(taxi_trips$pickup_time)


## ---------------------------------------------------------------------------------------
# define new variable for facets
taxi_trips$time_of_day <- "Morning"
taxi_trips[start_time > 12 & start_time < 17]$time_of_day <- "Afternoon"
taxi_trips[start_time %in% c(17:24, 0:5)]$time_of_day <- "Evening/Night"
taxi_trips$time_of_day  <- factor(taxi_trips$time_of_day, levels = c("Morning", "Afternoon", "Evening/Night"))



## ---------------------------------------------------------------------------------------
# set up the canvas
locations <- ggplot(taxi_trips, aes(x=long, y=lat))
# add the map geometry
locations <- locations + geom_map(data = nyc_map,
                                  map = nyc_map,
                                  aes(map_id = id))
locations


## ---------------------------------------------------------------------------------------
# add pick-up locations to plot
locations + 
     geom_point(aes(x=start_long, y=start_lat),
                color="orange",
                size = 0.1,
                alpha = 0.2)



## ---------------------------------------------------------------------------------------
# add pick-up locations to plot
locations +
     geom_point(aes(x=dest_long, y=dest_lat),
                color="steelblue",
                size = 0.1,
                alpha = 0.2) +
     geom_point(aes(x=start_long, y=start_lat),
                color="orange",
                size = 0.1,
                alpha = 0.2)




## ----fig.height=3, fig.width=9----------------------------------------------------------

# pick-up locations 
locations +
     geom_point(aes(x=start_long, y=start_lat),
                color="orange",
                size = 0.1,
                alpha = 0.2) +
     facet_wrap(vars(time_of_day))



## ----fig.height=3, fig.width=9----------------------------------------------------------

# drop-off locations 
locations +
     geom_point(aes(x=dest_long, y=dest_lat),
                color="steelblue",
                size = 0.1,
                alpha = 0.2) +
     facet_wrap(vars(time_of_day))



## ---------------------------------------------------------------------------------------
# drop-off locations 
locations +
     geom_point(aes(x=dest_long, y=dest_lat, color = start_time ),
                size = 0.1,
                alpha = 0.2) +
     scale_colour_gradient2( low = "red", mid = "yellow", high = "red",
                             midpoint = 12)



## ---------------------------------------------------------------------------------------
# drop-off locations 
locations +
     geom_point(aes(x=dest_long, y=dest_lat, color = start_time ),
                size = 0.1,
                alpha = 0.2) 



## ---------------------------------------------------------------------------------------
# indicate natural numbers
taxi[, dollar_paid := ifelse(tip_amount == round(tip_amount,0), "Full", "Fraction"),]


# extended x/y plot
taxiplot +
     geom_point(alpha=0.2, aes(color=payment_type)) +
     facet_wrap("dollar_paid")



## ---------------------------------------------------------------------------------------
# indicate natural numbers
taxi[, dollar_paid := ifelse(tip_amount == round(tip_amount,0), "Full", "Fraction"),]


# extended x/y plot
taxiplot +
     geom_point(alpha=0.2, aes(color=payment_type)) +
     facet_wrap("dollar_paid") +
     scale_color_discrete(type = c("red", "steelblue", "orange", "purple"))


