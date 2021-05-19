#dev.opentripplanner.org/apidoc/0.15.0/resource_PlannerResource.html
#https://developers.google.com/transit/gtfs/reference#routestxt
#https://transitfeeds.com/p/central-ohio-transit-authority/216

library(httr)
library(otpr)
library(progress)
library(geojson)
library(geojsonR)
library(geojsonio)

library(sf)
library(mapdeck)
library(geojsonsf)
library(tmap)

library(tidycensus)
library(tidyverse)
library(tigris)
library(magrittr)
library(dplyr)

options(tigris_use_cache = TRUE)

library(dplyr)
library(ggplot2)
library(tidytransit)


### cmd instructions
#cd C:\Users\armit\otp
#set path=%path%;C:\Program Files\jre1.8.0_261\bin
#java
#java -Xmx2G -jar otp.jar --build graphs/current
#java -Xmx2G -jar otp.jar --router current --graphs graphs --server

# getting census data
census_api_key("0374af53cfe8d2728204af53395977de1b54b17d")

# variable serach
v18 <- load_variables(2018, "acs5", cache = TRUE)

acs_data = get_acs(geography = "tract",
                     state = "OH",
                     variables = c(tot_pop = "B01003_001",
                                   tot_male = "B01001_002",
                                   pop_over25 = "B15003_001",
                                   white = "B02001_002",
                                   hispanic = "B03002_012",
                                   high_school = "B15003_018",
                                   med_inc = "B19013_001",
                                   poverty = "B17017_002",
                                   no_ins_under19 = "B27010_017",
                                   no_ins_19_34 = "B27010_033",
                                   no_ins_35_64 = "B27010_050",
                                   no_ins_over64 = "B27010_066",
                                   HH_size = "B11001_001"),
                     year = 2018,
                     survey = "acs5",
                     geometry = TRUE)

acs_data <- select(acs_data, -moe)
acs_data1 <- spread(acs_data, key = variable, value = estimate)
acs_data1$p_male = ((acs_data1$tot_male)/acs_data1$tot_pop)*100
acs_data1$p_female = 100 - acs_data1$p_male
acs_data1$p_n_whi = 100 - ((acs_data1$white)/acs_data1$tot_pop)*100
acs_data1$p_his = (acs_data1$hispanic/acs_data1$tot_pop)*100
acs_data1$p_hs = (acs_data1$high_school/acs_data1$pop_over25)*100
acs_data1$p_pov = (acs_data1$poverty/acs_data1$HH_size)*100
acs_data1$p_noins = ((acs_data1$no_ins_under19 + acs_data1$no_ins_19_34 +
                        acs_data1$no_ins_35_64 + acs_data1$no_ins_over64)/acs_data1$tot_pop)*100
acs_data1$county_geoid = 0
for (i in 1:nrow(acs_data1)){
  acs_data1$county_geoid[i] = substr(acs_data1$GEOID[i], 1,5)}

### centroid of census tracts
ct_centroid = st_transform(st_centroid(acs_data1), 4326)
a = as.data.frame(st_coordinates(ct_centroid))
ct_centroid$lon = a$X
ct_centroid$lat = a$Y
ct_centroid$latlong = paste(ct_centroid$lat, ct_centroid$lon, sep=",")



### vaccine provider data
loc = read.csv('D:/Vaccine_Access/vacc_location_w_lat_long.csv')
loc$latlong = paste(loc$lat, loc$lon, sep=",")
loc1= loc[complete.cases(loc[ , 9:10]),]
sf_loc = st_as_sf(loc1, coords = c("lon", "lat"), 
                   crs = 4326, agr = "constant")
plot(sf_loc)
'st_write(sf_loc, dsn = paste("D:/Vaccine_Access/Shapefiles/Vaccine_providers_ohio.shp"),
         driver = "ESRI Shapefile")
'''

###design isochrones
iso <- function(poi, Date, Time, Cutoff) {
      transit_current <- GET(
      "http://localhost:8080/otp/routers/current/isochrone",
      query = list(
        fromPlace = poi$latlong,
        toPlace = poi$latlong,
        arriveBy = TRUE,
        mode = "CAR", # modes we want the route planner to use
        date = Date,
        time= Time,
        cutoffSec = Cutoff
      )
    )
    transit_current <- content(transit_current, as = "text", encoding = "UTF-8")
    return(transit_current)
  }


### Spatial weights 
w1 = 1/exp(10/440)
w2 = 1/exp(20/440)
w3 = 1/exp(30/440)
w4 = 1/exp(40/440)


### physician to population ratio
dat = sf_loc[sf_loc$County=="Franklin",]
dat$pro_to_pop_ratio = 0
Total = nrow(dat)
for (i in 1:Total) {
  print(i)
  tryCatch({
    iso10 = st_simplify(geojson_sf(iso(dat[i, ], "03/01/2021", "9.00 am", 600)))
    iso20 = st_simplify(geojson_sf(iso(dat[i, ], "03/01/2021", "9.00 am", 1200)))
    iso30 = st_simplify(geojson_sf(iso(dat[i, ], "03/01/2021", "9.00 am", 1800)))
    iso40 = st_simplify(geojson_sf(iso(dat[i, ], "03/01/2021", "9.00 am", 2400)))
    iso1 = iso10
    iso2 = st_difference(iso20,iso10)
    iso3 = st_difference(iso30, iso20)
    iso4 = st_difference(iso40, iso30)
    ct1 = ct_centroid %>% filter(st_intersects(x = ., y = iso1, sparse = FALSE))
    ct2 = ct_centroid %>% filter(st_intersects(x = ., y = iso2, sparse = FALSE))
    ct3 = ct_centroid %>% filter(st_intersects(x = ., y = iso3, sparse = FALSE))
    ct4 = ct_centroid %>% filter(st_intersects(x = ., y = iso4, sparse = FALSE))
    dat$pro_to_pop_ratio[i] = 1/sum(sum(ct1$tot_pop*w1), sum(ct2$tot_pop*w2), 
                                    sum(ct3$tot_pop*w3), sum(ct4$tot_pop*w4))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }


### Spatial access index
ct_dat = ct_centroid[ct_centroid$county_geoid== 39049,]
ct_dat$SPAI = 0
Total = nrow(ct_dat)
for (i in 1:Total) {
  print(i)
  tryCatch({
    iso10 = st_simplify(geojson_sf(iso(ct_dat[i, ], "03/01/2021", "9.00 am", 600)))
    iso20 = st_simplify(geojson_sf(iso(ct_dat[i, ], "03/01/2021", "9.00 am", 1200)))
    iso30 = st_simplify(geojson_sf(iso(ct_dat[i, ], "03/01/2021", "9.00 am", 1800)))
    iso40 = st_simplify(geojson_sf(iso(ct_dat[i, ], "03/01/2021", "9.00 am", 2400)))
    iso1 = iso10
    iso2 = st_difference(iso20,iso10)
    iso3 = st_difference(iso30, iso20)
    iso4 = st_difference(iso40, iso30)
    ct1 = dat %>% filter(st_intersects(x = ., y = iso1, sparse = FALSE))
    ct2 = dat %>% filter(st_intersects(x = ., y = iso2, sparse = FALSE))
    ct3 = dat %>% filter(st_intersects(x = ., y = iso3, sparse = FALSE))
    ct4 = dat %>% filter(st_intersects(x = ., y = iso4, sparse = FALSE))
    ct_dat$SPAI[i] = sum(sum(ct1$pro_to_pop_ratio*w1), sum(ct2$pro_to_pop_ratio*w2), 
                         sum(ct3$pro_to_pop_ratio*w3), sum(ct4$pro_to_pop_ratio*w4))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}

st_write(ct_dat, dsn = paste("D:/Vaccine_Access/Shapefiles/ct_centroid_franklin.shp"),
         driver = "ESRI Shapefile")

### assign values to polygon boundaries
Franklin = acs_data1[acs_data1$county_geoid == 39049, ]
Franklin$SPAI = ct_dat$SPAI
Franklin$SPAR = Franklin$SPAI/mean(Franklin$SPAI)
st_write(Franklin, dsn = paste("D:/Vaccine_Access/Shapefiles/ct_franklin.shp"),
         driver = "ESRI Shapefile")

### create map
SPAR = tm_shape(Franklin) +  
  tm_borders(col = "grey30", alpha = 0.2) +
  tm_polygons("SPAR", n = 5, style = "quantile",
              palette = "GnBu",
              border.col = "white", 
              border.alpha = 0.1) +
  tm_borders() +
  tm_scale_bar(color.dark = "gray60",
               position = c("left", "bottom"), text.size = 0.3, 
               width = .25, lwd = .5) + 
  tm_compass(type = "4star", size = 1.5, text.size = 0.5,
             color.dark = "gray60", text.color = "gray60",
             position = c("left", "top")) +
  tm_layout(legend.outside = TRUE,
            legend.outside.position = c("right","bottom"))

tmap_save(SPAR, ("D:/Vaccine_Access/SPAR.png"), 
          dpi = 750, height = 7, width = 8.5, units = "in")






