rm(list = ls())
library(tidyverse)
library(leaflet)
library(readxl)
library(maps)
library(sp)
library(rgdal)
library(htmlwidgets)

#Initial setup 
orig_users = 21643 #Number of visits from old blogspot site
dir = "/Users/smsarkar/Documents/Blog/00_map_visitors/"
input <- paste0(dir, "input/")
output <- paste0(dir, "output/")

#Read data and clean from google analytics form
dat <- read_xlsx(paste0(input, "raw.xlsx")) %>%
  janitor::clean_names()
dat <- dat[-c(1:4), 2:ncol(dat)]
colnames(dat) <- c("country", colnames(dat)[1:(ncol(dat)-1)])
dat <- dat %>%
  mutate(new_users = as.numeric(new_users), 
        users = as.numeric(users),
        country = ifelse(country == "Czechia", "Czech Republic", country),
        country = ifelse(country == "South Korea", "Korea, Republic of", country),
        country = ifelse(grepl("kiye", country), "Turkey", country)) %>%
  group_by(country) %>%
  summarise(users = sum(users))


mapWorld = map("world", fill = TRUE, plot = FALSE)

  
# Read a template shape file with the rgdal library. 
world_spdf <- readOGR( 
  dsn= paste0(input,"/DATA/world_shape_file/") , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

#Join on user data
world_spdf@data <- left_join(world_spdf@data, dat, by = c("NAME" = "country"))

#Simple 1 or NA if I have visits from the country
world_spdf@data$users = ifelse(is.na(world_spdf@data$users), NA, 1)
mypalette <- colorNumeric(palette=c("blue"), 
                           domain=world_spdf@data$users, 
                           na.color="transparent")

#Create map
m <- leaflet(world_spdf) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons(fillColor = ~mypalette(users), stroke=FALSE )
# %>% Commented out code to add legend if ever needed
#   addLegend(colors = c("blue"),
#             labels = c("Country Visited"),
#             title = "", position = "bottomright")
m

#Output map
saveWidget(m, file=paste0(output, "/user_map.html"))
