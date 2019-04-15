library(dplyr)
library(qdap)
library(sf)
library(geojsonsf)
library(ggplot2)
library(eurostat)
library(knitr)
library(ggspatial)
library(ggrepel)
library(MASS)


#laod the dataset with volkskrant info
data <- read.csv("data/data_volkskrt.csv")

#create periods of ten years
census <- c(1960, 1970, 1980, 1990)
periods <- lapply(census, function(x) data.frame(years=c((x-5):(x+4)),
                                                 period=x)) %>% bind_rows()
#aggregation of the data
data$year <- lookup(data$year, periods)

data <- data %>%
  group_by(city, year) %>%
  summarise(freq=sum(freq))

#this function allow to download a sf object with geometries of municipalities at a given date
get_map <- function(year){
  library(geojsonsf)
  url <- paste("http://nlgis.nl/api/maps?year=", year, "&format=geojson", sep = "")
  mun <- geojson_sf(url)
  mun$period <- paste0(year-5, "-", year+4)
  mun}

#dowload one map for year period
for(i in census){
  name <- paste0("map_", i)
  assign(name, get_map(i))
}

#combine them into a single object to use them in with the ggplot function facet_wrap
base_maps <- rbind(map_1960, map_1970, map_1980, map_1990)
base_maps <- base_maps %>% group_by(period) %>%summarise()

#get map of neighboring countries
nbr <- get_eurostat_geospatial(output_class = "sf", resolution = "03", 
                               year = "2003", nuts_level = "0")
nbr <- nbr[nbr$CNTR_CODE %in% c("FR", "DE", "BE"),]
nbr <- st_transform(nbr, st_crs(base_maps))

#add spatial info to cities
cities <- read.csv("data/cities_geo.csv")
data <- left_join(data, cities[,-2], by=c("city"="location"))

#compute relative scores
total <- read.csv("data/corpus_volkskrt.csv")
data <- left_join(data, total, c("year"="year"))
data$rel <- data$freq/data$items*100

#plot the media coverage for each period
data$period <- paste0(data$year-5, "-", data$year+4)

g <- ggplot()+
  geom_sf(data=nbr, fill="white", size=.1)+
  geom_sf(data=base_maps, size=.1, fill="gray97")+
  coord_sf(xlim=c(3.3,7.1), ylim=c(50.8, 53.5), datum = NA)+
  geom_point(data=data, aes(x = lon, y=lat, size=rel), alpha=.6)+
  scale_size_continuous(range=c(.1, 6), name = 'Relative freq.')+
  facet_wrap(~period)+
  theme(axis.title = element_blank())
#g
#ggsave("map_facets.png", g, "png")

#change between 1960 and 1990
data_1990 <- data[data$year==1990,]#data for the 90s
data90 <- left_join(data[data$year==1990,c(1,3,4,5,7)], #add info on the 60s
                    data[data$year==1960,c(1,3,7)], 
                    by=c("city"="city"))
data90[is.na(data90)] <- 0
data90$change <- data90$rel.x-data90$rel.y#compute the difference

#visualisation
g2 <- ggplot()+
  geom_sf(data=nbr, fill="white", size=.1)+
  geom_sf(data=st_union(map_1990), fill="gray97", size=.1)+
  geom_point(data=data90, aes(x=lon, y=lat, size=rel.x, fill=change), 
             pch=21, color="gray50")+
  scale_fill_gradient2(high="red", mid="white", low = "blue", midpoint = 0)+
  coord_sf(xlim=c(3.3,7.1), ylim=c(50.8, 53.5), datum = NA)+
  theme(axis.title = element_blank())+
  annotation_north_arrow(height = unit(0.5, "cm"), width = unit(0.5, "cm"), 
                         location = "br")+
  scale_size_continuous(range=c(.3,9), name = 'Rel. freq. 1990')+
  annotation_scale(height = unit(0.1, "cm"))
#g2  
#ggsave("change.png", g2, "png")

#top 10 increase
data90 <- data90[order(data90$change, decreasing = T),]
data90[1:10,c(1,8)]

#top 10 decrease
data90 <- data90[order(data90$change),]
data90[1:10,c(1,8)]



#is coverage funciton of city size?

#population data
pop_F <- read.csv('C:/Users/aperis/Documents/Data/Netherlands/NLGIS/NLGIS-Input/D8_Population_F_NLGIS.csv')
pop_M <- read.csv('C:/Users/aperis/Documents/Data/Netherlands/NLGIS/NLGIS-Input/D7_Population_M_NLGIS.csv')
pop <- left_join(pop_F, pop_M, by=c("year"="year", "amsterdam_code"="amsterdam_code"))
pop$total <- pop$value.x+pop$value.y
pop <- pop[pop$year>1945 & pop$year<1995,]

#transformation into spatial data
map_1990 <- left_join(map_1990, pop[pop$year==1990,c(1,2,5)], by=c("amsterdamcode"="amsterdam_code"))

#creation of a dataset with municipal population and mention of places
cities <- st_as_sf(cities, coords = c("lon", "lat"), crs=st_crs(4326))
cities <- st_intersection(cities, map_1990)
cities <- left_join(cities, data90[,c(1,2)], by=c("location"="city"))
cities$lon <- st_coordinates(cities)[,1]
cities$lat <- st_coordinates(cities)[,2]
cities <- as.data.frame(cities)
cities <- cities[,-11]
cities <- cities %>% group_by(name, amsterdamcode, total)%>%
  summarise(lon=mean(lon), lat=mean(lat),freq=sum(freq.x))#as in some cases, several settlements 
#are in one municipality, we aggregated their frequency of mention together

cities <- cities[!is.na(cities$total),]

#correlation
cor(cities$freq, cities$total)

#linear model
mod <- lm(log(freq)~log(total), data=cities)
summary(mod)

#cities <- cities[order(cities$total, decreasing = T),]

see <- ggplot()+
  geom_point(data=cities, aes(x=log(total), y=log(freq)))+
  geom_smooth(data=cities, aes(x=log(total), y=log(freq)), method = "lm")+
  labs(y="ln(Frequency)", x="ln(Population)")#+
  #geom_text_repel(data=cities[1:10,], aes(x=log(total), y=log(freq), label=name), size=3)

see

#ggsave("regression.png", see, "png")


#mapping residuals
cities$res <- stdres(mod)

res <- ggplot()+
  geom_sf(data=nbr, fill="white", size=.1)+
  geom_sf(data=st_union(map_1990), fill="gray97", size=.1)+
  geom_point(data=cities, aes(x=lon, y=lat, color=res, size=abs(res)))+
  scale_color_gradient2(high="red", mid="white", low = "blue", midpoint = 0)+
  coord_sf(xlim=c(3.3,7.1), ylim=c(50.8, 53.5), datum = NA)+
  theme(axis.title = element_blank())+
  annotation_north_arrow(height = unit(0.5, "cm"), width = unit(0.5, "cm"), 
                         location = "br")+
  annotation_scale(height = unit(0.1, "cm"))

#res
#ggsave("res.png", res, "png")
