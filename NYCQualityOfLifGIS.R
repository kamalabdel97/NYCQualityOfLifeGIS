library(sf)
library(ggplot2)
library(data.table)
library(ggspatial)

#Shapefile
NycCom <- read_sf("https://data.cityofnewyork.us/api/geospatial/yfnk-k7r4?method=export&format=GeoJSON")

ggsave("/Users/kamal/Documents/GitHub/NYCQualityOfLifeGIS/Final Maps/NYC.png",
ggplot() + geom_sf(data = NycCom) +
theme(axis.title.x=element_blank(),
   axis.text.x=element_blank(),
   axis.ticks.x=element_blank(),
   axis.title.y=element_blank(),
   axis.text.y =element_blank(),
   axis.ticks.y=element_blank(),
   panel.background = element_rect(fill = "transparent"), # panel bg   
   plot.background = element_rect(fill = "transparent", color = NA), # plot bg  
   panel.grid.major.x =  element_blank(), # get rid of major grid 
   panel.grid.minor.x = element_blank(), # get rid of minor grid
   panel.grid.major.y =  element_blank(), # get rid of major grid 
   panel.grid.minor.y = element_blank(), # get rid of minor grid
   legend.background = element_rect(fill = "transparent"),
   plot.title = element_text(hjust = 0.5)) +
   annotation_scale(location = "tl", width_hint = 0.4) +
   annotation_north_arrow(location = "tl", which_north = "true", 
      pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
      style = north_arrow_fancy_orienteering), 
dpi = 300, units = "in", bg = "transparent")

#Income
NYCIncome1 <- fread("https://raw.githubusercontent.com/kamalabdel97/Laundro-Hydro-Startup/master/Data/Median%20Incomes.csv")

NYCIncome <- subset(NYCIncome, nchar(NYCIncome$Fips) == 3 & 
  NYCIncome$`Household Type` == "All Households" & 
  NYCIncome$TimeFrame == "2017")

NycComIncome <- merge(NycCom, NYCIncome, by.x = "boro_cd", by.y = "Fips", all.x = TRUE)

ggsave("/Users/kamal/Documents/GitHub/NYCQualityOfLifeGIS/Final Maps/NYCIncomeLevels.png", 
 ggplot() +
  geom_sf(data = NycComIncome, aes(fill = NycComIncome$Income), color = NA) +
  scale_fill_continuous(low = rgb(204, 209, 205, maxColorValue=255), 
     high = rgb(0, 100, 46, maxColorValue=255), name = "Income") +
  theme(axis.title.x=element_blank(),
   axis.text.x=element_blank(),
   axis.ticks.x=element_blank(),
   axis.title.y=element_blank(),
   axis.text.y =element_blank(),
   axis.ticks.y=element_blank(),
   panel.background = element_rect(fill = "transparent"), # panel bg   
   plot.background = element_rect(fill = "transparent", color = NA), # plot bg  
   panel.grid.major.x =  element_blank(), # get rid of major grid 
   panel.grid.minor.x = element_blank(), # get rid of minor grid
   panel.grid.major.y =  element_blank(), # get rid of major grid 
   panel.grid.minor.y = element_blank(), # get rid of minor grid
   legend.background = element_rect(fill = "transparent"),
   plot.title = element_text(hjust = 0.5)) +
  labs(title = "Median Income Levels Across New York City") +
  annotation_scale(location = "tl", width_hint = 0.4) +
  annotation_north_arrow(location = "tl", which_north = "true", 
   pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
   style = north_arrow_fancy_orienteering), 
 dpi = 300, units = "in", bg = "transparent")

#Population
NYCPop <- fread("/Users/kamal/Documents/GitHub/Laundro-Hydro Startup/Data/Population/TotalPopulation.csv")

NYCPop <- subset(NYCPop, NYCPop$TimeFrame == "2017")

NycComPop <- merge(NycCom, NYCPop, by.x = "boro_cd", by.y = "Fips", all.x = TRUE)

NycComPop$PopulationPerArea <- (as.numeric(NycComPop$Data)/as.numeric(NycComPop$shape_area)) * 10000000

ggsave("/Users/kamal/Documents/GitHub/NYCQualityOfLifeGIS/Final Maps/NYCPopulation.png", plot = 
  ggplot() + geom_sf(data = NycComPop, aes(fill = NycComPop$PopulationPerArea), color = NA) +
      scale_fill_continuous(low = rgb(204, 209, 205, maxColorValue=255), 
         high = rgb(219, 156, 8, maxColorValue=255), name = "Population") +
  theme(axis.title.x=element_blank(),
   axis.text.x=element_blank(),
   axis.ticks.x=element_blank(),
   axis.title.y=element_blank(),
   axis.text.y =element_blank(),
   axis.ticks.y=element_blank(),
   panel.background = element_rect(fill = "transparent"), # panel bg   
   plot.background = element_rect(fill = "transparent", color = NA), # plot bg  
   panel.grid.major.x =  element_blank(), # get rid of major grid 
   panel.grid.minor.x = element_blank(), # get rid of minor grid
   panel.grid.major.y =  element_blank(), # get rid of major grid 
   panel.grid.minor.y = element_blank(), # get rid of minor grid
   legend.background = element_rect(fill = "transparent"),
   plot.title = element_text(hjust = 0.5)) +
  labs(title = "Population Across New York City", x = "", y = ""),
 dpi = 300, units = "in", bg = "transparent")

#Arrests 
Arrests <- fread("https://raw.githubusercontent.com/kamalabdel97/NYCQualityOfLifeGIS/master/Data/Arrests.csv")

Arrests <- subset(Arrests, nchar(Arrests$Fips) == 3) 
Arrests <- subset(Arrests, Arrests$TimeFrame == "2017") 

NycComArrests <- merge(NycCom, Arrests, by.x = "boro_cd", by.y = "Fips", all.x = TRUE)

ggsave("/Users/kamal/Documents/GitHub/NYCQualityOfLifeGIS/Final Maps/NYCPopulation.png", plot = 
  ggplot() + geom_sf(data = NycComArrests, aes(fill = Data), color = NA) +
  scale_fill_continuous(low = "white", high = "#FF8900", name = "Arrests") +
  theme(axis.title.x=element_blank(),
   axis.text.x=element_blank(),
   axis.ticks.x=element_blank(),
   axis.title.y=element_blank(),
   axis.text.y =element_blank(),
   axis.ticks.y=element_blank(),
   panel.background = element_rect(fill = "transparent"), # panel bg   
   plot.background = element_rect(fill = "transparent", color = NA), # plot bg  
   panel.grid.major.x =  element_blank(), # get rid of major grid 
   panel.grid.minor.x = element_blank(), # get rid of minor grid
   panel.grid.major.y =  element_blank(), # get rid of major grid 
   panel.grid.minor.y = element_blank(), # get rid of minor grid
   legend.background = element_rect(fill = "transparent"),
   plot.title = element_text(hjust = 0.5)) +
  labs(title = "Arrests Across New York City", x = "", y = ""),
 dpi = 300, units = "in", bg = "transparent")

#Unemployment Rate
UnemploymentRate <- fread("https://raw.githubusercontent.com/kamalabdel97/NYCQualityOfLifeGIS/master/Data/Unemployment%20Rate%20.csv")


UnemploymentRate <- subset(UnemploymentRate, nchar(UnemploymentRate$Fips) == 3 & 
  UnemploymentRate$TimeFrame == "2017")

NycComUnemploymentRate <- merge(NycCom, UnemploymentRate, by.x = "boro_cd", by.y = "Fips", all.x = TRUE)

ggsave("/Users/kamal/Documents/GitHub/NYCQualityOfLifeGIS/Final Maps/Unemployment Rate.png", 
  ggplot() + geom_sf(data = NycComUnemploymentRate, aes(fill = NycComUnemploymentRate$Data), color = NA) +
  scale_fill_continuous(low = rgb(204, 209, 205, maxColorValue=255), 
     high = rgb(3, 33, 66, maxColorValue=255), name = "Number of People Unemployed") +
  theme(axis.title.x=element_blank(),
   axis.text.x=element_blank(),
   axis.ticks.x=element_blank(),
   axis.title.y=element_blank(),
   axis.text.y =element_blank(),
   axis.ticks.y=element_blank(),
   panel.background = element_rect(fill = "transparent"), # panel bg   
   plot.background = element_rect(fill = "transparent", color = NA), # plot bg  
   panel.grid.major.x =  element_blank(), # get rid of major grid 
   panel.grid.minor.x = element_blank(), # get rid of minor grid
   panel.grid.major.y =  element_blank(), # get rid of major grid 
   panel.grid.minor.y = element_blank(), # get rid of minor grid
   legend.background = element_rect(fill = "transparent"),
   plot.title = element_text(hjust = 0.5)) +
  labs(title = "Uemployment Rate Across New York City", x = "", y = ""),
 dpi = 300, units = "in", bg = "transparent")

#Poverty
Poverty <- fread("https://raw.githubusercontent.com/kamalabdel97/NYCQualityOfLifeGIS/master/Data/Poverty.csv")

Poverty <- subset(Poverty, nchar(Poverty$Fips) == 3 &
      Poverty$TimeFrame == "2017")

Poverty <- Poverty[c(1:59),]

NycComPoverty <- merge(NycCom, Poverty, by.x = "boro_cd", by.y = "Fips", all.x = TRUE)

ggsave("/Users/kamal/Documents/GitHub/NYCQualityOfLifeGIS/Final Maps/Poverty Level.png", plot = 
  ggplot() + geom_sf(data = NycComPoverty, aes(fill = NycComPoverty$Data), color = NA) +
  scale_fill_continuous(low = rgb(204, 209, 205, maxColorValue=255), 
     high = rgb(3, 33, 66, maxColorValue=255), name = "Number of People Below Poverty Level") +
  theme(axis.title.x=element_blank(),
   axis.text.x=element_blank(),
   axis.ticks.x=element_blank(),
   axis.title.y=element_blank(),
   axis.text.y =element_blank(),
   axis.ticks.y=element_blank(),
   panel.background = element_rect(fill = "transparent"), # panel bg   
   plot.background = element_rect(fill = "transparent", color = NA), # plot bg  
   panel.grid.major.x =  element_blank(), # get rid of major grid 
   panel.grid.minor.x = element_blank(), # get rid of minor grid
   panel.grid.major.y =  element_blank(), # get rid of major grid 
   panel.grid.minor.y = element_blank(), # get rid of minor grid
   legend.background = element_rect(fill = "transparent"),
   plot.title = element_text(hjust = 0.5)) +
  labs(title = "Poverty Levels Across New York City", x = "", y = ""),
 dpi = 300, units = "in", bg = "transparent")


ggplot(NYCIncome, aes(x = NYCIncome$Income)) +
   geom_histogram(aes(fill = ..count..)) +
   scale_fill_gradient("Count", low = "blue", high = "red")

ggplot(NYCIncome) + geom_qq(aes(sample = Income)) + theme_classic()
shapiro.test(NYCIncome$Income)


