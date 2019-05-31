library(sf)
library(ggplot2)
library(data.table)
library(ggspatial)

options(scipen=999)
#Shapefile
NycCom <- read_sf("https://data.cityofnewyork.us/api/geospatial/yfnk-k7r4?method=export&format=GeoJSON")

ggplot() + geom_sf(data = USA)

ggsave("/Users/kamal/Documents/untitled folder/USA NYC Locator.png",
ggplot() + geom_sf(data = USA) +
   geom_rect(data = NULL, aes(xmin = -73.939, xmax = -73.86, ymin = 40.795, ymax = 40.87), fill = NA, color="black")
      theme(axis.title.x= element_blank(),
         axis.text.x= element_blank(),
         axis.ticks.x= element_blank(),
         axis.title.y= element_blank(),
         axis.text.y = element_blank(),
         axis.ticks.y= element_blank(),
         panel.background = element_rect(fill = "transparent"), # panel bg   
         plot.background = element_rect(fill = "transparent", color = NA), # plot bg  
         panel.grid.major.x =  element_blank(), # get rid of major grid 
         panel.grid.minor.x = element_blank(), # get rid of minor grid
         panel.grid.major.y =  element_blank(), # get rid of major grid 
         panel.grid.minor.y = element_blank(), # get rid of minor grid
         legend.background = element_rect(fill = "transparent"),
         plot.title = element_text(hjust = 0.5)) +
      annotation_scale(location = "tl", plot_unit = "mi", height = unit(.15,"cm"),
         width_hint = .5, unit_category = "metric", style = "bar",
         pad_x = unit(0.5, "cm"), pad_y = unit(0.25, "cm")) +
      annotation_north_arrow(location = "tl", which_north = "true",
         pad_x = unit(.96, "in"), pad_y = unit(.25, "in"),
         style = north_arrow_fancy_orienteering),
   dpi = 300, units = "in", bg = "transparent")

ggsave("/Users/kamal/Documents/GitHub/NYCQualityOfLifeGIS/Final Maps/NYC.png",
ggplot() + geom_sf(data = NycCom) +
   theme(axis.title.x= element_blank(),
      axis.text.x= element_blank(),
      axis.ticks.x= element_blank(),
      axis.title.y= element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y= element_blank(),
      panel.background = element_rect(fill = "transparent"), # panel bg   
      plot.background = element_rect(fill = "transparent", color = NA), # plot bg  
      panel.grid.major.x =  element_blank(), # get rid of major grid 
      panel.grid.minor.x = element_blank(), # get rid of minor grid
      panel.grid.major.y =  element_blank(), # get rid of major grid 
      panel.grid.minor.y = element_blank(), # get rid of minor grid
      legend.background = element_rect(fill = "transparent"),
      plot.title = element_text(hjust = 0.5)) +
   annotation_scale(location = "tl", plot_unit = "mi", height = unit(.15,"cm"),
      width_hint = .5, unit_category = "metric", style = "bar",
      pad_x = unit(0.5, "cm"), pad_y = unit(0.25, "cm")) +
   annotation_north_arrow(location = "tl", which_north = "true",
      pad_x = unit(.96, "in"), pad_y = unit(.25, "in"),
      style = north_arrow_fancy_orienteering),
dpi = 300, units = "in", bg = "transparent")
   
#Income
NYCIncome <- fread("https://raw.githubusercontent.com/kamalabdel97/Laundro-Hydro-Startup/master/Data/Median%20Incomes.csv")
   
NYCIncome <- subset(NYCIncome, nchar(NYCIncome$Fips) == 3 & 
   NYCIncome$`Household Type` == "All Households" & 
   NYCIncome$TimeFrame == "2017")
   
colnames(NYCIncome)[5] <- "Data"
   
NycComIncome <- merge(NycCom, NYCIncome, by.x = "boro_cd", by.y = "Fips", all.x = TRUE)

ggsave("/Users/kamal/Documents/GitHub/NYCQualityOfLifeGIS/Final Maps/NYCIncomeLevels.png",
ggplot() + geom_sf(data = NycComIncome, aes(fill = NycComIncome$Data), color = NA) +
   scale_fill_continuous(low = rgb(204, 209, 205, maxColorValue=255), 
      high = rgb(0, 100, 46, maxColorValue=255), name = "Median\nIncome Level") +
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
      legend.position = c(.312,.685),
      plot.title = element_text(hjust = 0.5)) +
   labs(title = "Median Income Levels Across New York City") +
   geom_rect(data = NULL, aes(xmin = -73.939, xmax = -73.86, ymin = 40.795, ymax = 40.87), fill = NA, color="black") +
   geom_rect(data = NULL, aes(xmin = -74.04, xmax = -73.91, ymin = 40.7, ymax = 40.799), fill = NA, color="blue") +
   annotation_scale(location = "tl", plot_unit = "mi", height = unit(.15,"cm"),
         width_hint = .5, unit_category = "metric", style = "bar",
         pad_x = unit(0.8, "cm"), pad_y = unit(0.25, "cm")) +
   annotation_north_arrow(location = "tl", which_north = "true",
         pad_x = unit(1.03, "in"), pad_y = unit(.25, "in"),
         style = north_arrow_fancy_orienteering),
         dpi = 300, units = "in", bg = "transparent")
   
#Population
NYCPop <- fread("https://raw.githubusercontent.com/kamalabdel97/NYCQualityOfLifeGIS/master/Data/TotalPopulation.csv")
   
NYCPop <- subset(NYCPop, NYCPop$TimeFrame == "2017" & nchar(NYCPop$Fips) == 3)
   
NycComPop <- merge(NycCom, NYCPop, by.x = "boro_cd", by.y = "Fips", all.x = TRUE)
   
NycComPop$Data <- (as.numeric(NycComPop$Data)/as.numeric(NycComPop$shape_area)) * 10000000
   
ggsave("/Users/kamal/Documents/GitHub/NYCQualityOfLifeGIS/Final Maps/NYCPopulation.png", 
ggplot() + geom_sf(data = NycComPop, aes(fill = NycComPop$Data), color = NA) +
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
      plot.title = element_text(hjust = 0.5), 
      legend.position = c(.312,.685)) +
   labs(title = "Population Across New York City", x = "", y = "") +
   annotation_scale(location = "tl", plot_unit = "mi", height = unit(.15,"cm"),
      width_hint = .5, unit_category = "metric", style = "bar",
      pad_x = unit(0.8, "cm"), pad_y = unit(0.25, "cm")) +
   annotation_north_arrow(location = "tl", which_north = "true",
            pad_x = unit(1.03, "in"), pad_y = unit(.25, "in"),
            style = north_arrow_fancy_orienteering),
      dpi = 300, units = "in", bg = "transparent")
   
#Unemployment Rate
NYCUnemploymentRate <- fread("https://raw.githubusercontent.com/kamalabdel97/NYCQualityOfLifeGIS/master/Data/Unemployment%20Rate.csv")
   
NYCUnemploymentRate <- subset(NYCUnemploymentRate, 
   nchar(NYCUnemploymentRate$Fips) == 3 & 
   NYCUnemploymentRate$TimeFrame == "2017")
   
NYCUnemploymentRate$Data <- NYCUnemploymentRate$Data * 100
NycComUnemploymentRate <- merge(NycCom, NYCUnemploymentRate, by.x = "boro_cd", by.y = "Fips", all.x = TRUE)
   
ggsave("/Users/kamal/Documents/GitHub/NYCQualityOfLifeGIS/Final Maps/Unemployment Rate.png", 
   ggplot() + geom_sf(data = NycComUnemploymentRate, aes(fill = NycComUnemploymentRate$Data), color = NA) +
      scale_fill_continuous(low = rgb(204, 209, 205, maxColorValue=255), 
            high = rgb(3, 33, 66, maxColorValue=255), name = "Percent of\nPeople Unemployed") +
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
         plot.title = element_text(hjust = 0.5), 
         legend.position = c(.312,.65)) +
      labs(title = "Unemployment Across New York City", x = "", y = "") +
      annotation_scale(location = "tl", plot_unit = "mi", height = unit(.15,"cm"),
            width_hint = .5, unit_category = "metric", style = "bar",
            pad_x = unit(0.8, "cm"), pad_y = unit(0.25, "cm")) +
      annotation_north_arrow(location = "tl", which_north = "true",
            pad_x = unit(1.03, "in"), pad_y = unit(.25, "in"),
            style = north_arrow_fancy_orienteering),
      dpi = 300, units = "in", bg = "transparent")
   
#Poverty
NYCPoverty <- fread("https://raw.githubusercontent.com/kamalabdel97/NYCQualityOfLifeGIS/master/Data/Poverty.csv")
   
NYCPoverty <- subset(NYCPoverty, nchar(NYCPoverty$Fips) == 3 &
   NYCPoverty$TimeFrame == "2017")
   
NYCPoverty <- NYCPoverty[c(1:59),]
   
NycComPoverty <- merge(NycCom, NYCPoverty, by.x = "boro_cd", by.y = "Fips", all.x = TRUE)
   
ggsave("/Users/kamal/Documents/GitHub/NYCQualityOfLifeGIS/Final Maps/Poverty Level.png",  
   ggplot() + geom_sf(data = NycComPoverty, aes(fill = NycComPoverty$Data), color = NA) +
      scale_fill_continuous(low = rgb(204, 209, 205, maxColorValue=255), 
            high = rgb(152, 36, 3, maxColorValue=255), name = "Number of People \nBelow Poverty Level") +
      theme(axis.title.x=element_blank(),
         axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         axis.title.y = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks.y= element_blank(),
         panel.background = element_rect(fill = "transparent"), # panel bg   
         plot.background = element_rect(fill = "transparent", color = NA), # plot bg  
         panel.grid.major.x =  element_blank(), # get rid of major grid 
         panel.grid.minor.x = element_blank(), # get rid of minor grid
         panel.grid.major.y =  element_blank(), # get rid of major grid 
         panel.grid.minor.y = element_blank(), # get rid of minor grid
         legend.background = element_rect(fill = "transparent"),
         plot.title = element_text(hjust = 0.5), 
         legend.position = c(.312,.677)) +
      labs(title = "Poverty Across New York City", x = "", y = "") +
      annotation_scale(location = "tl", plot_unit = "mi", height = unit(.15,"cm"),
         width_hint = .5, unit_category = "metric", style = "bar",
         pad_x = unit(0.8, "cm"), pad_y = unit(0.25, "cm")) +
      annotation_north_arrow(location = "tl", which_north = "true",
         pad_x = unit(1.03, "in"), pad_y = unit(.25, "in"),
         style = north_arrow_fancy_orienteering),
      dpi = 300, units = "in", bg = "transparent")

#Income Bronx Locator  
ggsave("/Users/kamal/Documents/GitHub/NYCQualityOfLifeGIS/Final Maps/NYCIncomeLevels Bronx Locator.png",
 ggplot() + geom_sf(data = NycComIncome, aes(fill = NycComIncome$Data), color = NA) +
      scale_fill_continuous(low = rgb(204, 209, 205, maxColorValue=255), 
       high = rgb(0, 100, 46, maxColorValue=255)) +
      theme(axis.title.x=element_blank(),
         axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         axis.title.y = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks.y= element_blank(),
         panel.background = element_rect(fill = "transparent"), # panel bg   
         plot.background = element_rect(fill = "transparent", color = NA), # plot bg  
         panel.grid.major.x =  element_blank(), # get rid of major grid 
         panel.grid.minor.x = element_blank(), # get rid of minor grid
         panel.grid.major.y =  element_blank(), # get rid of major grid 
         panel.grid.minor.y = element_blank(), # get rid of minor grid
         legend.background = element_rect(fill = "transparent"),
         plot.title = element_text(hjust = 0.5), 
         legend.position = "none") +
      coord_sf(xlim = c(-73.93, - 73.77), ylim = c(40.799, 40.914)) +
      labs(x = "", y = "") +
      annotation_scale(location = "tr", plot_unit = "mi", height = unit(.15,"cm"),
         width_hint = .5, unit_category = "metric", style = "bar",
         pad_x = unit(.3, "cm"), pad_y = unit(0.25, "cm")) +
      annotation_north_arrow(location = "tr", which_north = "true",
         pad_x = unit(.79, "in"), pad_y = unit(.25, "in"),
         style = north_arrow_fancy_orienteering),
   dpi = 300, units = "in", bg = "transparent")

#Income Manhattan Locator   
ggsave("/Users/kamal/Documents/GitHub/NYCQualityOfLifeGIS/Final Maps/Income Level Manhattan Locator.png",
    ggplot() + geom_sf(data = NycComIncome, aes(fill = NycComIncome$Data), color = NA) +
      scale_fill_continuous(low = rgb(204, 209, 205, maxColorValue=255), 
         high = rgb(0, 100, 46, maxColorValue=255)) +
  theme(axis.title.x=element_blank(),
   axis.text.x = element_blank(),
   axis.ticks.x = element_blank(),
   axis.title.y = element_blank(),
   axis.text.y = element_blank(),
   axis.ticks.y= element_blank(),
   panel.background = element_rect(fill = "transparent"), # panel bg   
   plot.background = element_rect(fill = "transparent", color = NA), # plot bg  
   panel.grid.major.x =  element_blank(), # get rid of major grid 
   panel.grid.minor.x = element_blank(), # get rid of minor grid
   panel.grid.major.y =  element_blank(), # get rid of major grid 
   panel.grid.minor.y = element_blank(), # get rid of minor grid
   legend.background = element_rect(fill = "transparent"),
         plot.title = element_text(hjust = 0.5), 
         legend.position = "none") +
      coord_sf(xlim = c(-74.04, -73.91), ylim = c(40.7, 40.799)) +
      labs(x = "", y = "") +
      annotation_scale(location = "tl", plot_unit = "mi", height = unit(.15,"cm"),
         width_hint = .5, unit_category = "metric", style = "bar",
         pad_x = unit(0.2, "cm"), pad_y = unit(0.25, "cm")) +
      annotation_north_arrow(location = "tl", which_north = "true",
         pad_x = unit(.7, "in"), pad_y = unit(.25, "in"),
         style = north_arrow_fancy_orienteering),
   dpi = 300, units = "in", bg = "transparent")
   
   
NYC <- rbind(NYCIncome,NYCPop, NYCUnemploymentRate, NYCPoverty)
   
NYC$Dataset <- c(rep("Income",59), 
   rep("Population", 59), 
   rep("Unemployment Rate", 59), 
   rep("Poverty Rate", 59))
   
ggsave("/Users/kamal/Documents/GitHub/NYCQualityOfLifeGIS/Final Maps/IncomeHistogram.png",
  ggplot(NYCIncome, aes(x = Data)) + geom_density(fill = rgb(0, 100, 46, maxColorValue=255), alpha = .5) +
  theme(axis.title.x=element_blank(),
   # axis.text.x=element_blank(),
   # axis.ticks.x=element_blank(),
   # axis.title.y=element_blank(),
   # axis.text.y =element_blank(),
   # axis.ticks.y=element_blank(),
   panel.background = element_rect(fill = "transparent"), # panel bg   
   plot.background = element_rect(fill = "transparent", color = NA), # plot bg  
   panel.grid.major.x =  element_blank(), # get rid of major grid 
   panel.grid.minor.x = element_blank(), # get rid of minor grid
   panel.grid.major.y =  element_blank(), # get rid of major grid 
   panel.grid.minor.y = element_blank(), # get rid of minor grid
   legend.background = element_rect(fill = "transparent"))+
  labs(x = "Density", y = "Distribution of Income Levels"),
  dpi = 300, units = "in", bg = "transparent")
  
