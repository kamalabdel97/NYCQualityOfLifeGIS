library(sf)
library(ggplot2)
install_github('rstudio/pagedown')

NYCComDistricts <- read_sf("/Users/kamal/Documents/GitHub/NYCQualityOfLifeGIS/NYC Community Districts Shapefile/Community Districts/geo_export_56ffd4c2-465e-4b9e-ab06-8ab9ebaa596f.shp")

TotalPop <- read.csv("/Users/kamal/Documents/GitHub/NYCQualityOfLifeGIS/Population/TotalPopulation.csv")

NYCComDistricts <- merge(NYCComDistricts, TotalPop, by.x = "boro_cd", by.y = "Fips")

ggplot(NYCComDistricts) + geom_sf(aes(fill = NYCComDistricts$Data)) +
 facet_grid(NYCComDistricts$TimeFrame) +
 facet_wrap(as.factor(NYCComDistricts$TimeFrame), nrow= 13) +
 scale_fill_gradient(low = "white", high = "black")
