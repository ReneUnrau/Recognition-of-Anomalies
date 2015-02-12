# read shapefile (from osm) to export coordinates from traffic signals
library(rgdal)

# path to shapefile directory
shp_path = "traffic"
# read shape
traffic_shape = readOGR(dsn = shp_path, layer = "traffic_signals")
traffic_coords = data.frame(x = traffic_shape@coords[,1], y = traffic_shape@coords[,2])
# write to csv
write.table(x = traffic_coords, file = "fraffic.csv", sep = ";")

# test reading
read_coords = read.csv(file = "fraffic.csv", sep = ";")
head(read_coords)
