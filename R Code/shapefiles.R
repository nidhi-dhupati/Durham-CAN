#didn't end up using this method
# static shapefiles to visualize Durham County

library(rgdal)
ncspdf <- readOGR(
  dsn = paste0(getwd(), "/tl_2020_37_tract"),
  layer = "tl_2020_37_tract",
  verbose = FALSE
)
class(ncspdf)
ncspdf <- ncspdf[ncspdf$COUNTYFP == "063", ]


library(broom)
library(rgdal)
library(maptools)
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()
ncspdf_fortified <- tidy(ncspdf, region = "NAME")

library(ggplot2)
ggplot() +
  geom_polygon(data = shapeandhcv, aes(x = long, y = lat, group = group, fill = as.factor(GEOID.x))) +
  theme_void() +
  coord_fixed(ratio = 1)

class(ncspdf)
ncspdf2 <- fortify(ncspdf)
class(ncspdf2)

ncspdf@data$id <- 0:(dim(ncspdf@data)[1]-1)

ncspdf2_join = plyr::join(x = ncspdf2, y = ncspdf@data, by="id")

shape <- ncspdf2_join %>% filter(id == 1291)

ggplot() +
  geom_polygon(data = ncspdf2_join, aes(x = long, y = lat, group = group, fill = id), color = "blue") +
  theme_void() +
  theme(legend.position = "none") +
  coord_fixed(ratio = 1)

shapefile_merge <- hcvchar %>% select(GEOID, NAME, state, county, tract, hcv_pct_of_total, pctbelowpoverty)

vec <- c(1270, 1284, 1286, 400, 1287, 1288, 1289, 1290, 1291, 1292, 1293, 1294, 339, 340, 
         1595, 341, 342, 358, 359, 1281, 1275, 1276, 1277, 1278, 360, 361, 1274, 
         1267, 1268, 1269, 1279, 730, 1111, 727, 1273, 1271, 397, 1272, 398, 399, 
         728, 729, 1114, 1115, 1116, 731, 1132, 1113, 1280, 1112, 1596, 732)

shapefile_merge$id <- vec
View(shapefile_merge)

shapeandhcv <- merge(hcvchar, shapefile_merge, by = "GEOID")

shapeandhcv$NAME.y <- NULL
shapeandhcv$state.y <- NULL
shapeandhcv$tract.y <- NULL
shapeandhcv$county.y <- NULL
shapeandhcv$GEOID.y <- NULL

#it works this way
hcv_mapping <- full_join(ncspdf2_join, shapeandhcv, by = "id")

ggplot() +
  geom_polygon(data = hcv_mapping, aes(x = long, y = lat, group = group, fill = as.factor(GEOID.y))) +
  theme_void() +
  coord_fixed(ratio = 1)

ggplot() +
  geom_polygon(data = hcv_mapping, aes(x = long, y = lat, group = group, fill = hcv_pct_of_total)) +
  theme_void() +
  coord_fixed(ratio = 1)

hcv_mapping$STATEFP <- NULL
hcv_mapping$COUNTYFP <- NULL
hcv_mapping$TRACTCE <- NULL
hcv_mapping$GEOID.x <- NULL
hcv_mapping$NAME <- NULL
hcv_mapping$NAMELSAD <- NULL
hcv_mapping$MTFCC <- NULL
hcv_mapping$FUNCSTAT <- NULL
hcv_mapping$ALAND <- NULL
hcv_mapping$AWATER <- NULL
hcv_mapping$INTPTLAT <- NULL
hcv_mapping$INTPTLON <- NULL
hcv_mapping$NAME.y <- NULL
hcv_mapping$state.y <- NULL
hcv_mapping$tract.y <- NULL
hcv_mapping$county.y <- NULL


#map with grey is NA, white is 0
durham_by_hcv <- ggplot() + geom_polygon(data = hcv_mapping, aes(x = long, y = lat, group = group, fill = as.factor(hcv_pct_of_total)), color = "blue") +
  scale_fill_manual(values = c("#fbfcfe", "#e3eafc", "#d9e1f9", "#c9d4f6", "#b8c7f4", "#a7b9f1", "#9db2f2", "#96acee",
                               "#859eec", "#7491e9", "#6384e6", "#527cfa", "#5276e4", "#4169e1", "#305cde",
                               "#2250d9", "#204ac8", "#1d43b7", "#1a3da6", "#173795", "#153184",
                               "#122a73", "#0f2462", "#0d1e51", "#0a1840", "#07112f", "#050b1e"), na.value = "grey") +
  theme_void() +
  labs(title = "Durham County by Census Tract") +
  #geom_star(data = test, aes(x = long, y = lat, group = group), fill = "red", size = 3) +
  #theme(legend.position = "none") +
  guides(fill = guide_legend(title = "Percent of HCV Residents in Tract")) +
  coord_sf()

library(ggplot2)
library(ggstar)

hcv_mapping[,'hcv_pct_of_total']=round(hcv_mapping[,'hcv_pct_of_total'],2)


# by percent poverty

ggplot(data = hcv_mapping, aes(x = long, y = lat, group = group, fill = pctbelowpoverty)) + geom_polygon(color = "blue") +
  theme_void() +
  scale_fill_continuous(trans = 'reverse', low = "#d9e1f9", high = "#0a1840") +
  coord_fixed(ratio = 1)

ggplot(data = hcv_mapping, aes(x = long, y = lat, group = group, fill = pctbelowpoverty)) + geom_polygon(color = "blue") +
  theme_void() +
  scale_fill_continuous(name = "Percent Below Poverty Line", low = "#d9e1f9", high = "#0a1840") +
  coord_fixed(ratio = 1) +
  labs(title = "Durham County by Census Tract")
  

ggplot(data = hcv_mapping, aes(x = long, y = lat, group = group, fill = popdensity)) + geom_polygon(color = "blue") +
  theme_void() +
  scale_fill_continuous(trans = 'reverse') +
  coord_fixed(ratio = 1)


ggplot(data = hcv_mapping, aes(x = long, y = lat, group = group, fill = ifelse(hcv_pct_of_total == 0, "white", hcv_pct_of_total))) + geom_polygon(color = "blue") +
  scale_fill_gradient2(high = "#132B43", mid = "#56B1F7", low = "#FFFFFF", midpoint = 0.1) +
  theme_void() +
  coord_fixed(ratio = 1)

ggplot() +
  geom_polygon(data = ncspdf2_join, aes(x = long, y = lat, group = group), color = "blue", fill = "white") +
  #geom_point(data = test, aes(x = long, y = lat, group = group), color = "red", size = 3) +
  geom_star(data = point, aes(x = long, y = lat, group = group), fill = "red", size = 3) +
  theme_void() +
  coord_fixed(ratio = 1)


COPYhcvmapping <- data.frame(hcv_mapping)

install.packages("remotes")
remotes::install_github("coolbutuseless/ggpattern")

ggplot() + geom_polygon(data = hcv_mapping, aes(x = long, y = lat, group = group, fill = hcv_pct_of_total), color = "blue") +
  geom_polygon_pattern(data = stripe, aes(x = long, y = lat, group = group), pattern = "stripe") +
  theme_void() +
  scale_fill_continuous(trans = 'reverse') +
  coord_fixed(ratio = 1)


