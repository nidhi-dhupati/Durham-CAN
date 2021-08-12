install.packages("leaflet")
library(leaflet)
library(RColorBrewer)

#copy base dataset to have a backup
tractandhcvCOPY <- tractandhcv

#make popup display Census Tract's name
LSAD_first <- tractandhcv %>% select(NAMELSAD, everything())
map <- tractandhcv %>% select(NAMELSAD, everything())
View(LSAD_first)

#base map
test <- leaflet(tractandhcv_poly) %>% addTiles() %>%
  addPolygons(stroke = TRUE,
              color = "#03F",
              weight = 2,
              fillColor = "white")
View(tractandhcv)

#adding color
bins <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
pal <- colorBin("Blues", domain = tractandhcv$hcv_pct_of_total, bins = bins)
View(pal)

test %>% addPolygons(
  fillColor = ~pal(hcv_pct_of_total),
  weight = 1,
  opacity = 1,
  color = "#666",
  dashArray = "2",
  fillOpacity = 0.7)

#adding interaction
test %>% addPolygons(
  fillColor = ~pal(hcv_pct_of_total),
  weight = 1,
  opacity = 1,
  color = "#666",
  dashArray = "2",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 3,
    color = "black",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE))

#add labels
labels <- sprintf(
  "<strong>%s</strong><br/>%g Percent of HCV Residents",
  tractandhcv$NAMELSAD, tractandhcv$hcv_pct_of_total
) %>% lapply(htmltools::HTML)


tractcopy <- data.frame(tractandhcv)

library(tidyverse)
View(tractcopy)

test <- test %>% addTiles() %>% addPolygons(
  fillColor = ~pal(hcv_pct_of_total),
  weight = 1.2,
  opacity = 1,
  color = "#666",
  dashArray = "2",
  fillOpacity = 0.5,
  highlight = highlightOptions(
    weight = 3,
    color = "black",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto"))

test

#trying different methods for overlaying circles
tractandhcv_poly <- st_cast(tractandhcv, "POLYGON")
tractandhcv_circle <- st_cast(tractandhcv, "POINT")
View(tractandhcv_circle)

leaflet(tractandhcv_circle) %>% addTiles() %>%
  addCircles(~tractandhcv_circle, radius = 10)


test %>% addTiles() %>% addCircles(tractandhcv_poly, radius = tractandhcv_poly$pctbelowpoverty)

#add markers

test %>% addTiles() %>% addMarkers(lng=-78.903, lat=36.022, popup="Popup here - insert link") %>%
  addMarkers(lng=-78.9, lat=36.2, popup="Popup 2")

house_icon <- makeIcon(
  iconUrl = "house2.png",
  iconWidth = "26", iconHeight = "26"
)

test %>% addTiles() %>% addMarkers(lng=-78.903, lat=36.022, popup="Popup here - insert link") %>%
  addMarkers(lng=-78.9, lat=36.2, popup="Popup 2", icon = house_icon) + 
  addCircles(lng=-78.9, lat=36.2, radius = 3)



#making the bubble size based on a characteristic
tmap_mode("view")

pct_below_pov <- tm_shape(tractandhcv) +
  tm_borders(alpha = 0.5, lty = "dashed") +
  tm_fill(col = "hcv_pct_of_total", breaks = c(0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17), 
          palette = "Blues", title = "Percent of HCV Residents", alpha = 0.7) +
  tm_basemap(server = "OpenStreetMap") +
  tm_bubbles(size = "pctbelowpoverty", col = "pctbelowpoverty", alpha = 0.5,
             border.col = "black", border.alpha = 0.5, palette = "YlOrRd")

tmap_mode("view")
default_dash
pct_below_pov


library(tigris)
library(leaflet)
library(dplyr)
library(tidyverse)
library(tmap)

tmap_mode("plot")
nofeatures <- tm_shape(map) +
  tm_borders(alpha = 0.5, lty = "dashed", lwd = 1.5) +
  tm_fill(col = "hcv_pct_of_total", popup.vars = c("Percent of HCV Residents" = "hcv_pct_of_total"
                                                   ),
          breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17), 
          palette = "Blues", title = "Percent of HCV Residents", alpha = 0.7) +
  tm_basemap(server = "OpenStreetMap")

library(tidyverse)
library(tmap)
no_hover_below_pov <- tm_shape(map) +
  tm_borders(alpha = 0.5, lty = "dashed", lwd = 1.5) +
  tm_fill(col = "hcv_pct_of_total", popup.vars = c("Percent of HCV Residents" = "hcv_pct_of_total",
                                                   "Percent of Households Living Below Poverty" = "pctbelowpoverty"),
          breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17), 
          palette = "Blues", title = "Percent of HCV Residents", alpha = 0.7) +
  tm_basemap(server = "OpenStreetMap") +
  tm_bubbles(size = "pctbelowpoverty", col = "pctbelowpoverty", alpha = 0.5,
             border.col = "black", border.alpha = 0.5, palette = "YlOrRd", title.col = "Percent of Households Below Poverty",
             popup.vars = c("Percent of HCV Residents" = "hcv_pct_of_total",
                            "Percent of Households Living Below Poverty" = "pctbelowpoverty"))

tmap_mode("view"
)
no_hover_below_pov

tractandhcv <- tractandhcv %>% mutate(hhinc_med = na_if(hhinc_med, -666666666))

map <- map %>% mutate(hhinc_med = na_if(hhinc_med, -666666666))

#hhinc_med
tm_shape(map) +
  tm_borders(alpha = 0.5, lty = "dashed", lwd = 1.5) +
  tm_fill(col = "hcv_pct_of_total", popup.vars = c("Percent of HCV Residents" = "hcv_pct_of_total",
                                                   "Median Household Income" = "hhinc_med"),
          breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17), 
          palette = "Blues", title = "Percent of HCV Residents", alpha = 0.7) +
  tm_basemap(server = "OpenStreetMap") +
  tm_bubbles(size = "hhinc_med", col = "hhinc_med", alpha = 0.5,
             border.col = "black", border.alpha = 0.5, palette = "YlOrRd", title.col = "Median Household Income ($)",
             popup.vars = c("Percent of HCV Residents" = "hcv_pct_of_total",
                            "Median Household Income ($)" = "hhinc_med"))
#popdensity
tm_shape(map) +
  tm_borders(alpha = 0.5, lty = "dashed", lwd = 1.5) +
  tm_fill(col = "hcv_pct_of_total", popup.vars = c("Percent of HCV Residents" = "hcv_pct_of_total",
                                                   "Population Density" = "popdensity"),
          breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17), 
          palette = "Blues", title = "Percent of HCV Residents", alpha = 0.7) +
  tm_basemap(server = "OpenStreetMap") +
  tm_bubbles(size = "popdensity", col = "popdensity", alpha = 0.5,
             border.col = "black", border.alpha = 0.5, palette = "YlOrRd", title.col = "Population Density (people / sq. mile)",
             popup.vars = c("Percent of HCV Residents" = "hcv_pct_of_total",
                            "Population Density" = "popdensity"))

View(map)

#mean rent 2 bed
tm_shape(map) +
  tm_borders(alpha = 0.5, lty = "dashed", lwd = 1.5) +
  tm_fill(col = "hcv_pct_of_total", popup.vars = c("Percent of HCV Residents" = "hcv_pct_of_total",
                                                   "Mean Rent of a 2 Bedroom Apartment ($)" = "mean_gross_rent_2bed"),
          breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17), 
          palette = "Blues", title = "Percent of HCV Residents", alpha = 0.7) +
  tm_basemap(server = "OpenStreetMap") +
  tm_bubbles(size = "mean_gross_rent_2bed", col = "mean_gross_rent_2bed", alpha = 0.5,
             border.col = "black", border.alpha = 0.5, palette = "YlOrRd", title.col = "Mean Rent of a 2 Bedroom Apartment ($)",
             popup.vars = c("Percent of HCV Residents" = "hcv_pct_of_total",
                            "Mean Rent of a 2 Bedroom Apartment ($)" = "mean_gross_rent_2bed"))


#wage growth
tm_shape(map) +
  tm_borders(alpha = 0.5, lty = "dashed", lwd = 1.5) +
  tm_fill(col = "hcv_pct_of_total", popup.vars = c("Percent of HCV Residents" = "hcv_pct_of_total",
                                                   "Wage Growth % (2014 to 2019)" = "wagegrowth"),
          breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17), 
          palette = "Blues", title = "Percent of HCV Residents", alpha = 0.7) +
  tm_basemap(server = "OpenStreetMap") +
  tm_bubbles(size = "wagegrowth", col = "wagegrowth", alpha = 0.5,
             border.col = "black", border.alpha = 0.5, palette = "YlOrRd", title.col = "Wage Growth % (2014 to 2019)",
             popup.vars = c("Percent of HCV Residents" = "hcv_pct_of_total",
                            "Wage Growth % (2014 to 2019)" = "wagegrowth"))

View(svi_index)
only_SVI <- svi_index %>% select(GEOID, Social.Vulnerability.Index)
View(only_SVI)
library(tidyverse)

only_SVI <- only_SVI %>% mutate(Social.Vulnerability.Index = na_if(Social.Vulnerability.Index, -999.0000))

LSAD_first_SVI <- merge(LSAD_first, only_SVI, by = "GEOID", all = TRUE)
View(LSAD_first_SVI)

library(tmap)
tmap_mode("view")

tm_shape(LSAD_first_SVI) +
  tm_borders(alpha = 0.5, lty = "dashed", lwd = 1.5) +
  tm_fill(col = "hcv_pct_of_total", popup.vars = c("Percent of HCV Residents" = "hcv_pct_of_total",
                                                   "Social Vulnerability Index" = "Social.Vulnerability.Index"),
          breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17), 
          palette = "Blues", title = "Percent of HCV Residents", alpha = 0.7) +
  tm_basemap(server = "OpenStreetMap") +
  tm_bubbles(size = "Social.Vulnerability.Index", col = "Social.Vulnerability.Index", alpha = 0.5,
             border.col = "black", border.alpha = 0.5, palette = "YlOrRd", title.col = "Social Vulnerability Index",
             popup.vars = c("Percent of HCV Residents" = "hcv_pct_of_total",
                            "Social Vulnerability Index" = "Social.Vulnerability.Index"))

install.packages("naniar")
library(naniar)





#add legend

tractandhcv_poly <- st_cast(tractandhcv, "POLYGON")
View(tractandhcv_poly)

leaflet(tractandhcv_poly) %>% addTiles() %>%
  addPolygons(stroke = TRUE,
              color = "#03F",
              weight = 2,
              fillColor = "white")

default_dash <- test
default_dash


#EVERYTHING BELOW THIS POINT is "scratchwork" and was not used in the final website
#keeping it here for possible expansion opportunities

test <- test %>% addLegend(pal = pal, values = ~hcv_pct_of_total, opacity = 0.7, title = NULL,
          position = "topright") %>%
  addCircleMarkers(tractandhcv_poly, radius = 3)

View(geometry)

tm_bubbles(size = "pctbelowpoverty", col = "pctbelowpoverty", alpha = 0.5,
           border.col = "black", border.alpha = 0.5, palette = "YlOrRd")

update(test, title = "Percent of HCV Residents")

test


plot(tractandhcv["hcv_pct_of_total"], 
     main = "HCV Percent of Total Leaflet",
     pal = brewer.pal(9, "Blues"))


View(tracts)
View(hcvchar)
View(tractandhcv)
tractandhcv <- merge(tracts, hcvchar, by = "GEOID", all = TRUE)

install.packages("tmap")
library(tmap)

tm_shape(tractandhcv) +
  tm_polygons(fill = "white")

#tmap version - GEOIDs with dataset, not labeled yet
tmap_mode("view")
tmap_last()


palette <- colorQuantile("YlOrRd", NULL, n = 28)


#change color palette, exclude 0s, add a "pin" to a certain place"
tmap_mode("view")
tmap_test <- tm_shape(tractandhcv) +
  tm_borders(alpha = 0.5) +
  tm_fill(col = "hcv_pct_of_total", palette = c("white", "blue"), title = "Percent of HCV Residents", alpha = 0.7) +
  tm_basemap(server = "OpenStreetMap")


tmap_leaflet(tmap_test)

#making unique colors for each hcv percentage

require(mgcv)

library(sf)
library(cartogram)
install.packages("cartogram")

var <- get.var("hcv_pct_of_total", tractandhcv)
vbreaks <- boxbreaks(var)
summary(vbreaks)

summary(tractandhcv$hcv_pct_of_total)
library(tmap)

tmap_categories <- tm_shape(tractandhcv) +
  tm_borders(alpha = 0.5) +
  tm_fill(col = "hcv_pct_of_total", breaks = c(0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17), 
          palette = "Blues", title = "Percent of HCV Residents", alpha = 0.7) +
  tm_basemap(server = "OpenStreetMap") +
  tm_bubbles(size = "pctbelowpoverty", col = "pctbelowpoverty", alpha = 0.5,
             border.col = "black", border.alpha = 0.5, palette = "YlOrRd")

tmap_leaf <- tm_shape(tractandhcv) +
  tm_borders(alpha = 0.5) +
  tm_fill(col = "hcv_pct_of_total", breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17), 
          palette = "Blues", title = "Percent of HCV Residents", alpha = 0.7) +
  tm_basemap(server = "OpenStreetMap") +
  tm_bubbles(size = "pctbelowpoverty", col = "pctbelowpoverty", alpha = 0.5,
             border.col = "black", border.alpha = 0.5, palette = "YlOrRd")

m <- tmap_leaflet(tmap_leaf)

labels <- sprintf(
  "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
  tractandhcv$NAME.x, tractandhcv$pctbelowpoverty
) %>% lapply(htmltools::HTML)

m %>% addPolygons(label = labels,
labelOptions = labelOptions(
  style = list("font-weight" = "normal", padding = "3px 8px"),
  textsize = "15px",
  direction = "auto"))

library(leaflet)
tmap_leaflet(tmap_leaf) %>% 
  addPolygons(popup = paste0("GEOID:", GEOID))
View(tractandhcv)

library(tigris)
library(tmap)
#switched chloropleth parts
tmap_switched <- tm_shape(tractandhcv) +
  tm_borders(alpha = 0.5) +
  tm_fill(col = "pctbelowpoverty", breaks = c(0, 10, 20, 30, 40, 50, 60), 
          palette = "Blues", title = "Percent of HCV Residents", alpha = 0.7) +
  tm_basemap(server = "OpenStreetMap") +
  tm_bubbles(size = "hcv_pct_of_total", col = "hcv_pct_of_total", alpha = 0.5,
             border.col = "black", border.alpha = 0.5, palette = "YlOrRd")

tmap_leaflet(tmap_switched)


tm_shape(tractandhcv) +
  tm_borders(alpha = 0.5) +
  tm_fill(col = "hcv_pct_of_total", n = 10, 
          style = "jenks", palette = "Blues", title = "Percent of HCV Residents", alpha = 0.7) +
  tm_basemap(server = "OpenStreetMap")

#need to get 0s by themselves
tmap_breaks <- tm_shape(tractandhcv) +
  tm_borders(alpha = 0.5) +
  tm_fill(col = "hcv_pct_of_total", n = 10, 
          style = "equal", palette = "Blues", title = "Percent of HCV Residents", alpha = 0.7) +
  tm_basemap(server = "OpenStreetMap")

tmap_leaflet(tmap_breaks)

spplot(tractandhcv, "hcv_pct_of_total")

boxbreaks <- function(v,mult=1.5) {
  # break points for box map
  # arguments:
  #   v: vector with observations
  #   mult: multiplier for IQR (default 1.5)
  # returns:
  #   bb: vector with 7 break points
  # compute quartile and fences
  qv <- unname(quantile(v))
  iqr <- qv[4] - qv[2]
  upfence <- qv[4] + mult * iqr
  lofence <- qv[2] - mult * iqr
  # initialize break points vector
  bb <- vector(mode="numeric",length=7)
  # logic for lower and upper fences
  if (lofence < qv[1]) {  # no lower outliers
    bb[1] <- lofence
    bb[2] <- floor(qv[1])
  } else {
    bb[2] <- lofence
    bb[1] <- qv[1]
  }
  if (upfence > qv[5]) { # no upper outliers
    bb[7] <- upfence
    bb[6] <- ceiling(qv[5])
  } else {
    bb[6] <- upfence
    bb[7] <- qv[5]
  }
  bb[3:5] <- qv[2:4]
  return(bb)
}


#leaflet time

proj4string(ncspdf)
tract_WGS84 <- st_transform(tractandhcv)

leaflet(tractandhcv) %>% addPolygons()


library(htmlwidgets)
library(DT)
install.packages("DT")
a <- datatable(to_html)
saveWidget(to_html, "example.html")

#adding dots to good map
dplyr::glimpse(tracts)
View(tractandhcv)
View(tracts)
class(tracts)

#create ncspdf with GEOID field
class(ncspdf)
df.polygon2<-ncspdf
data<-shapefile_merge
View(data)
View(tractandhcv)
View(hcvchar)

df.polygon2@data$rec<-1:nrow(df.polygon2@data)
tmp <- left_join(df.polygon2@data, data, by=c("GEOID"="id")) %>% 
  arrange(rec)

df.polygon2@data<-tmp

polygontest <- leaflet() %>% addTiles() %>%
  addPolygons(data = df.polygon2, stroke = TRUE,
              color = "#03F",
              weight = 2,
              fillColor = "white")

bins <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
pal <- colorBin("Blues", domain = df.polygon2$hcv_pct_of_total, bins = bins)

polygontest <- polygontest %>% addTiles() %>% addPolygons(
  data = df.polygon2,
  fillColor = ~pal(df.polygon2$hcv_pct_of_total),
  weight = 1.2,
  opacity = 1,
  color = "#666",
  dashArray = "2",
  fillOpacity = 0.5,
  highlight = highlightOptions(
    weight = 3,
    color = "black",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto"))
View(df.polygon2)

test







