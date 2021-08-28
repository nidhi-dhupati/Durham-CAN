#lihtc = Low Income Housing Tax Credits (another form of subsidized housing)
#LIHTC propertieis can be at risk of losing affordability when they are 15 years old
#after 30 years, LIHTC properties "expire", but are sometimes kept affordable by local nonprofits
# plot where LIHTC properties are in Durham County, and whether they are at risk

lihtc_nc = read.csv('lihtc_nc.csv')
View(lihtc_nc)

lihtc_durham <- lihtc_nc %>% filter(proj_cty == "DURHAM")
View(lihtc_durham)

lihtc_charlotte <- lihtc_nc %>% filter(proj_cty == "CHARLOTTE")
View(lihtc_charlotte)


library(tidyverse)
library(leaflet)

leaflet(green_icons) %>% addTiles() %>%
  addMarkers(green_icons, lng = ~longitude, lat = ~latitude, icon = green)

leaflet(lihtc_durham) %>% addTiles() %>%
  addMarkers(lihtc_durham, lng = ~longitude, lat = ~latitude, popup = lihtc_durham$project)

green <- makeIcon(
  iconUrl = "green.png",
  iconWidth = "26", iconHeight = "26"
)

yellow <- makeIcon(
  iconUrl = "yellow.jpeg",
  iconWidth = "26", iconHeight = "26"
)

red <- makeIcon(
  iconUrl = "red.png",
  iconWidth = "26", iconHeight = "26"
)

green_icons <- lihtc_durham %>% filter(yr_alloc == 2015 | yr_alloc == 2016 |
                                         yr_alloc == 2017 | yr_alloc == 2018 |
                                         yr_alloc == 2019 | yr_alloc == 2020)


above_median <- lihtc_durham %>% filter(li_units >= 63)
below_median <- lihtc_durham %>% filter(li_units <= 63)


leaflet(above_median) %>% addTiles() %>%
  addMarkers(above_median, lng = ~longitude, lat = ~latitude, popup = above_median$project)

leaflet(below_median) %>% addTiles() %>%
  addMarkers(below_median, lng = ~longitude, lat = ~latitude, popup = below_median$project)

View(green_icons)

lihtc_durham %>% median(n_units)

units_number <- as.numeric(lihtc_durham$n_units)

lihtc_durham %>% median(units_number, na.rm = FALSE)

getColor <- function(lihtc_durham) {
  sapply(lihtc_durham$yr_alloc, function(yr_alloc) {
    if(yr_alloc == 2004 | yr_alloc == 2005 | yr_alloc == 2006) {
      "red"
    } else if(yr_alloc <= 1990) {
      "orange"
    } else {
      "green"
    } })
}

#by number of units
leaflet(lihtc_durham) %>% addTiles() %>%
  addCircles(radius = (lihtc_durham$li_units)*5)

iconColor <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'white',
  library = 'ion',
  markerColor = getColor(lihtc_durham)
)

lihtc_popup <- sprintf(
  lihtc_durham$project, lihtc_durham$proj_add, lihtc_durham$yr_alloc
) %>% lapply(htmltools::HTML)


lihtc_popup <- sprintf(
  "Data",
  lihtc_durham$project, lihtc_durham$proj_add
) %>% lapply(htmltools::HTML)
2

#14, 15, and 30+ are red
leaflet(lihtc_durham) %>% addTiles() %>%
  addAwesomeMarkers(icon=iconColor, label=lihtc_durham$proj_add)
library(tidyverse)
library(dplyr)
library(leaflet)

lihtc_select_cols <- lihtc_durham %>% select(project, proj_add, yr_pis, credit, nonprog, nlm_reason)
View(lihtc_select_cols)

write.csv(lihtc_select_cols, "\\durhamlihtc.csv")

View(lihtc_durham)
  
radius = getColor(lihtc_durham)

lihtc_nc

allNCiconcolor <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'white',
  library = 'ion',
  markerColor = getColor(lihtc_nc)
)
leaflet(lihtc_nc) %>% addTiles() %>%
  addAwesomeMarkers(icon=allNCiconcolor, label=lihtc_durham$proj_add)

