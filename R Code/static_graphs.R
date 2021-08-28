#visualizations
ggplot(data = hcvchar) + geom_point(mapping = aes(x = NAME, y = pctcollplus))

ggplot(data = hcvchar) + geom_histogram(mapping = aes(x = pctcollplus), bins = 10) +
  labs(title = "Percentage of Residents with at least a College Degree", x = "Percentage", y = "Count")
ggplot(data = hcvchar) + geom_bar(mapping = aes(x = pctcollplus)) + scale_x_binned()

ggplot(data = hcvchar) + geom_boxplot(mapping = aes(y = pctcollplus))

#making hcv_pct_total categorical
hcvchar$category[hcvchar$hcv_pct_of_total == 0] = "No HCV Residents"
hcvchar$category[hcvchar$hcv_pct_of_total > 0 & hcvchar$hcv_pct_of_total < 1] = "0-1%"
hcvchar$category[hcvchar$hcv_pct_of_total >= 1 & hcvchar$hcv_pct_of_total < 2] = "1-2%"
hcvchar$category[hcvchar$hcv_pct_of_total >= 2 & hcvchar$hcv_pct_of_total < 3] = "2-3%"
hcvchar$category[hcvchar$hcv_pct_of_total >= 3 & hcvchar$hcv_pct_of_total < 4] = "3-4%"
hcvchar$category[hcvchar$hcv_pct_of_total >= 4 & hcvchar$hcv_pct_of_total < 5] = "4-5%"
hcvchar$category[hcvchar$hcv_pct_of_total >= 5 & hcvchar$hcv_pct_of_total < 6] = "5-6%"
hcvchar$category[hcvchar$hcv_pct_of_total >= 6 & hcvchar$hcv_pct_of_total < 7] = "6-7%"
hcvchar$category[hcvchar$hcv_pct_of_total >= 7 & hcvchar$hcv_pct_of_total < 8] = "7-8%"
hcvchar$category[hcvchar$hcv_pct_of_total >= 8 & hcvchar$hcv_pct_of_total < 9] = "8-9%"
hcvchar$category[hcvchar$hcv_pct_of_total >= 9 & hcvchar$hcv_pct_of_total < 10] = "9-10%"
hcvchar$category[hcvchar$hcv_pct_of_total >= 10] = "10+%"


hcvchar$category = factor(hcvchar$category, levels = c("0-1%", "1-2%", "2-3%",
                                                       "3-4%", "4-5%", "5-6%", "6-7%", "7-8%", "8-9%", "9-10%", "10+%", "No HCV Residents"))

#with a line
ggplot(hcvchar) + geom_boxplot(mapping = aes(x = category, y = pctcollplus)) +
  geom_boxplot(data = highlight, aes(x = category, y = pctcollplus), color = 'red') +
  labs(x = "Percentage of HCV Residents in Tract", y = "Percentage of Residents with at least a College Degree")

#with a dot

install.packages("ggstar")
library(ggstar)


#frequency table of population and number of census tracts
freqtable <- table(hcvchar$category)
freqtable
frequencies <- as.data.frame(freqtable)
names(frequencies)[names(frequencies) == "Percent_HCV"] <- "category"

poptable <- hcvchar %>% group_by(category) %>% summarise_at(vars(population), list(pop = sum))

freqandpop <- merge(frequencies, poptable, by = "category")
names(freqandpop)[names(freqandpop) == "category"] <- "Percent_HCV"
names(freqandpop)[names(freqandpop) == "Freq"] <- "Number of Census Tracts"
names(freqandpop)[names(freqandpop) == "pop"] <- "Estimated Population"




ggplot(hcvchar) + geom_boxplot(mapping = aes(x = category, y = pctcollplus, fill = hhinc_med)) +
  geom_star(data = highlight, aes(x = category, y = pctcollplus), color = 'red', fill = 'red', size = 5) +
  #stat_summary(fun = stat_box_data, geom = "text", mapping = (aes (y = pctcollplus, x = category)), hjust = 0.5, vjust = 0.9) +
  labs(x = "Percentage of HCV Residents in Tract", y = "Percentage of Residents with at least a College Degree")

"stat_box_data <- function(y, upper_limit = 100) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('count =', length(y), '\n',
                    'mean =', round(mean(y), 1), '\n')
    )
  )
}"

#final graphs

highlight <- hcvchar %>% filter(NAME == "Census Tract 3.01, Durham County, North Carolina")

ggplot(hcvchar) + geom_boxplot(mapping = aes(x = category, y = pctcollplus)) +
  geom_star(data = highlight, aes(x = category, y = pctcollplus), color = 'red', fill = 'red', size = 5) +
  labs(x = "Percentage of HCV Residents in Tract", y = "Percentage of Residents with at least a College Degree")

ggplot(hcvchar) + geom_boxplot(mapping = aes(x = category, y = mean_gross_rent_2bed)) +
  geom_star(data = highlight, aes(x = category, y = mean_gross_rent_2bed), color = 'red', fill = 'red', size = 5) +
  labs(x = "Percentage of HCV Residents in Tract", y = "Mean Gross Rent of a Two Bedroom Apartment")




ggplot(hcvchar) + geom_dotplot(mapping = aes(x = category, y = pctcollplus)) +
  labs(x = "Percentage of HCV Residents", y = "Percentage of Residents with at least a College Degree")

ggplot(hcvchar) + geom_point(mapping = aes(x = hcv_pct_of_total, y = pctcollplus)) +
  labs(x = "Percentage of HCV Residents", y = "Percentage of Residents with at least a College Degree")

ggplot(hcvchar, aes(pctcollplus, fill = category)) + geom_histogram(bins = 10)


#maps?
nc_counties <- map_data("county", "north carolina") %>% 
  select(lon = long, lat, group, id = subregion)
head(nc_counties)

ggplot(nc_counties, aes(lon, lat, group = group)) +
  geom_polygon(fill = "white", colour = "grey50") + 
  coord_quickmap()


#making mean rent 2 bed categorical

#FIVE BINS 500-800, 800-1000, 1000-1200, 1200-1400, 1400+
hcvchar$mrent_cat[hcvchar$mean_gross_rent_2bed >= 500 & hcvchar$mean_gross_rent_2bed < 800] = "$500-$800"
hcvchar$mrent_cat[hcvchar$mean_gross_rent_2bed >= 800 & hcvchar$mean_gross_rent_2bed < 1000] = "$800-$1000"
hcvchar$mrent_cat[hcvchar$mean_gross_rent_2bed >= 1000 & hcvchar$mean_gross_rent_2bed < 1200] = "$1000-$1200"
hcvchar$mrent_cat[hcvchar$mean_gross_rent_2bed >= 1200 & hcvchar$mean_gross_rent_2bed < 1500] = "$1200-$1500"
hcvchar$mrent_cat[hcvchar$mean_gross_rent_2bed > 1500] = "$1500+"

hcvchar$mrent_cat = factor(hcvchar$mrent_cat, levels = c("$500-$800", "$800-$1000", "$1000-$1200", "$1200-$1500", "$1500+"))

ggplot(hcvchar) + geom_boxplot(mapping = aes(x = mrent_cat, y = hcv_pct_of_total)) +
  labs(x = "Mean Rent of a 2 Bedroom Apartment", y = "Percentage of HCV Residents in Tract")

ggplot(hcvchar) + geom_bar(mapping = aes(x = mrent_cat, fill = category)) +
  labs(x = "Mean Rent of a 2 Bedroom Apartment", y = "Count") +
  scale_fill_manual(values = c("#b8c7f4", "#a7b9f1", "#859eec", "#6384e6", "#4169e1", 
                               "#2250d9", "#1d43b7", "#173795", "#122a73", "#fbfcfe"))

#put in order and then manually assign colors
ggplot(data = subset(hcvchar, !is.na(mrent_cat))) + geom_bar(mapping = aes(x = mrent_cat, fill = as.factor(hcv_pct_of_total))) +
  labs(x = "Mean Rent of a 2 Bedroom Apartment in Census Tract", y = "Number of Census Tracts") +
  scale_fill_manual(values = c("#fbfcfe", "#e3eafc", "#d9e1f9", "#c9d4f6", "#b8c7f4", "#a7b9f1", "#9db2f2", "#96acee",
                               "#859eec", "#7491e9", "#6384e6", "#527cfa", "#5276e4", "#4169e1", "#305cde",
                               "#2250d9", "#204ac8", "#1d43b7", "#1a3da6", "#173795", "#153184",
                               "#122a73", "#0f2462", "#0d1e51", "#0a1840", "#07112f", "#050b1e")) +
  theme(legend.position = "none") +
  geom_star(data = highlight, aes(x = mrent_cat, y = hcv_pct_of_total), color = 'red', fill = 'red', size = 5)

highlight <- hcvchar %>% filter(NAME == "Census Tract 3.01, Durham County, North Carolina")

#making household median income categorical
#2000 - 10000, 10000 - 20000, 20000 - 30000, 30000 - 40000, 40000 - 50000, 50000+

hcvchar$inc_cat[hcvchar$hhinc_med >= 2000 & hcvchar$hhinc_med < 10000] = "$2000-$10000"
hcvchar$inc_cat[hcvchar$hhinc_med >= 10000 & hcvchar$hhinc_med < 20000] = "$10000-$20000"
hcvchar$inc_cat[hcvchar$hhinc_med >= 20000 & hcvchar$hhinc_med < 30000] = "$20000-$30000"
hcvchar$inc_cat[hcvchar$hhinc_med >= 30000 & hcvchar$hhinc_med < 40000] = "$30000-$40000"
hcvchar$inc_cat[hcvchar$hhinc_med >= 40000 & hcvchar$hhinc_med < 50000] = "$40000-$50000"
hcvchar$inc_cat[hcvchar$hhinc_med >= 50000] = "$50000+"

ggplot(data = subset(hcvchar, !is.na(inc_cat))) + geom_bar(mapping = aes(x = inc_cat, fill = as.factor(hcv_pct_of_total))) +
  labs(x = "Median Household Income in Census Tract", y = "Number of Census Tracts", title = "Percent of HCV Residents in Tract by Median Household Income") +
  scale_fill_manual(values = c("#fbfcfe", "#e3eafc", "#d9e1f9", "#c9d4f6", "#b8c7f4", "#a7b9f1", "#9db2f2", "#96acee",
                               "#859eec", "#7491e9", "#6384e6", "#527cfa", "#5276e4", "#4169e1", "#305cde",
                               "#2250d9", "#204ac8", "#1d43b7", "#1a3da6", "#173795", "#153184",
                               "#122a73", "#0f2462", "#0d1e51", "#0a1840", "#07112f", "#050b1e")) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.text.x = element_text(angle = 90)) +
  geom_star(data = highlight, aes(x = inc_cat, y = hcv_pct_of_total), color = 'red', fill = 'red', size = 5)

hcvchar$inc_cat = factor(hcvchar$inc_cat, levels = c("$2000-$10000", "$10000-$20000", "$20000-$30000", "$30000-$40000", "$40000-$50000", "$50000+"))


# percent below poverty

hcvchar$pov_cat[hcvchar$pctbelowpoverty > 0 & hcvchar$pctbelowpoverty < 10] = "0%-10%"
hcvchar$pov_cat[hcvchar$pctbelowpoverty >= 10 & hcvchar$pctbelowpoverty < 20] = "10%-20%"
hcvchar$pov_cat[hcvchar$pctbelowpoverty >= 20 & hcvchar$pctbelowpoverty < 30] = "20%-30%"
hcvchar$pov_cat[hcvchar$pctbelowpoverty >= 30 & hcvchar$pctbelowpoverty < 40] = "30%-40%"
hcvchar$pov_cat[hcvchar$pctbelowpoverty >= 40 & hcvchar$pctbelowpoverty < 50] = "40%-50%"
hcvchar$pov_cat[hcvchar$pctbelowpoverty >= 50] = "50%+"


ggplot(data = subset(hcvchar, !is.na(pov_cat))) + geom_bar(mapping = aes(x = pov_cat, fill = as.factor(hcv_pct_of_total))) +
  labs(x = "Percent Below Poverty in Census Tract", y = "Number of Census Tracts") +
  scale_fill_manual(values = c("#fbfcfe", "#e3eafc", "#d9e1f9", "#c9d4f6", "#b8c7f4", "#a7b9f1", "#9db2f2", "#96acee",
                               "#859eec", "#7491e9", "#6384e6", "#527cfa", "#5276e4", "#4169e1", "#305cde",
                               "#2250d9", "#204ac8", "#1d43b7", "#1a3da6", "#173795", "#153184",
                               "#122a73", "#0f2462", "#0d1e51", "#0a1840", "#07112f", "#050b1e")) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90)) +
  geom_star(data = highlight, aes(x = pov_cat, y = hcv_pct_of_total), color = 'red', fill = 'red', size = 5)


# population density

hcvchar$popdens_cat[hcvchar$popdensity >= 0 & hcvchar$popdensity < 1000] = "0-1000"
hcvchar$popdens_cat[hcvchar$popdensity >= 1000 & hcvchar$popdensity < 2000] = "1000-2000"
hcvchar$popdens_cat[hcvchar$popdensity >= 2000 & hcvchar$popdensity < 3000] = "2000-3000"
hcvchar$popdens_cat[hcvchar$popdensity >= 3000 & hcvchar$popdensity < 4000] = "3000-4000"
hcvchar$popdens_cat[hcvchar$popdensity >= 4000 & hcvchar$popdensity < 5000] = "4000-5000"
hcvchar$popdens_cat[hcvchar$popdensity >= 5000 & hcvchar$popdensity < 6000] = "5000-6000"
hcvchar$popdens_cat[hcvchar$popdensity >= 6000] = "6000+"

ggplot(data = subset(hcvchar, !is.na(popdens_cat))) + geom_bar(mapping = aes(x = popdens_cat, fill = as.factor(hcv_pct_of_total))) +
  labs(x = "Population Density (people / square mile) in Census Tract", y = "Number of Census Tracts") +
  scale_fill_manual(values = c("#fbfcfe", "#e3eafc", "#d9e1f9", "#c9d4f6", "#b8c7f4", "#a7b9f1", "#9db2f2", "#96acee",
                               "#859eec", "#7491e9", "#6384e6", "#527cfa", "#5276e4", "#4169e1", "#305cde",
                               "#2250d9", "#204ac8", "#1d43b7", "#1a3da6", "#173795", "#153184",
                               "#122a73", "#0f2462", "#0d1e51", "#0a1840", "#07112f", "#050b1e")) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90)) +
  geom_star(data = highlight, aes(x = popdens_cat, y = hcv_pct_of_total), color = 'red', fill = 'red', size = 5)

ggplot(data = subset(hcvchar, !is.na(popdens_cat)), aes(x = popdens_cat, group = hcv_pct_of_total, fill = hcv_pct_of_total)) + geom_bar() +
  labs(x = "Population Density (people / square mile) in Census Tract", y = "Number of Census Tracts") +
  scale_colour_gradient(low = "white", high = "blue") +
  theme(axis.text.x = element_text(angle = 90))


# wage growth
hcvchar$wage_cat[hcvchar$wagegrowth  < -1] = "Less than -1%"
hcvchar$wage_cat[hcvchar$wagegrowth >= -1 & hcvchar$wagegrowth < -0.5] = "-1% to -0.5%"
hcvchar$wage_cat[hcvchar$wagegrowth >= -0.5 & hcvchar$wagegrowth < 0] = "-0.5% to 0%"
hcvchar$wage_cat[hcvchar$wagegrowth >= 0 & hcvchar$wagegrowth < 0.5] = "0% to 0.5%"
hcvchar$wage_cat[hcvchar$wagegrowth >= 0.5 & hcvchar$wagegrowth < 1] = "0.5% to 1%"
hcvchar$wage_cat[hcvchar$wagegrowth  > 1] = "Greater than 1%"

hcvchar$wage_cat = factor(hcvchar$wage_cat, levels = c("Less than -1%", "-1% to -0.5%", "-0.5% to 0%", "0% to 0.5%", 
                                                             "0.5% to 1%", "Greater than 1%"))


ggplot(data = subset(hcvchar, !is.na(wage_cat))) + geom_bar(mapping = aes(x = wage_cat, fill = as.factor(hcv_pct_of_total))) +
  labs(x = "Wage Growth (2014 to 2019) in Census Tract", y = "Number of Census Tracts") +
  scale_fill_manual(values = c("#fbfcfe", "#e3eafc", "#d9e1f9", "#c9d4f6", "#b8c7f4", "#a7b9f1", "#9db2f2", "#96acee",
                               "#859eec", "#7491e9", "#6384e6", "#527cfa", "#5276e4", "#4169e1", "#305cde",
                               "#2250d9", "#204ac8", "#1d43b7", "#1a3da6", "#173795", "#153184",
                               "#122a73", "#0f2462", "#0d1e51", "#0a1840", "#07112f", "#050b1e")) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90)) +
  geom_star(data = highlight, aes(x = wage_cat, y = hcv_pct_of_total), color = 'red', fill = 'red', size = 5)


# single parent
hcvchar$parent_cat[hcvchar$percent_parent_alone  > 0 & hcvchar$percent_parent_alone < 0.20] = "0%-20%"
hcvchar$parent_cat[hcvchar$percent_parent_alone  >= 0.20 & hcvchar$percent_parent_alone < 0.30] = "20%-30%"
hcvchar$parent_cat[hcvchar$percent_parent_alone  >= 0.30 & hcvchar$percent_parent_alone < 0.40] = "30%-40%"
hcvchar$parent_cat[hcvchar$percent_parent_alone  >= 0.40 & hcvchar$percent_parent_alone < 0.50] = "40%-50%"
hcvchar$parent_cat[hcvchar$percent_parent_alone  >= 0.50] = "50+%"

ggplot(data = subset(hcvchar, !is.na(parent_cat))) + geom_bar(mapping = aes(x = parent_cat, fill = as.factor(hcv_pct_of_total))) +
  labs(x = "Percentage of Single Parent Homes in Census Tract", y = "Number of Census Tracts") +
  scale_fill_manual(values = c("#fbfcfe", "#e3eafc", "#d9e1f9", "#c9d4f6", "#b8c7f4", "#a7b9f1", "#9db2f2", "#96acee",
                               "#859eec", "#7491e9", "#6384e6", "#527cfa", "#5276e4", "#4169e1", "#305cde",
                               "#2250d9", "#204ac8", "#1d43b7", "#1a3da6", "#173795", "#153184",
                               "#122a73", "#0f2462", "#0d1e51", "#0a1840", "#07112f", "#050b1e")) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90)) +
  geom_star(data = highlight, aes(x = parent_cat, y = hcv_pct_of_total), color = 'red', fill = 'red', size = 5)

# race

graph_for_race <- hcvchar %>% select(category, percent_white, percent_blackorafricanamerican)

ggplot(data = graph_for_race, aes(x = category, y = percent_white, y = percent_blackorafricanamerican)) + geom_col(position = position_dodge())
ggplot(data = graph_for_race, aes(x = category, y = percent_blackorafricanamerican)) + geom_col(position = position_dodge())

final_racegraph <- graph_for_race %>% group_by(category) %>%
  summarise_at(vars(percent_white, percent_blackorafricanamerican), list(name = mean))

# 1st way

scaleFUN <- function(x) sprintf("%.2f", x)

final_racegraph %>% gather(key, val, -category) %>%
  ggplot(aes(category, val, fill = key)) +
  geom_col(position = "dodge2") +
  labs(x = "Percent of HCV Residents", y = "Percent Race in those Census Tracts") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scaleFUN)


hcvchar$white_cat[hcvchar$percent_white  > 0 & hcvchar$percent_white < 0.25] = "0%-25%"
hcvchar$white_cat[hcvchar$percent_white  > 0.25 & hcvchar$percent_white < 0.50] = "25%-50%"
hcvchar$white_cat[hcvchar$percent_white  > 0.50 & hcvchar$percent_white < 0.75] = "50%-75%"
hcvchar$white_cat[hcvchar$percent_white  > 0.75 & hcvchar$percent_white < 1.00] = "75%-100%"

hcvchar$black_cat[hcvchar$percent_blackorafricanamerican > 0 & hcvchar$percent_blackorafricanamerican < 0.25] = "0%-25%"
hcvchar$black_cat[hcvchar$percent_blackorafricanamerican  > 0.25 & hcvchar$percent_blackorafricanamerican < 0.50] = "25%-50%"
hcvchar$black_cat[hcvchar$percent_blackorafricanamerican  > 0.50 & hcvchar$percent_blackorafricanamerican < 0.75] = "50%-75%"
hcvchar$black_cat[hcvchar$percent_blackorafricanamerican  > 0.75 & hcvchar$percent_blackorafricanamerican < 1.00] = "75%-100%"


graph_white <- ggplot(data = subset(hcvchar, !is.na(white_cat))) + geom_bar(mapping = aes(x = white_cat, fill = as.factor(hcv_pct_of_total))) +
  labs(x = "Percentage of White Residents", y = "Number of Census Tracts") +
  scale_fill_manual(values = c("#fbfcfe", "#d9e1f9", "#c9d4f6", "#b8c7f4", "#a7b9f1", "#96acee",
                               "#859eec", "#7491e9", "#6384e6", "#5276e4", "#4169e1", "#305cde",
                               "#2250d9", "#204ac8", "#1d43b7", "#1a3da6", "#173795", "#153184",
                               "#122a73", "#0f2462", "#0d1e51", "#0a1840", "#07112f", "#050b1e")) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90)) +
  geom_star(data = highlight, aes(x = white_cat, y = hcv_pct_of_total), color = 'red', fill = 'red', size = 5)


graph_black <- ggplot(data = subset(hcvchar, !is.na(black_cat))) + geom_bar(mapping = aes(x = black_cat, fill = as.factor(hcv_pct_of_total))) +
  labs(x = "Percentage of Black Residents", y = "Number of Census Tracts") +
  scale_fill_manual(values = c("#fbfcfe", "#d9e1f9", "#c9d4f6", "#b8c7f4", "#a7b9f1", "#96acee",
                               "#859eec", "#7491e9", "#6384e6", "#5276e4", "#4169e1", "#305cde",
                               "#2250d9", "#204ac8", "#1d43b7", "#1a3da6", "#173795", "#153184",
                               "#122a73", "#0f2462", "#0d1e51", "#0a1840", "#07112f", "#050b1e")) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90)) +
  geom_star(data = highlight, aes(x = black_cat, y = hcv_pct_of_total), color = 'red', fill = 'red', size = 5)

hcvchar$anotherrace <- hcvchar$percent_americanindianoralaskanative + hcvchar$percent_asian + hcvchar$percent_nativehawaiianorpacificislander +
  hcvchar$percent_other_or_2plus

hcvchar$other_cat[hcvchar$anotherrace > 0 & hcvchar$anotherrace < 0.25] = "0%-25%"
hcvchar$other_cat[hcvchar$anotherrace > 0.25 & hcvchar$anotherrace < 0.50] = "25%-50%"
hcvchar$other_cat[hcvchar$anotherrace > 0.50 & hcvchar$anotherrace < 0.75] = "50%-75%"
hcvchar$other_cat[hcvchar$anotherrace > 0.75 & hcvchar$anotherrace < 1] = "75%-100%"


graph_other_race <- ggplot(data = subset(hcvchar, !is.na(other_cat))) + geom_bar(mapping = aes(x = other_cat, fill = as.factor(hcv_pct_of_total))) +
  labs(x = "Other (Percent)", y = "Number of Census Tracts") +
  scale_fill_manual(values = c("#fbfcfe", "#e3eafc", "#d9e1f9", "#c9d4f6", "#b8c7f4", "#a7b9f1", "#9db2f2", "#96acee",
                               "#859eec", "#7491e9", "#6384e6", "#527cfa", "#5276e4", "#4169e1", "#305cde",
                               "#2250d9", "#204ac8", "#1d43b7", "#1a3da6", "#173795", "#153184",
                               "#122a73", "#0f2462", "#0d1e51", "#0a1840", "#07112f", "#050b1e")) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90))

p <- plot_grid(graph_white, graph_black)
plot(graph_white, graph_black)
race_title <- ggdraw() + draw_label("Percent of HCV Residents in Tract by Racial Demographics")

plot_grid(race_title, p, ncol = 1, rel_heights=c(0.1, 1))














