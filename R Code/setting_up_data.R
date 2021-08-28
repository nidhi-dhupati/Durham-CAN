# merging census data

library(tidyverse)

install.packages(c("httr", "jsonlite"))
library(httr)
library(jsonlite)
library(ggplot2)
library(sp)

install.packages("maps")
library(maps)

#get data
hcv2 = read.csv('Housing_Choice_Vouchers_By_Tract.csv')
hcv2 <- filter(hcv2, COUNTY == "63")

#filter and replace NA with 0
hcv <- hcv %>% select(GEOID, HCV_PUBLIC, HCV_PCT_RENTER_OCC_UNITS)
hcv[is.na(hcv)] <- 0

# only durham county census tracts
hcv <- filter(hcv, GEOID == "37063000301" | GEOID == "37063000302" | GEOID == "37063000401" |
       GEOID == "37063000402" | GEOID == "37063000500" | GEOID == "37063000600"  | 
         GEOID == "37063000700" | GEOID == "37063000900" | GEOID == "37063001001" |
         GEOID == "37063001002" | GEOID == "37063001100" | GEOID == "37063001301" | 
         GEOID == "37063001303" | GEOID == "37063001304" | GEOID == "37063001400" | 
         GEOID == "37063001501" | GEOID == "37063001502" | GEOID == "37063001503" |
         GEOID == "37063001601" | GEOID == "37063001603" | GEOID == "37063001604" |
         GEOID == "37063001705" | GEOID == "37063001706" | GEOID == "37063001707" |
         GEOID == "37063001708" | GEOID == "37063001709" | GEOID == "37063001710" | 
         GEOID == "37063001711" | GEOID == "37063001801" | GEOID == "37063001802" | 
         GEOID == "37063001806" | GEOID == "37063001807" | GEOID == "37063001808" |
         GEOID == "37063001809" | GEOID == "37063001900" | GEOID == "37063002007" |
         GEOID == "37063002008" | GEOID == "37063002009" | GEOID == "37063002013" |
         GEOID == "37063002015" | GEOID == "37063002016" | GEOID == "37063002017" | 
         GEOID == "37063002018" | GEOID == "37063002019" | GEOID == "37063002020" |
         GEOID == "37063002021" | GEOID == "37063002022" | GEOID == "37063002023" |
         GEOID == "37063002024" | GEOID == "37063002025" | GEOID == "37063002026" |
         GEOID == "37063002027" | GEOID == "37063002028" | GEOID == "37063002100" |
         GEOID == "37063002200" | GEOID == "37063002300")

sum(hcv$HCV_PUBLIC) #different number - 2459?

hcv$hcv_pct_of_total <- (100*hcv$HCV_PUBLIC) / 2541

# median income
# B06011_001E is median income
res1 = GET("https://api.census.gov/data/2019/acs/acs5?get=NAME,B06011_001E&for=tract:*&in=state:37&in=county:063")
rawToChar(res1$content)
censdf1 = fromJSON(rawToChar(res1$content))
res1

colnames(censdf1) <- censdf1[1, ]
censdf1 <- censdf1[-c(1), ]
censdf1 <- as.data.frame(censdf1)
names(censdf1)[names(censdf1) == "B06011_001E"] <- "hhinc_med"

censdf1[, c(2:5)] <- sapply(censdf1[, c(2:5)], as.numeric)

censdf1$GEOID <- ((censdf1$state) * 1000000000 + (censdf1$county) * 1000000 + censdf1$tract)

censdf1 <- subset(censdf1, GEOID %in% c(37063000301, 37063000302, 37063000401, 37063000402, 37063000500, 37063000600, 37063000700, 37063000900, 
                                        37063001001, 37063001002, 37063001100, 37063001301, 37063001303, 37063001304, 37063001400, 37063001501, 
                                        37063001502, 37063001503, 37063001601, 37063001603, 37063001604, 37063001705, 37063001706, 37063001707, 
                                        37063001708, 37063001709, 37063001710, 37063001711, 37063001801, 37063001802, 37063001806, 37063001807, 
                                        37063001808, 37063001809, 37063001900, 37063002007, 37063002008, 37063002009, 37063002013, 37063002015, 
                                        37063002016, 37063002017, 37063002018, 37063002019, 37063002020, 37063002021, 37063002022, 37063002023, 
                                        37063002024, 37063002025, 37063002026, 37063002027, 37063002028, 37063002100, 37063002200, 37063002300))

# rate of employment
# B23025_002E is inlaborfrc, B23025_001E is population
res2 = GET("https://api.census.gov/data/2019/acs/acs5?get=NAME,B23025_002E,B23025_001E&for=tract:*&in=state:37&in=county:063")
rawToChar(res2$content)
censdf2 = fromJSON(rawToChar(res2$content))
res2

colnames(censdf2) <- censdf2[1, ]
censdf2 <- censdf2[-c(1), ]
censdf2 <- as.data.frame(censdf2)
names(censdf2)[names(censdf2) == "B23025_002E"] <- "inlaborfrc"
names(censdf2)[names(censdf2) == "B23025_001E"] <- "population"

censdf2[, c(2:6)] <- sapply(censdf2[, c(2:6)], as.numeric)

censdf2$pctemploy <- (100 * censdf2$inlaborfrc) / (censdf2$population)
censdf2$GEOID <- ((censdf2$state) * 1000000000 + (censdf2$county) * 1000000 + censdf2$tract)

censdf2 <- subset(censdf2, GEOID %in% c(37063000301, 37063000302, 37063000401, 37063000402, 37063000500, 37063000600, 37063000700, 37063000900, 
                                        37063001001, 37063001002, 37063001100, 37063001301, 37063001303, 37063001304, 37063001400, 37063001501, 
                                        37063001502, 37063001503, 37063001601, 37063001603, 37063001604, 37063001705, 37063001706, 37063001707, 
                                        37063001708, 37063001709, 37063001710, 37063001711, 37063001801, 37063001802, 37063001806, 37063001807, 
                                        37063001808, 37063001809, 37063001900, 37063002007, 37063002008, 37063002009, 37063002013, 37063002015, 
                                        37063002016, 37063002017, 37063002018, 37063002019, 37063002020, 37063002021, 37063002022, 37063002023, 
                                        37063002024, 37063002025, 37063002026, 37063002027, 37063002028, 37063002100, 37063002200, 37063002300))



# percentage of population with BS or more
# B23006_001E is popest, B23006_023E is collplusest
res3 = GET("https://api.census.gov/data/2019/acs/acs5?get=NAME,B23006_001E,B23006_023E&for=tract:*&in=state:37&in=county:063")
rawToChar(res3$content)
censdf3 = fromJSON(rawToChar(res3$content))
res3

colnames(censdf3) <- censdf3[1, ]
censdf3 <- censdf3[-c(1), ]
censdf3 <- as.data.frame(censdf3)
names(censdf3)[names(censdf3) == "B23006_001E"] <- "popest"
names(censdf3)[names(censdf3) == "B23006_023E"] <- "collplusest"

censdf3[, c(2:6)] <- sapply(censdf3[, c(2:6)], as.numeric)

censdf3$pctcollplus <- (100 * censdf3$collplusest) / (censdf3$popest)
censdf3$GEOID <- ((censdf3$state) * 1000000000 + (censdf3$county) * 1000000 + censdf3$tract)

censdf3 <- subset(censdf3, GEOID %in% c(37063000301, 37063000302, 37063000401, 37063000402, 37063000500, 37063000600, 37063000700, 37063000900, 
                        37063001001, 37063001002, 37063001100, 37063001301, 37063001303, 37063001304, 37063001400, 37063001501, 
                        37063001502, 37063001503, 37063001601, 37063001603, 37063001604, 37063001705, 37063001706, 37063001707, 
                        37063001708, 37063001709, 37063001710, 37063001711, 37063001801, 37063001802, 37063001806, 37063001807, 
                        37063001808, 37063001809, 37063001900, 37063002007, 37063002008, 37063002009, 37063002013, 37063002015, 
                        37063002016, 37063002017, 37063002018, 37063002019, 37063002020, 37063002021, 37063002022, 37063002023, 
                        37063002024, 37063002025, 37063002026, 37063002027, 37063002028, 37063002100, 37063002200, 37063002300))

# mean commute time - less than 10 minutes

res4_1 = GET("https://api.census.gov/data/2019/acs/acs5?get=NAME,B08134_001E,B08134_002E&for=tract:*&in=state:37&in=county:063")
rawToChar(res4_1$content)
censdf4_1 = fromJSON(rawToChar(res4_1$content))
res4_1

colnames(censdf4_1) <- censdf4_1[1, ]
censdf4_1 <- censdf4_1[-c(1), ]
censdf4_1 <- as.data.frame(censdf4_1)
names(censdf4_1)[names(censdf4_1) == "B08134_001E"] <- "commute_pop"
names(censdf4_1)[names(censdf4_1) == "B08134_002E"] <- "lessthan10"


censdf4_1[, c(2:6)] <- sapply(censdf4_1[, c(2:6)], as.numeric)

censdf4_1$pctless10 <- (100 * censdf4_1$lessthan10) / (censdf4_1$commute_pop)
censdf4_1$GEOID <- ((censdf4_1$state) * 1000000000 + (censdf4_1$county) * 1000000 + censdf4_1$tract)

censdf4_1 <- subset(censdf4_1, GEOID %in% c(37063000301, 37063000302, 37063000401, 37063000402, 37063000500, 37063000600, 37063000700, 37063000900, 
                                        37063001001, 37063001002, 37063001100, 37063001301, 37063001303, 37063001304, 37063001400, 37063001501, 
                                        37063001502, 37063001503, 37063001601, 37063001603, 37063001604, 37063001705, 37063001706, 37063001707, 
                                        37063001708, 37063001709, 37063001710, 37063001711, 37063001801, 37063001802, 37063001806, 37063001807, 
                                        37063001808, 37063001809, 37063001900, 37063002007, 37063002008, 37063002009, 37063002013, 37063002015, 
                                        37063002016, 37063002017, 37063002018, 37063002019, 37063002020, 37063002021, 37063002022, 37063002023, 
                                        37063002024, 37063002025, 37063002026, 37063002027, 37063002028, 37063002100, 37063002200, 37063002300))


# mean commute time - 10 to 14 minutes

res4_2 = GET("https://api.census.gov/data/2019/acs/acs5?get=NAME,B08134_001E,B08134_003E&for=tract:*&in=state:37&in=county:063")
rawToChar(res4_2$content)
censdf4_2 = fromJSON(rawToChar(res4_2$content))
res4_2

colnames(censdf4_2) <- censdf4_2[1, ]
censdf4_2 <- censdf4_2[-c(1), ]
censdf4_2 <- as.data.frame(censdf4_2)
names(censdf4_2)[names(censdf4_2) == "B08134_001E"] <- "commute_pop"
names(censdf4_2)[names(censdf4_2) == "B08134_003E"] <- "10to14"


censdf4_2[, c(2:6)] <- sapply(censdf4_2[, c(2:6)], as.numeric)

censdf4_2$pct10to14 <- (100 * censdf4_2$"10to14") / (censdf4_2$commute_pop)
censdf4_2$GEOID <- ((censdf4_2$state) * 1000000000 + (censdf4_2$county) * 1000000 + censdf4_2$tract)


censdf4_2 <- subset(censdf4_2, GEOID %in% c(37063000301, 37063000302, 37063000401, 37063000402, 37063000500, 37063000600, 37063000700, 37063000900, 
                                            37063001001, 37063001002, 37063001100, 37063001301, 37063001303, 37063001304, 37063001400, 37063001501, 
                                            37063001502, 37063001503, 37063001601, 37063001603, 37063001604, 37063001705, 37063001706, 37063001707, 
                                            37063001708, 37063001709, 37063001710, 37063001711, 37063001801, 37063001802, 37063001806, 37063001807, 
                                            37063001808, 37063001809, 37063001900, 37063002007, 37063002008, 37063002009, 37063002013, 37063002015, 
                                            37063002016, 37063002017, 37063002018, 37063002019, 37063002020, 37063002021, 37063002022, 37063002023, 
                                            37063002024, 37063002025, 37063002026, 37063002027, 37063002028, 37063002100, 37063002200, 37063002300))

# mean commute time - 15 to 19 minutes

res4_3 = GET("https://api.census.gov/data/2019/acs/acs5?get=NAME,B08134_001E,B08134_004E&for=tract:*&in=state:37&in=county:063")
rawToChar(res4_3$content)
censdf4_3 = fromJSON(rawToChar(res4_3$content))
res4_3

colnames(censdf4_3) <- censdf4_3[1, ]
censdf4_3 <- censdf4_3[-c(1), ]
censdf4_3 <- as.data.frame(censdf4_3)
names(censdf4_3)[names(censdf4_3) == "B08134_001E"] <- "commute_pop"
names(censdf4_3)[names(censdf4_3) == "B08134_004E"] <- "15to19"


censdf4_3[, c(2:6)] <- sapply(censdf4_3[, c(2:6)], as.numeric)

censdf4_3$pct15to19 <- (100 * censdf4_3$"15to19") / (censdf4_3$commute_pop)
censdf4_3$GEOID <- ((censdf4_3$state) * 1000000000 + (censdf4_3$county) * 1000000 + censdf4_3$tract)


censdf4_3 <- subset(censdf4_3, GEOID %in% c(37063000301, 37063000302, 37063000401, 37063000402, 37063000500, 37063000600, 37063000700, 37063000900, 
                                            37063001001, 37063001002, 37063001100, 37063001301, 37063001303, 37063001304, 37063001400, 37063001501, 
                                            37063001502, 37063001503, 37063001601, 37063001603, 37063001604, 37063001705, 37063001706, 37063001707, 
                                            37063001708, 37063001709, 37063001710, 37063001711, 37063001801, 37063001802, 37063001806, 37063001807, 
                                            37063001808, 37063001809, 37063001900, 37063002007, 37063002008, 37063002009, 37063002013, 37063002015, 
                                            37063002016, 37063002017, 37063002018, 37063002019, 37063002020, 37063002021, 37063002022, 37063002023, 
                                            37063002024, 37063002025, 37063002026, 37063002027, 37063002028, 37063002100, 37063002200, 37063002300))

# mean commute time - 20 to 24 minutes

res4_4 = GET("https://api.census.gov/data/2019/acs/acs5?get=NAME,B08134_001E,B08134_005E&for=tract:*&in=state:37&in=county:063")
rawToChar(res4_4$content)
censdf4_4 = fromJSON(rawToChar(res4_4$content))
res4_4

colnames(censdf4_4) <- censdf4_4[1, ]
censdf4_4 <- censdf4_4[-c(1), ]
censdf4_4 <- as.data.frame(censdf4_4)
names(censdf4_4)[names(censdf4_4) == "B08134_001E"] <- "commute_pop"
names(censdf4_4)[names(censdf4_4) == "B08134_005E"] <- "20to24"


censdf4_4[, c(2:6)] <- sapply(censdf4_4[, c(2:6)], as.numeric)

censdf4_4$pct20to24 <- (100 * censdf4_4$"20to24") / (censdf4_4$commute_pop)
censdf4_4$GEOID <- ((censdf4_4$state) * 1000000000 + (censdf4_4$county) * 1000000 + censdf4_4$tract)


censdf4_4 <- subset(censdf4_4, GEOID %in% c(37063000301, 37063000302, 37063000401, 37063000402, 37063000500, 37063000600, 37063000700, 37063000900, 
                                            37063001001, 37063001002, 37063001100, 37063001301, 37063001303, 37063001304, 37063001400, 37063001501, 
                                            37063001502, 37063001503, 37063001601, 37063001603, 37063001604, 37063001705, 37063001706, 37063001707, 
                                            37063001708, 37063001709, 37063001710, 37063001711, 37063001801, 37063001802, 37063001806, 37063001807, 
                                            37063001808, 37063001809, 37063001900, 37063002007, 37063002008, 37063002009, 37063002013, 37063002015, 
                                            37063002016, 37063002017, 37063002018, 37063002019, 37063002020, 37063002021, 37063002022, 37063002023, 
                                            37063002024, 37063002025, 37063002026, 37063002027, 37063002028, 37063002100, 37063002200, 37063002300))

# mean commute time - 25 to 29 minutes

res4_5 = GET("https://api.census.gov/data/2019/acs/acs5?get=NAME,B08134_001E,B08134_006E&for=tract:*&in=state:37&in=county:063")
rawToChar(res4_5$content)
censdf4_5 = fromJSON(rawToChar(res4_5$content))
res4_5

colnames(censdf4_5) <- censdf4_5[1, ]
censdf4_5 <- censdf4_5[-c(1), ]
censdf4_5 <- as.data.frame(censdf4_5)
names(censdf4_5)[names(censdf4_5) == "B08134_001E"] <- "commute_pop"
names(censdf4_5)[names(censdf4_5) == "B08134_006E"] <- "25to29"


censdf4_5[, c(2:6)] <- sapply(censdf4_5[, c(2:6)], as.numeric)

censdf4_5$pct25to29 <- (100 * censdf4_5$"25to29") / (censdf4_5$commute_pop)
censdf4_5$GEOID <- ((censdf4_5$state) * 1000000000 + (censdf4_5$county) * 1000000 + censdf4_5$tract)


censdf4_5 <- subset(censdf4_5, GEOID %in% c(37063000301, 37063000302, 37063000401, 37063000402, 37063000500, 37063000600, 37063000700, 37063000900, 
                                            37063001001, 37063001002, 37063001100, 37063001301, 37063001303, 37063001304, 37063001400, 37063001501, 
                                            37063001502, 37063001503, 37063001601, 37063001603, 37063001604, 37063001705, 37063001706, 37063001707, 
                                            37063001708, 37063001709, 37063001710, 37063001711, 37063001801, 37063001802, 37063001806, 37063001807, 
                                            37063001808, 37063001809, 37063001900, 37063002007, 37063002008, 37063002009, 37063002013, 37063002015, 
                                            37063002016, 37063002017, 37063002018, 37063002019, 37063002020, 37063002021, 37063002022, 37063002023, 
                                            37063002024, 37063002025, 37063002026, 37063002027, 37063002028, 37063002100, 37063002200, 37063002300))

# mean commute time - 30 to 34 minutes

res4_6 = GET("https://api.census.gov/data/2019/acs/acs5?get=NAME,B08134_001E,B08134_007E&for=tract:*&in=state:37&in=county:063")
rawToChar(res4_6$content)
censdf4_6 = fromJSON(rawToChar(res4_6$content))
res4_6

colnames(censdf4_6) <- censdf4_6[1, ]
censdf4_6 <- censdf4_6[-c(1), ]
censdf4_6 <- as.data.frame(censdf4_6)
names(censdf4_6)[names(censdf4_6) == "B08134_001E"] <- "commute_pop"
names(censdf4_6)[names(censdf4_6) == "B08134_007E"] <- "30to34"


censdf4_6[, c(2:6)] <- sapply(censdf4_6[, c(2:6)], as.numeric)

censdf4_6$pct30to34 <- (100 * censdf4_6$"30to34") / (censdf4_6$commute_pop)
censdf4_6$GEOID <- ((censdf4_6$state) * 1000000000 + (censdf4_6$county) * 1000000 + censdf4_6$tract)


censdf4_6 <- subset(censdf4_6, GEOID %in% c(37063000301, 37063000302, 37063000401, 37063000402, 37063000500, 37063000600, 37063000700, 37063000900, 
                                            37063001001, 37063001002, 37063001100, 37063001301, 37063001303, 37063001304, 37063001400, 37063001501, 
                                            37063001502, 37063001503, 37063001601, 37063001603, 37063001604, 37063001705, 37063001706, 37063001707, 
                                            37063001708, 37063001709, 37063001710, 37063001711, 37063001801, 37063001802, 37063001806, 37063001807, 
                                            37063001808, 37063001809, 37063001900, 37063002007, 37063002008, 37063002009, 37063002013, 37063002015, 
                                            37063002016, 37063002017, 37063002018, 37063002019, 37063002020, 37063002021, 37063002022, 37063002023, 
                                            37063002024, 37063002025, 37063002026, 37063002027, 37063002028, 37063002100, 37063002200, 37063002300))


# mean commute time - 35 to 44 minutes

res4_7 = GET("https://api.census.gov/data/2019/acs/acs5?get=NAME,B08134_001E,B08134_008E&for=tract:*&in=state:37&in=county:063")
rawToChar(res4_7$content)
censdf4_7 = fromJSON(rawToChar(res4_7$content))
res4_7

colnames(censdf4_7) <- censdf4_7[1, ]
censdf4_7 <- censdf4_7[-c(1), ]
censdf4_7 <- as.data.frame(censdf4_7)
names(censdf4_7)[names(censdf4_7) == "B08134_001E"] <- "commute_pop"
names(censdf4_7)[names(censdf4_7) == "B08134_008E"] <- "35to44"


censdf4_7[, c(2:6)] <- sapply(censdf4_7[, c(2:6)], as.numeric)

censdf4_7$pct35to44 <- (100 * censdf4_7$"35to44") / (censdf4_7$commute_pop)
censdf4_7$GEOID <- ((censdf4_7$state) * 1000000000 + (censdf4_7$county) * 1000000 + censdf4_7$tract)


censdf4_7 <- subset(censdf4_7, GEOID %in% c(37063000301, 37063000302, 37063000401, 37063000402, 37063000500, 37063000600, 37063000700, 37063000900, 
                                            37063001001, 37063001002, 37063001100, 37063001301, 37063001303, 37063001304, 37063001400, 37063001501, 
                                            37063001502, 37063001503, 37063001601, 37063001603, 37063001604, 37063001705, 37063001706, 37063001707, 
                                            37063001708, 37063001709, 37063001710, 37063001711, 37063001801, 37063001802, 37063001806, 37063001807, 
                                            37063001808, 37063001809, 37063001900, 37063002007, 37063002008, 37063002009, 37063002013, 37063002015, 
                                            37063002016, 37063002017, 37063002018, 37063002019, 37063002020, 37063002021, 37063002022, 37063002023, 
                                            37063002024, 37063002025, 37063002026, 37063002027, 37063002028, 37063002100, 37063002200, 37063002300))

# mean commute time - 45 to 59 minutes

res4_8 = GET("https://api.census.gov/data/2019/acs/acs5?get=NAME,B08134_001E,B08134_009E&for=tract:*&in=state:37&in=county:063")
rawToChar(res4_8$content)
censdf4_8 = fromJSON(rawToChar(res4_8$content))
res4_8

colnames(censdf4_8) <- censdf4_8[1, ]
censdf4_8 <- censdf4_8[-c(1), ]
censdf4_8 <- as.data.frame(censdf4_8)
names(censdf4_8)[names(censdf4_8) == "B08134_001E"] <- "commute_pop"
names(censdf4_8)[names(censdf4_8) == "B08134_009E"] <- "45to59"


censdf4_8[, c(2:6)] <- sapply(censdf4_8[, c(2:6)], as.numeric)

censdf4_8$pct35to39 <- (100 * censdf4_8$"45to59") / (censdf4_8$commute_pop)
censdf4_8$GEOID <- ((censdf4_8$state) * 1000000000 + (censdf4_8$county) * 1000000 + censdf4_8$tract)


censdf4_8 <- subset(censdf4_8, GEOID %in% c(37063000301, 37063000302, 37063000401, 37063000402, 37063000500, 37063000600, 37063000700, 37063000900, 
                                            37063001001, 37063001002, 37063001100, 37063001301, 37063001303, 37063001304, 37063001400, 37063001501, 
                                            37063001502, 37063001503, 37063001601, 37063001603, 37063001604, 37063001705, 37063001706, 37063001707, 
                                            37063001708, 37063001709, 37063001710, 37063001711, 37063001801, 37063001802, 37063001806, 37063001807, 
                                            37063001808, 37063001809, 37063001900, 37063002007, 37063002008, 37063002009, 37063002013, 37063002015, 
                                            37063002016, 37063002017, 37063002018, 37063002019, 37063002020, 37063002021, 37063002022, 37063002023, 
                                            37063002024, 37063002025, 37063002026, 37063002027, 37063002028, 37063002100, 37063002200, 37063002300))

# mean commute time - 60 or more minutes

res4_9 = GET("https://api.census.gov/data/2019/acs/acs5?get=NAME,B08134_001E,B08134_010E&for=tract:*&in=state:37&in=county:063")
rawToChar(res4_9$content)
censdf4_9 = fromJSON(rawToChar(res4_9$content))
res4_9

colnames(censdf4_9) <- censdf4_9[1, ]
censdf4_9 <- censdf4_9[-c(1), ]
censdf4_9 <- as.data.frame(censdf4_9)
names(censdf4_9)[names(censdf4_9) == "B08134_001E"] <- "commute_pop"
names(censdf4_9)[names(censdf4_9) == "B08134_010E"] <- "60ormore"


censdf4_9[, c(2:6)] <- sapply(censdf4_9[, c(2:6)], as.numeric)

censdf4_9$pct60ormore <- (100 * censdf4_9$"60ormore") / (censdf4_9$commute_pop)
censdf4_9$GEOID <- ((censdf4_9$state) * 1000000000 + (censdf4_9$county) * 1000000 + censdf4_9$tract)


censdf4_9 <- subset(censdf4_9, GEOID %in% c(37063000301, 37063000302, 37063000401, 37063000402, 37063000500, 37063000600, 37063000700, 37063000900, 
                                            37063001001, 37063001002, 37063001100, 37063001301, 37063001303, 37063001304, 37063001400, 37063001501, 
                                            37063001502, 37063001503, 37063001601, 37063001603, 37063001604, 37063001705, 37063001706, 37063001707, 
                                            37063001708, 37063001709, 37063001710, 37063001711, 37063001801, 37063001802, 37063001806, 37063001807, 
                                            37063001808, 37063001809, 37063001900, 37063002007, 37063002008, 37063002009, 37063002013, 37063002015, 
                                            37063002016, 37063002017, 37063002018, 37063002019, 37063002020, 37063002021, 37063002022, 37063002023, 
                                            37063002024, 37063002025, 37063002026, 37063002027, 37063002028, 37063002100, 37063002200, 37063002300))

# percentage below poverty line
# B06012_001E is popest, B06012_002E is belowpoverty
res5 = GET("https://api.census.gov/data/2019/acs/acs5?get=NAME,B06012_001E,B06012_002E&for=tract:*&in=state:37&in=county:063")
rawToChar(res5$content)
censdf5 = fromJSON(rawToChar(res5$content))
res5

colnames(censdf5) <- censdf5[1, ]
censdf5 <- censdf5[-c(1), ]
censdf5 <- as.data.frame(censdf5)
names(censdf5)[names(censdf5) == "B06012_001E"] <- "totalpop_poverty"
names(censdf5)[names(censdf5) == "B06012_002E"] <- "belowpoverty"

censdf5[, c(2:6)] <- sapply(censdf5[, c(2:6)], as.numeric)

censdf5$pctbelowpoverty <- (100 * censdf5$belowpoverty) / (censdf5$totalpop_poverty)
censdf5$GEOID <- ((censdf5$state) * 1000000000 + (censdf5$county) * 1000000 + censdf5$tract)

censdf5 <- subset(censdf5, GEOID %in% c(37063000301, 37063000302, 37063000401, 37063000402, 37063000500, 37063000600, 37063000700, 37063000900, 
                                        37063001001, 37063001002, 37063001100, 37063001301, 37063001303, 37063001304, 37063001400, 37063001501, 
                                        37063001502, 37063001503, 37063001601, 37063001603, 37063001604, 37063001705, 37063001706, 37063001707, 
                                        37063001708, 37063001709, 37063001710, 37063001711, 37063001801, 37063001802, 37063001806, 37063001807, 
                                        37063001808, 37063001809, 37063001900, 37063002007, 37063002008, 37063002009, 37063002013, 37063002015, 
                                        37063002016, 37063002017, 37063002018, 37063002019, 37063002020, 37063002021, 37063002022, 37063002023, 
                                        37063002024, 37063002025, 37063002026, 37063002027, 37063002028, 37063002100, 37063002200, 37063002300))

# population density
# B06012_001E is popest, B06012_002E is belowpoverty
censdf6_1 = read.csv('AreaLand.csv')

names(censdf6)[names(censdf6) == "B06012_001E"] <- "totalpop_poverty"
names(censdf6)[names(censdf6) == "B06012_002E"] <- "belowpoverty"

censdf6_1[, c(1:4)] <- sapply(censdf6_1[, c(1:4)], as.numeric)

censdf6$GEOID <- ((censdf6$state) * 1000000000 + (censdf6$county) * 1000000 + censdf6$tract)

censdf6_1 <- subset(censdf6_1, GEOID %in% c(37063000301, 37063000302, 37063000401, 37063000402, 37063000500, 37063000600, 37063000700, 37063000900, 
                                        37063001001, 37063001002, 37063001100, 37063001301, 37063001303, 37063001304, 37063001400, 37063001501, 
                                        37063001502, 37063001503, 37063001601, 37063001603, 37063001604, 37063001705, 37063001706, 37063001707, 
                                        37063001708, 37063001709, 37063001710, 37063001711, 37063001801, 37063001802, 37063001806, 37063001807, 
                                        37063001808, 37063001809, 37063001900, 37063002007, 37063002008, 37063002009, 37063002013, 37063002015, 
                                        37063002016, 37063002017, 37063002018, 37063002019, 37063002020, 37063002021, 37063002022, 37063002023, 
                                        37063002024, 37063002025, 37063002026, 37063002027, 37063002028, 37063002100, 37063002200, 37063002300))

censdf6_1$landareasqmile <- (censdf6_1$AREALAND) / (2589988)

res6 = GET("https://api.census.gov/data/2019/acs/acs5?get=NAME,B01003_001E&for=tract:*&in=state:37&in=county:063")
rawToChar(res6$content)
censdf6 = fromJSON(rawToChar(res6$content))
res6

colnames(censdf6) <- censdf6[1, ]
censdf6 <- censdf6[-c(1), ]
censdf6 <- as.data.frame(censdf6)
names(censdf6)[names(censdf6) == "B01003_001E"] <- "totalpop_fordensity"

censdf6[, c(2:5)] <- sapply(censdf6[, c(2:5)], as.numeric)

censdf6$GEOID <- ((censdf6$state) * 1000000000 + (censdf6$county) * 1000000 + censdf6$tract)

censdf6 <- subset(censdf6, GEOID %in% c(37063000301, 37063000302, 37063000401, 37063000402, 37063000500, 37063000600, 37063000700, 37063000900, 
                                        37063001001, 37063001002, 37063001100, 37063001301, 37063001303, 37063001304, 37063001400, 37063001501, 
                                        37063001502, 37063001503, 37063001601, 37063001603, 37063001604, 37063001705, 37063001706, 37063001707, 
                                        37063001708, 37063001709, 37063001710, 37063001711, 37063001801, 37063001802, 37063001806, 37063001807, 
                                        37063001808, 37063001809, 37063001900, 37063002007, 37063002008, 37063002009, 37063002013, 37063002015, 
                                        37063002016, 37063002017, 37063002018, 37063002019, 37063002020, 37063002021, 37063002022, 37063002023, 
                                        37063002024, 37063002025, 37063002026, 37063002027, 37063002028, 37063002100, 37063002200, 37063002300))

censdf6 <- merge(censdf6, censdf6_1, by = "GEOID")
censdf6$popdensity <- (censdf6$totalpop_fordensity) / (censdf6$landareasqmile)
censdf6$state <- NULL
censdf6$county <- NULL
censdf6$tract <- NULL
censdf6$NAME <- NULL

# median gross rent for 2-bed
# B25031_004E is median gross rent for 2-bed
res7 = GET("https://api.census.gov/data/2019/acs/acs5?get=NAME,B25031_004E&for=tract:*&in=state:37&in=county:063")
rawToChar(res7$content)
censdf7 = fromJSON(rawToChar(res7$content))
res7

colnames(censdf7) <- censdf7[1, ]
censdf7 <- censdf7[-c(1), ]
censdf7 <- as.data.frame(censdf7)
names(censdf7)[names(censdf7) == "B25031_004E"] <- "mean_gross_rent_2bed"

censdf7[, c(2:5)] <- sapply(censdf7[, c(2:5)], as.numeric)

censdf7$GEOID <- ((censdf7$state) * 1000000000 + (censdf7$county) * 1000000 + censdf7$tract)

censdf7 <- subset(censdf7, GEOID %in% c(37063000301, 37063000302, 37063000401, 37063000402, 37063000500, 37063000600, 37063000700, 37063000900, 
                                        37063001001, 37063001002, 37063001100, 37063001301, 37063001303, 37063001304, 37063001400, 37063001501, 
                                        37063001502, 37063001503, 37063001601, 37063001603, 37063001604, 37063001705, 37063001706, 37063001707, 
                                        37063001708, 37063001709, 37063001710, 37063001711, 37063001801, 37063001802, 37063001806, 37063001807, 
                                        37063001808, 37063001809, 37063001900, 37063002007, 37063002008, 37063002009, 37063002013, 37063002015, 
                                        37063002016, 37063002017, 37063002018, 37063002019, 37063002020, 37063002021, 37063002022, 37063002023, 
                                        37063002024, 37063002025, 37063002026, 37063002027, 37063002028, 37063002100, 37063002200, 37063002300))

censdf7 <- censdf7 %>% mutate(mean_gross_rent_2bed = na_if(mean_gross_rent_2bed, (-666666666)))

# percent single parent household
# B09019_005E, B09019_008E, B09019_001E
res8_m = GET("https://api.census.gov/data/2019/acs/acs5?get=NAME,B09019_001E,B09019_005E&for=tract:*&in=state:37&in=county:063")
rawToChar(res8_m$content)
censdf8_m = fromJSON(rawToChar(res8_m$content))
res8_m

colnames(censdf8_m) <- censdf8_m[1, ]
censdf8_m <- censdf8_m[-c(1), ]
censdf8_m <- as.data.frame(censdf8_m)
names(censdf8_m)[names(censdf8_m) == "B09019_001E"] <- "total_pop_parent"
names(censdf8_m)[names(censdf8_m) == "B09019_005E"] <- "male_alone"


censdf8_m[, c(2:6)] <- sapply(censdf8_m[, c(2:6)], as.numeric)

censdf8_m$GEOID <- ((censdf8_m$state) * 1000000000 + (censdf8_m$county) * 1000000 + censdf8_m$tract)

censdf8_m <- subset(censdf8_m, GEOID %in% c(37063000301, 37063000302, 37063000401, 37063000402, 37063000500, 37063000600, 37063000700, 37063000900, 
                                        37063001001, 37063001002, 37063001100, 37063001301, 37063001303, 37063001304, 37063001400, 37063001501, 
                                        37063001502, 37063001503, 37063001601, 37063001603, 37063001604, 37063001705, 37063001706, 37063001707, 
                                        37063001708, 37063001709, 37063001710, 37063001711, 37063001801, 37063001802, 37063001806, 37063001807, 
                                        37063001808, 37063001809, 37063001900, 37063002007, 37063002008, 37063002009, 37063002013, 37063002015, 
                                        37063002016, 37063002017, 37063002018, 37063002019, 37063002020, 37063002021, 37063002022, 37063002023, 
                                        37063002024, 37063002025, 37063002026, 37063002027, 37063002028, 37063002100, 37063002200, 37063002300))

censdf8_m$percent_male_alone <- (censdf8_m$male_alone) / (censdf8_m$total_pop_parent)

res8_f = GET("https://api.census.gov/data/2019/acs/acs5?get=NAME,B09019_001E,B09019_008E&for=tract:*&in=state:37&in=county:063")
rawToChar(res8_f$content)
censdf8_f = fromJSON(rawToChar(res8_f$content))
res8_f

colnames(censdf8_f) <- censdf8_f[1, ]
censdf8_f <- censdf8_f[-c(1), ]
censdf8_f <- as.data.frame(censdf8_f)
names(censdf8_f)[names(censdf8_f) == "B09019_001E"] <- "total_pop_parent_f"
names(censdf8_f)[names(censdf8_f) == "B09019_008E"] <- "female_alone"

censdf8_f[, c(2:6)] <- sapply(censdf8_f[, c(2:6)], as.numeric)

censdf8_f$GEOID <- ((censdf8_f$state) * 1000000000 + (censdf8_f$county) * 1000000 + censdf8_f$tract)

censdf8_f <- subset(censdf8_f, GEOID %in% c(37063000301, 37063000302, 37063000401, 37063000402, 37063000500, 37063000600, 37063000700, 37063000900, 
                                            37063001001, 37063001002, 37063001100, 37063001301, 37063001303, 37063001304, 37063001400, 37063001501, 
                                            37063001502, 37063001503, 37063001601, 37063001603, 37063001604, 37063001705, 37063001706, 37063001707, 
                                            37063001708, 37063001709, 37063001710, 37063001711, 37063001801, 37063001802, 37063001806, 37063001807, 
                                            37063001808, 37063001809, 37063001900, 37063002007, 37063002008, 37063002009, 37063002013, 37063002015, 
                                            37063002016, 37063002017, 37063002018, 37063002019, 37063002020, 37063002021, 37063002022, 37063002023, 
                                            37063002024, 37063002025, 37063002026, 37063002027, 37063002028, 37063002100, 37063002200, 37063002300))

censdf8_f$percent_female_alone <- (censdf8_f$female_alone) / (censdf8_f$total_pop_parent)

censdf8 <- merge(censdf7_f, censdf7_m, by = "GEOID")

censdf8$total_pop_parent_f <- NULL
censdf8$NAME.x <- NULL
censdf8$state.x <- NULL
censdf8$county.x <- NULL
censdf8$tract.x <- NULL

censdf8$percent_parent_alone <- censdf8$percent_female_alone + censdf8$percent_male_alone

# race

res9_1 = GET("https://api.census.gov/data/2014/acs/acs5?get=NAME,B02001_001E,B02001_002E&for=tract:*&in=state:37&in=county:063")
rawToChar(res9_1$content)
censdf9_1 = fromJSON(rawToChar(res9_1$content))
res9_1

colnames(censdf9_1) <- censdf9_1[1, ]
censdf9_1 <- censdf9_1[-c(1), ]
censdf9_1 <- as.data.frame(censdf9_1)
names(censdf9_1)[names(censdf9_1) == "B02001_001E"] <- "total_race"
names(censdf9_1)[names(censdf9_1) == "B02001_002E"] <- "total_white"


censdf9_1[, c(2:6)] <- sapply(censdf9_1[, c(2:6)], as.numeric)

censdf9_1$GEOID <- ((censdf9_1$state) * 1000000000 + (censdf9_1$county) * 1000000 + censdf9_1$tract)

censdf9_1 <- subset(censdf9_1, GEOID %in% c(37063000301, 37063000302, 37063000401, 37063000402, 37063000500, 37063000600, 37063000700, 37063000900, 
                                            37063001001, 37063001002, 37063001100, 37063001301, 37063001303, 37063001304, 37063001400, 37063001501, 
                                            37063001502, 37063001503, 37063001601, 37063001603, 37063001604, 37063001705, 37063001706, 37063001707, 
                                            37063001708, 37063001709, 37063001710, 37063001711, 37063001801, 37063001802, 37063001806, 37063001807, 
                                            37063001808, 37063001809, 37063001900, 37063002007, 37063002008, 37063002009, 37063002013, 37063002015, 
                                            37063002016, 37063002017, 37063002018, 37063002019, 37063002020, 37063002021, 37063002022, 37063002023, 
                                            37063002024, 37063002025, 37063002026, 37063002027, 37063002028, 37063002100, 37063002200, 37063002300))

res9_2 = GET("https://api.census.gov/data/2014/acs/acs5?get=NAME,B02001_003E&for=tract:*&in=state:37&in=county:063")
rawToChar(res9_2$content)
censdf9_2 = fromJSON(rawToChar(res9_2$content))
res9_2

colnames(censdf9_2) <- censdf9_2[1, ]
censdf9_2 <- censdf9_2[-c(1), ]
censdf9_2 <- as.data.frame(censdf9_2)
names(censdf9_2)[names(censdf9_2) == "B02001_003E"] <- "total_black_or_africanamerican"


censdf9_2[, c(2:5)] <- sapply(censdf9_2[, c(2:5)], as.numeric)

censdf9_2$GEOID <- ((censdf9_2$state) * 1000000000 + (censdf9_2$county) * 1000000 + censdf9_2$tract)

censdf9 <- merge(censdf9_1, censdf9_2, by = "GEOID")
censdf9$NAME.x <- NULL
censdf9$state.x <- NULL
censdf9$county.x <- NULL
censdf9$tract.x <- NULL

res9_3 = GET("https://api.census.gov/data/2014/acs/acs5?get=NAME,B02001_004E&for=tract:*&in=state:37&in=county:063")
rawToChar(res9_3$content)
censdf9_3 = fromJSON(rawToChar(res9_3$content))
res9_3

colnames(censdf9_3) <- censdf9_3[1, ]
censdf9_3 <- censdf9_3[-c(1), ]
censdf9_3 <- as.data.frame(censdf9_3)
names(censdf9_3)[names(censdf9_3) == "B02001_004E"] <- "total_americanindian_and_alaskanative"


censdf9_3[, c(2:5)] <- sapply(censdf9_3[, c(2:5)], as.numeric)

censdf9_3$GEOID <- ((censdf9_3$state) * 1000000000 + (censdf9_3$county) * 1000000 + censdf9_3$tract)

censdf9 <- merge(censdf9, censdf9_3, by = "GEOID")
censdf9$NAME.x <- NULL
censdf9$state.y <- NULL
censdf9$county.y <- NULL
censdf9$tract.y <- NULL

res9_4 = GET("https://api.census.gov/data/2014/acs/acs5?get=NAME,B02001_005E&for=tract:*&in=state:37&in=county:063")
rawToChar(res9_4$content)
censdf9_4 = fromJSON(rawToChar(res9_4$content))
res9_4

colnames(censdf9_4) <- censdf9_4[1, ]
censdf9_4 <- censdf9_4[-c(1), ]
censdf9_4 <- as.data.frame(censdf9_4)
names(censdf9_4)[names(censdf9_4) == "B02001_005E"] <- "total_asian"


censdf9_4[, c(2:5)] <- sapply(censdf9_4[, c(2:5)], as.numeric)

censdf9_4$GEOID <- ((censdf9_4$state) * 1000000000 + (censdf9_4$county) * 1000000 + censdf9_4$tract)

censdf9 <- merge(censdf9, censdf9_4, by = "GEOID")
censdf9$NAME.y <- NULL
censdf9$state.y <- NULL
censdf9$county.y <- NULL
censdf9$tract.y <- NULL

res9_5 = GET("https://api.census.gov/data/2014/acs/acs5?get=NAME,B02001_006E&for=tract:*&in=state:37&in=county:063")
rawToChar(res9_5$content)
censdf9_5 = fromJSON(rawToChar(res9_5$content))
res9_5

colnames(censdf9_5) <- censdf9_5[1, ]
censdf9_5 <- censdf9_5[-c(1), ]
censdf9_5 <- as.data.frame(censdf9_5)
names(censdf9_5)[names(censdf9_5) == "B02001_006E"] <- "total_nativehawaiian_or_pacificislander"


censdf9_5[, c(2:5)] <- sapply(censdf9_5[, c(2:5)], as.numeric)

censdf9_5$GEOID <- ((censdf9_5$state) * 1000000000 + (censdf9_5$county) * 1000000 + censdf9_5$tract)

censdf9 <- merge(censdf9, censdf9_5, by = "GEOID")
censdf9$NAME.x <- NULL
censdf9$state.x <- NULL
censdf9$county.x <- NULL
censdf9$tract.x <- NULL

res9_6 = GET("https://api.census.gov/data/2014/acs/acs5?get=NAME,B02001_007E&for=tract:*&in=state:37&in=county:063")
rawToChar(res9_6$content)
censdf9_6 = fromJSON(rawToChar(res9_6$content))
res9_6

colnames(censdf9_6) <- censdf9_6[1, ]
censdf9_6 <- censdf9_6[-c(1), ]
censdf9_6 <- as.data.frame(censdf9_6)
names(censdf9_6)[names(censdf9_6) == "B02001_007E"] <- "someotherracealone"


censdf9_6[, c(2:5)] <- sapply(censdf9_6[, c(2:5)], as.numeric)

censdf9_6$GEOID <- ((censdf9_6$state) * 1000000000 + (censdf9_6$county) * 1000000 + censdf9_6$tract)

censdf9 <- merge(censdf9, censdf9_6, by = "GEOID")
censdf9$NAME.x <- NULL
censdf9$state.x <- NULL
censdf9$county.x <- NULL
censdf9$tract.x <- NULL

res9_7 = GET("https://api.census.gov/data/2014/acs/acs5?get=NAME,B02001_008E&for=tract:*&in=state:37&in=county:063")
rawToChar(res9_7$content)
censdf9_7 = fromJSON(rawToChar(res9_7$content))
res9_7

colnames(censdf9_7) <- censdf9_7[1, ]
censdf9_7 <- censdf9_7[-c(1), ]
censdf9_7 <- as.data.frame(censdf9_7)
names(censdf9_7)[names(censdf9_7) == "B02001_008E"] <- "twoormoreraces"


censdf9_7[, c(2:5)] <- sapply(censdf9_7[, c(2:5)], as.numeric)

censdf9_7$GEOID <- ((censdf9_7$state) * 1000000000 + (censdf9_7$county) * 1000000 + censdf9_7$tract)

censdf9 <- merge(censdf9, censdf9_7, by = "GEOID")
censdf9$NAME.y <- NULL
censdf9$state.y <- NULL
censdf9$county.y <- NULL
censdf9$tract.y <- NULL

res9_8 = GET("https://api.census.gov/data/2014/acs/acs5?get=NAME,B02001_009E&for=tract:*&in=state:37&in=county:063")
rawToChar(res9_8$content)
censdf9_8 = fromJSON(rawToChar(res9_8$content))
res9_8

colnames(censdf9_8) <- censdf9_8[1, ]
censdf9_8 <- censdf9_8[-c(1), ]
censdf9_8 <- as.data.frame(censdf9_8)
names(censdf9_8)[names(censdf9_8) == "B02001_009E"] <- "two+orother"


censdf9_8[, c(2:5)] <- sapply(censdf9_8[, c(2:5)], as.numeric)

censdf9_8$GEOID <- ((censdf9_8$state) * 1000000000 + (censdf9_8$county) * 1000000 + censdf9_8$tract)

censdf9 <- merge(censdf9, censdf9_8, by = "GEOID")
censdf9$NAME.y <- NULL
censdf9$state.y <- NULL
censdf9$county.y <- NULL
censdf9$tract.y <- NULL

res9_9 = GET("https://api.census.gov/data/2014/acs/acs5?get=NAME,B02001_010E&for=tract:*&in=state:37&in=county:063")
rawToChar(res9_9$content)
censdf9_9 = fromJSON(rawToChar(res9_9$content))
res9_9

colnames(censdf9_9) <- censdf9_9[1, ]
censdf9_9 <- censdf9_9[-c(1), ]
censdf9_9 <- as.data.frame(censdf9_9)
names(censdf9_9)[names(censdf9_9) == "B02001_010E"] <- "two+orotherorthree+"


censdf9_9[, c(2:5)] <- sapply(censdf9_9[, c(2:5)], as.numeric)

censdf9_9$GEOID <- ((censdf9_9$state) * 1000000000 + (censdf9_9$county) * 1000000 + censdf9_9$tract)

censdf9 <- merge(censdf9, censdf9_9, by = "GEOID")
censdf9$NAME.x <- NULL
censdf9$state.x <- NULL
censdf9$county.x <- NULL
censdf9$tract.x <- NULL

censdf9$other <- censdf9$someotherracealone + censdf9$twoormoreraces + censdf9$"two+orother" + censdf9$"two+orotherorthree+"

censdf9$someotherracealone <- NULL
censdf9$twoormoreraces <- NULL
censdf9$"two+orother" <- NULL
censdf9$"two+orotherorthree+" <- NULL

censdf9 <- subset(censdf9, GEOID %in% c(37063000301, 37063000302, 37063000401, 37063000402, 37063000500, 37063000600, 37063000700, 37063000900, 
                                            37063001001, 37063001002, 37063001100, 37063001301, 37063001303, 37063001304, 37063001400, 37063001501, 
                                            37063001502, 37063001503, 37063001601, 37063001603, 37063001604, 37063001705, 37063001706, 37063001707, 
                                            37063001708, 37063001709, 37063001710, 37063001711, 37063001801, 37063001802, 37063001806, 37063001807, 
                                            37063001808, 37063001809, 37063001900, 37063002007, 37063002008, 37063002009, 37063002013, 37063002015, 
                                            37063002016, 37063002017, 37063002018, 37063002019, 37063002020, 37063002021, 37063002022, 37063002023, 
                                            37063002024, 37063002025, 37063002026, 37063002027, 37063002028, 37063002100, 37063002200, 37063002300))

names(censdf9)[names(censdf9) == "other"] <- "other_race"

censdf9$percent_white <- (censdf9$total_white) / (censdf9$total_race)
censdf9$percent_blackorafricanamerican <- (censdf9$total_black_or_africanamerican) / (censdf9$total_race)
censdf9$percent_americanindianoralaskanative <- (censdf9$total_americanindian_and_alaskanative) / (censdf9$total_race)
censdf9$percent_asian <- (censdf9$total_asian) / (censdf9$total_race)
censdf9$percent_nativehawaiianorpacificislander <- (censdf9$total_nativehawaiian_or_pacificislander) / (censdf9$total_race)
censdf9$percent_other_or_2plus <- (censdf9$other_race) / (censdf9$total_race)


# wages increase

res10_2014 = GET("https://api.census.gov/data/2014/acs/acs5?get=NAME,B20004_003E&for=tract:*&in=state:37&in=county:063")
rawToChar(res10_2014$content)
censdf10_2014 = fromJSON(rawToChar(res10_2014$content))
res10_2014

colnames(censdf10_2014) <- censdf10_2014[1, ]
censdf10_2014 <- censdf10_2014[-c(1), ]
censdf10_2014 <- as.data.frame(censdf10_2014)
names(censdf10_2014)[names(censdf10_2014) == "B20004_003E"] <- "hswage_2014"

censdf10_2014[, c(2:5)] <- sapply(censdf10_2014[, c(2:5)], as.numeric)

censdf10_2014$GEOID <- ((censdf10_2014$state) * 1000000000 + (censdf10_2014$county) * 1000000 + censdf10_2014$tract)

censdf10_2014 <- subset(censdf10_2014, GEOID %in% c(37063000301, 37063000302, 37063000401, 37063000402, 37063000500, 37063000600, 37063000700, 37063000900, 
                                        37063001001, 37063001002, 37063001100, 37063001301, 37063001303, 37063001304, 37063001400, 37063001501, 
                                        37063001502, 37063001503, 37063001601, 37063001603, 37063001604, 37063001705, 37063001706, 37063001707, 
                                        37063001708, 37063001709, 37063001710, 37063001711, 37063001801, 37063001802, 37063001806, 37063001807, 
                                        37063001808, 37063001809, 37063001900, 37063002007, 37063002008, 37063002009, 37063002013, 37063002015, 
                                        37063002016, 37063002017, 37063002018, 37063002019, 37063002020, 37063002021, 37063002022, 37063002023, 
                                        37063002024, 37063002025, 37063002026, 37063002027, 37063002028, 37063002100, 37063002200, 37063002300))


res10_2019 = GET("https://api.census.gov/data/2019/acs/acs5?get=NAME,B20004_003E&for=tract:*&in=state:37&in=county:063")
rawToChar(res10_2019$content)
censdf10_2019 = fromJSON(rawToChar(res10_2019$content))
res10_2019

colnames(censdf10_2019) <- censdf10_2019[1, ]
censdf10_2019 <- censdf10_2019[-c(1), ]
censdf10_2019 <- as.data.frame(censdf10_2019)
names(censdf10_2019)[names(censdf10_2019) == "B20004_003E"] <- "hswage_2019"

censdf10_2019[, c(2:5)] <- sapply(censdf10_2019[, c(2:5)], as.numeric)

censdf10_2019$GEOID <- ((censdf10_2019$state) * 1000000000 + (censdf10_2019$county) * 1000000 + censdf10_2019$tract)

censdf10_2019 <- subset(censdf10_2019, GEOID %in% c(37063000301, 37063000302, 37063000401, 37063000402, 37063000500, 37063000600, 37063000700, 37063000900, 
                                                    37063001001, 37063001002, 37063001100, 37063001301, 37063001303, 37063001304, 37063001400, 37063001501, 
                                                    37063001502, 37063001503, 37063001601, 37063001603, 37063001604, 37063001705, 37063001706, 37063001707, 
                                                    37063001708, 37063001709, 37063001710, 37063001711, 37063001801, 37063001802, 37063001806, 37063001807, 
                                                    37063001808, 37063001809, 37063001900, 37063002007, 37063002008, 37063002009, 37063002013, 37063002015, 
                                                    37063002016, 37063002017, 37063002018, 37063002019, 37063002020, 37063002021, 37063002022, 37063002023, 
                                                    37063002024, 37063002025, 37063002026, 37063002027, 37063002028, 37063002100, 37063002200, 37063002300))
censdf10 <- merge(censdf10_2014, censdf10_2019, by = "GEOID")

censdf10$state.y <- NULL
censdf10$county.y <- NULL
censdf10$tract.y <- NULL
censdf10$NAME.y <- NULL

censdf10 <- censdf10 %>% mutate(hswage_2014 = na_if(hswage_2014, (-666666666)))
censdf10 <- censdf10 %>% mutate(hswage_2019 = na_if(hswage_2019, (-666666666)))

censdf10$wagegrowth <- log(censdf10$hswage_2019) - log(censdf10$hswage_2014)
  
# merging datasets
hcvchar <- merge(hcv, censdf1, by = "GEOID", all = TRUE)
hcvchar <- merge(hcvchar, censdf2, by = "GEOID", all = TRUE)
hcvchar <- merge(hcvchar, censdf3, by = "GEOID", all = TRUE)
hcvchar <- merge(hcvchar, censdf4_1, by = "GEOID", all = TRUE)
hcvchar <- merge(hcvchar, censdf4_2, by = "GEOID", all = TRUE)
hcvchar <- merge(hcvchar, censdf4_3, by = "GEOID", all = TRUE)
hcvchar <- merge(hcvchar, censdf4_4, by = "GEOID", all = TRUE)
hcvchar <- merge(hcvchar, censdf4_5, by = "GEOID", all = TRUE)
hcvchar <- merge(hcvchar, censdf4_6, by = "GEOID", all = TRUE)
hcvchar <- merge(hcvchar, censdf4_7, by = "GEOID", all = TRUE)
hcvchar <- merge(hcvchar, censdf4_8, by = "GEOID", all = TRUE)
hcvchar <- merge(hcvchar, censdf4_9, by = "GEOID", all = TRUE)
hcvchar <- merge(hcvchar, censdf5, by = "GEOID", all = TRUE)
hcvchar <- merge(hcvchar, censdf6, by = "GEOID", all = TRUE)
hcvchar <- merge(hcvchar, censdf7, by = "GEOID", all = TRUE)
hcvchar <- merge(hcvchar, censdf8, by = "GEOID", all = TRUE)
hcvchar <- merge(hcvchar, censdf9, by = "GEOID", all = TRUE)
hcvchar <- merge(hcvchar, censdf10, by = "GEOID", all = TRUE)


hcvchar$NAME.x <- NULL
hcvchar$NAME.y.x <- NULL
hcvchar$state.x <- NULL
hcvchar$state.y.x <- NULL
hcvchar$county.x <- NULL
hcvchar$county.y.x <- NULL
hcvchar$tract.x <- NULL
hcvchar$tract.y.x <- NULL
hcvchar$commute_pop.x <- NULL
hcvchar$mean_gross_rent_2bed.x <- NULL

hcvchar$female_alone.y <- NULL
hcvchar$percent_female_alone.y <- NULL
hcvchar$total_pop_parent.y <- NULL
hcvchar$male_alone.y <- NULL
hcvchar$state.y.x <- NULL
hcvchar$county.y.x <- NULL
hcvchar$tract.y.x <- NULL
hcvchar$percent_male_alone.y <- NULL
hcvchar$percent_parent_alone.y <- NULL
hcvchar$state.y.y <- NULL
hcvchar$county.y.y <- NULL
hcvchar$tract.y.y <- NULL

hcvchar$STATE <- NULL
hcvchar$COUNTY <- NULL

hcvchar$pct35to39.x <- NULL



names(hcvchar)[names(hcvchar) == "pct35to39"] <- "pct45to59"


# summary statistics
mean(hcvchar$hhinc_med) #33967.68
median(hcvchar$hhinc_med) #33245
mean(hcvchar$pctcollplus) #49.49096
mean(hcvchar$population) #4342.518
mean(hcvchar$pctemploy) #65.90622
mean(hcvchar$hcv_pct_of_total) #1.785714
median(hcvchar$hcv_pct_of_total) #0.305002
summary(hcvchar)

hcvchar %>% summarise(cor(hhinc_med, hcv_pct_of_total)) #-0.3421
hcvchar %>% summarise(cor(pctcollplus, hcv_pct_of_total)) #-0.4636

hcvchar <- as.numeric(hcvchar$pctbelowpoverty)
hcvchar <- as.data.frame(hcvchar)

hcvchar <- subset(hcvchar, GEOID %in% c(37063000101, 37063000102, 37063000200, 37063000301, 37063000302, 37063000401, 37063000402, 37063000500, 37063000600, 37063000700, 37063000900, 
                                                    37063001001, 37063001002, 37063001100, 37063001301, 37063001303, 37063001304, 37063001400, 37063001501, 
                                                    37063001503, 37063001601, 37063001603, 37063001604, 37063001705, 37063001706, 
                                                    37063001708, 37063001709, 37063001710, 37063001711, 37063001801, 37063001802, 37063001806, 
                                                    37063001808, 37063001809, 37063001900, 37063002007, 37063002008, 37063002009, 37063002013, 37063002015, 
                                                    37063002019, 37063002020, 37063002021, 37063002022, 37063002023, 
                                                    37063002024, 37063002025, 37063002026, 37063002100, 37063002200, 37063002300, 37063980100))

#backup
COPYhcvchar <- data.frame(hcvchar)






