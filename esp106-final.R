knitr::opts_chunk$set(echo = FALSE)

# libraries
library(dplyr)
library(tidyverse)
library(stringdist)
library(terra)
library(geodata)
library(ggplot2)
library(knitr)



# dataset 1
ds_1 = read.csv("https://ourworldindata.org/grapher/share-of-adults-who-smoke.csv?v=1&csvType=full&useColumnShortNames=true")
ds_1 = ds_1 %>%
  select(Entity, Year, sh_prv_smok) # select relevant columns

# rename columns to match ds_2
colnames(ds_1) = c("location", "year", "sh_prv_smok")



# dataset 2
ds_2 = read.csv("IHME-GBD_2021_DATA-a081ae7a-1.csv")
ds_2 = ds_2 %>%
  select(measure, location, year, val) %>% # select relevant columns
  filter(year > 1999, year < 2021) %>% # limit to same years as dataset 1
  filter(measure == "Deaths") %>% # limit to only data on deaths
  select(location, year, val)

# make location names constant across ds_1 and ds_2
ds_2$location = gsub("Republic of ", "", ds_2$location)
ds_2$location = gsub("Republic", "", ds_2$location)

ds_2$closest_match <- sapply(ds_2$location, function(x) {
  ds_1$location[which.min(stringdist::stringdist(x, ds_1$location))]
})

ds_2a = ds_2 %>% # see closest matches and determine if they are right
  select(location, closest_match) %>%
  distinct(location, .keep_all = TRUE)

# manual matching of incorrectly matched countries
# sorry for the repetitive code... I couldn't find a better way to do this non-tediously as it was a case-by-case basis and already tried to cut down on most of the work...
ds_2 <- ds_2 %>%
  mutate(location = ifelse(location == "Nicaragua", "none", location)) %>%
  mutate(location = ifelse(location == "Brunei Darussalam", "Brunei", location)) %>%
  mutate(location = ifelse(location == "Federal Democratic Nepal", "Nepal", location)) %>%
  mutate(location = ifelse(location == "Democratic Timor-Leste", "none", location)) %>%
  mutate(location = ifelse(location == "Cook Islands", "none", location)) %>%
  mutate(location = ifelse(location == "Saint Vincent and the Grenadines", "none", location)) %>%
  mutate(location = ifelse(location == "Sultanate of Oman", "Oman", location)) %>%
  mutate(location = ifelse(location == "United Mexican States", "Mexico", location)) %>%
  mutate(location = ifelse(location == "Saint Lucia", "none", location)) %>%
  mutate(location = ifelse(location == "Central African", "none", location)) %>%
  mutate(location = ifelse(location == "Bermuda", "none", location)) %>%
  mutate(location = ifelse(location == "Palestine", "none", location)) %>%
  mutate(location = ifelse(location == "Guinea", "none", location)) %>%
  mutate(location = ifelse(location == "Principality of Monaco", "none", location)) %>%
  mutate(location = ifelse(location == "Independent State of Samoa", "none", location)) %>%
  mutate(location = ifelse(location == "Equatorial Guinea", "none", location)) %>%
  mutate(location = ifelse(location == "Sudan", "none", location)) %>%
  mutate(location = ifelse(location == "Angola", "none", location)) %>%
  mutate(location = ifelse(location == "Taiwan", "none", location)) %>%
  mutate(location = ifelse(location == "South Sudan", "none", location)) %>%
  mutate(location = ifelse(location == "Swiss Confederation", "Sweden", location)) %>%
  mutate(location = ifelse(location == "Gabonese", "none", location)) %>%
  mutate(location = ifelse(location == "Northen Mariana Islands", "none", location)) %>%
  mutate(location = ifelse(location == "Plurinational State of Bolivia", "Bolivia", location)) %>%
  mutate(location = ifelse(location == "Hashemite Kingdom of Jordan", "Jordan", location)) %>%
  mutate(location = ifelse(location == "Dominican Republic", "Dominican", location)) %>%
  mutate(location = ifelse(location == "Federal Somalia", "none", location)) %>% 
  mutate(location = ifelse(location == "Dijibouti", "none", location)) %>%
  mutate(location = ifelse(location == "Grenada", "none", location)) %>%
  mutate(location = ifelse(location == "Tokelau", "none", location)) %>%
  mutate(location = ifelse(location == "Democratic Socialist Sri Lanka", "Sri Lanka", location)) %>%
  mutate(location = ifelse(location == "North Macedonia", "none", location)) %>%
  mutate(location = ifelse(location == "Kingdom of Bhutan", "none", location)) %>%
  mutate(location = ifelse(location == "Hellenic", "none", location)) %>%
  mutate(location = ifelse(location == "American Samoa", "none", location)) %>%
  mutate(location = ifelse(location == "Korea", "South Korea", location)) %>%
  mutate(location = ifelse(location == "State of Libya", "none", location)) %>%
  mutate(location = ifelse(location == "Democratic People's Korea", "North Korea", location)) %>%
  mutate(location = ifelse(location == "United States Virgin Islands", "none", location)) %>%
  mutate(location = ifelse(location == "Guam", "none", location)) %>%
  mutate(location = ifelse(location == "Federated States of Micronesia", "none", location)) %>%
  mutate(location = ifelse(location == "Lao People's Democratic", "Laos", location)) %>%
  mutate(location = ifelse(location == "Niue", "none", location)) %>%
  mutate(location = ifelse(location == "Bolivarian Venezuela", "none", location)) %>%
  mutate(location = ifelse(location == "Greenland", "none", location)) %>%
  mutate(location = ifelse(location == "Syrian Arab", "none", location)) %>%
  mutate(location = ifelse(location == "Commonwealth of Dominica", "none", location)) %>%
  mutate(location = ifelse(location == "Suriname", "none", location)) %>%
  mutate(location = ifelse(location == "State of Qatar", "Qatar", location)) %>%
  mutate(location = ifelse(location == "United Arab Emirates", "none", location)) %>%
  mutate(location = ifelse(location == "Trinidad and Tobago", "none", location)) %>%
  mutate(location = ifelse(location == "Russian Federation", "Russia", location)) %>%
  mutate(location = ifelse(location == "Honduras", "none", location)) %>%
  mutate(location = ifelse(location == "Antigua and Barbuda", "none", location)) %>%
  mutate(location = ifelse(location == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom", location)) %>%
  mutate(location = ifelse(location == "San Marino", "none", location)) %>%
  mutate(location = ifelse(location == "Puerto Rico", "none", location)) %>%
  mutate(location = ifelse(location == "Saint Kitts and Nevis", "none", location)) %>%
  filter(location != "none")

# reapply closest match
ds_2$closest_match <- sapply(ds_2$location, function(x) {
  ds_1$location[which.min(stringdist::stringdist(x, ds_1$location))]
})

# replace location with closest match
ds_2 = ds_2 %>%
  select(closest_match, year, val)

colnames(ds_2) = c("location", "year", "val")


# data merging into df

ds_A = merge(ds_1, ds_2, by = c("location", "year"))
colnames(ds_A) = c("location", "year", "p_smoke", "p_lung_death")


# spatial data merging

wrld = world(path=".") # read in wrld data

wrld$location <- wrld$NAME_0 # make column names same
sv_A = merge(wrld, ds_A, by = "location", all.x = FALSE) # merge

r = rast(sv_A, res = 0.5) # template

raster_list = list() # make layers (one for each variable and year combo, 14 in total)
for (yr in unique(sv_A$year)) {
  year_data = sv_A[sv_A$year == yr, ]
  rast1 = rasterize(year_data, r, "p_smoke")
  rast2 = rasterize(year_data, r, "p_lung_death")
  
  names(rast1) <- paste0("p_smoke", yr)
  names(rast2) <- paste0("p_lung_death", yr)
  
  raster_list <- c(raster_list, list(rast1, rast2))
}
sr_A <- do.call(c, raster_list) # merge all layers



# read in data (manually) from study
ct_A = data.frame(
  smoking = c("never", "never", "former", "former", "current", "current"),
  lung_cancer = c("yes", "no", "yes", "no", "yes", "no"),
  individuals = c(33,54,163,99,114,41),
  proportion = c(33/87, 54/87, 163/262, 99/262, 114/155, 41/155)
)

# show data in nice format
tab = xtabs(individuals ~ smoking + lung_cancer, data = ct_A)
kable(tab, format = "html", table.attr ="class='table table striped'")



# calculate summary statistics (multiply lung cancer by 100 to better match smoking data)
sum_stats_A = data.frame(
  Measure = c("Percent of Smokers", "Percent of Deaths from Lung Cancer"),
  Minimum = c(round(min(ds_A$p_smoke),2), round(min(ds_A$p_lung_death)*100,2)),
  Median = c(round(median(ds_A$p_smoke),2), round(median(ds_A$p_lung_death)*100,2)),
  Mean = c(round(mean(ds_A$p_smoke),2), round(mean(ds_A$p_lung_death)*100,2)),
  SD = c(round(sd(ds_A$p_smoke),2), round(sd(ds_A$p_lung_death)*100,2)),
  Maximum = c(round(max(ds_A$p_smoke),2), round(max(ds_A$p_lung_death)*100,2))
)

# display in table
kable(sum_stats_A, format = "html", table.attr ="class='table table striped'")



# read in continent df
continent_df = read.csv("Countries by continents.csv")

# assign a continent to each unique country
conts = c()
for (country in unique(ds_A$location)) {
  if (country %in% continent_df$Country) {
    i = which(continent_df$Country == country)
    cont = continent_df$Continent[i]
    conts = c(conts, cont)
  } else {
    conts = c(conts, "manual") # check for mistakes/spelling differences
  }
}
temp_df = data.frame(
  country = unique(ds_A$location),
  continent = conts
)

# manual input for mistakes
temp_df$continent[which(temp_df$country == "Burkina Faso")] = "Africa"
temp_df$continent[which(temp_df$country == "Cote d'Ivoire")] = "Africa"
temp_df$continent[which(temp_df$country == "Eswatini")] = "Africa"
temp_df$continent[which(temp_df$country == "Myanmar")] = "Asia"

# assign continents agai, using the manual inputs
conts = c()
for (country in ds_A$location) {
  if (country %in% temp_df$country) {
    i = which(temp_df$country == country)
    cont = temp_df$continent[i]
    conts = c(conts, cont)
  } else {
    conts = c(conts, "manual")
  }
}

ds_A$continent = conts

# aggregate mean by year and continent
agg_df_A = aggregate(ds_A, by = list(ds_A$continent, ds_A$year), FUN = mean)
agg_df_A = agg_df_A %>%
  select(Group.1, Group.2, p_smoke, p_lung_death)
colnames(agg_df_A) = c("continent", "year", "p_smoke", "p_lung_death")


# plot 1
ggplot(agg_df_A, aes(x = year, y = p_smoke/100, col = continent)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(title = "Smokers by Continent by Year",
       x = "Year",
       y = "Percentage of Smokers", 
       col = "Continent")


# plot 2
ggplot(agg_df_A, aes(x = year, y = p_lung_death, col = continent)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(title = "Lung Cancer Deaths by Continent by Year",
       x = "Year",
       y = "Percentage of Deaths", 
       col = "Continent")



# aggregate mean of smoking by location
ds_1a = aggregate(ds_1$sh_prv_smok, by = list(ds_1$location), FUN = mean)

# make histogram
appendix_1a = ggplot(ds_1a, aes(x = x)) +
  geom_histogram(bins = 10, fill = "pink", color = "hotpink") +
  labs(
    title = "Distribution of Average Percentage of Smokers, 2000-2020",
    x = "Percentage of Smokers per Country",
    y = "Number of Countries"
  ) +
  theme_minimal()

# shapiro test
st_1 = shapiro.test(ds_1a$x)



# aggregate mean of lung cancer deaths by location
ds_2a = aggregate(ds_2$val, by = list(ds_2$location), FUN = mean)

# make histogram
appendix_1b = ggplot(ds_2a, aes(x = x)) +
  geom_histogram(bins=10, fill = "lightblue", color = "blue") +
  labs(
    title = "Distribution of Average Percentage of Lung Cancer Deaths, 2000-2020",
    x = "Percentage of Lung Cancer Deaths per Country",
    y = "Number of Countries"
  ) +
  theme_minimal()

# shapiro test
st_2 = shapiro.test(ds_2a$x)


# subset and names
sr_A_smoke = sr_A[[c(1, 3, 5, 7, 9, 11, 13)]]
names(sr_A_smoke) = c("Percentage of Smokers, 2000", "Percentage of Smokers, 2005", "Percentage of Smokers, 2010", "Percentage of Smokers, 2015", "Percentage of Smokers, 2018", "Percentage of Smokers, 2019", "Percentage of Smokers, 2020")

# get range
mm = minmax(sr_A_smoke)
rg1 = c(min(mm[1,]), max(mm[2,]))
  

## # The code for each of the plots. To save processing power, animate() itself was not used, and the images of each of the plots was downloaded and turned into a gif.
## 
## animate(sr_A_smoke, range=rg1, pause=1, n=1, fun=\() lines(wrld))
## 

# subset
sr_A_lung = sr_A[[c(2, 4, 6, 8, 10, 12, 14)]]
names(sr_A_lung) = c("Percentage of Deaths from Lung Cancer, 2000", "Percentage of Deaths from Lung Cancer, 2005", "Percentage of Deaths from Lung Cancer, 2010", "Percentage of Deaths from Lung Cancer, 2015", "Percentage of Deaths from Lung Cancer, 2018", "Percentage of Deaths from Lung Cancer, 2019", "Percentage of Deaths from Lung Cancer, 2020")

# range
mm = minmax(sr_A_lung)
rg2 = c(min(mm[1,]), max(mm[2,]))


## # The code for each of the plots. To save processing power, animate() itself was not used, and the images of each of the plots was downloaded and turned into a gif.
## 
## animate(sr_A_lung, range=rg2, pause=1, n=1, fun=\() lines(wrld))


# dataset 3
ds_3 = read.csv("https://ourworldindata.org/grapher/sales-of-cigarettes-per-adult-per-day.csv?v=1&csvType=full&useColumnShortNames=true")
ds_3 = ds_3 %>%
  select(Entity, Year, manufactured_cigarettes_per_adult_per_day) # select relevant columns

# rename columns to match ds_4
colnames(ds_3) = c("location", "year", "cigs_per_adult_per_day")



# dataset 4
ds_4 = read.csv("https://ourworldindata.org/grapher/affordability-cigarettes.csv?v=1&csvType=full&useColumnShortNames=true")
ds_4 = ds_4 %>%
  select(Entity, Year, affordability_of_cigarettes__percentage_of_gdp_per_capita_required_to_purchase_2000_cigarettes_of_the_most_sold_brand) # select relevant columns

# rename columns
colnames(ds_4) = c("location", "year", "price_p_gdp")



# data merging into df

ds_B = merge(ds_3, ds_4, by = c("location", "year"))
colnames(ds_B) = c("location", "year", "cigs_per_adult_per_day", "price_p_gdp") # rename columns



# aggregate mean by location
ds_3a = aggregate(ds_3$cigs_per_adult_per_day, by = list(ds_3$location), FUN = mean)

# make histogram
appendix_1c = ggplot(ds_3a, aes(x = x)) +
  geom_histogram(bins = 8, fill = "lightgreen", color = "darkgreen") +
  labs(
    title = "Distribution of Average Cigarettes per Adult per Day, 1865-2015",
    x = "Average Cigarettes per Adult per Day",
    y = "Number of Countries"
  ) +
  theme_minimal()

# shapiro test
st_3 = shapiro.test(ds_3a$x)

# aggregate df by location
ds_4a = aggregate(ds_4$price_p_gdp, by = list(ds_4$location), FUN = mean)

# make histogram
appendix_1d = ggplot(ds_4a, aes(x = x)) +
  geom_histogram(bins=8, fill = "lavender", color = "purple") +
  labs(
    title = "Distribution of Price of Cigarettes, 2012-2022",
    x = "Average Quintile of Tobacco Tax Raise",
    y = "Number of Countries"
  ) +
  theme_minimal()

# shapiro test
st_4 = shapiro.test(ds_4a$x)



# calculate summary statistics
sum_stats_B = data.frame(
  Measure = c("Cigarettes Sold", "Cigarette Price"),
  Minimum = c(round(min(ds_3$cigs_per_adult_per_day),2), round(min(ds_4$price_p_gdp),2)),
  Median = c(round(median(ds_3$cigs_per_adult_per_day),2), round(median(ds_4$price_p_gdp),2)),
  Mean = c(round(mean(ds_3$cigs_per_adult_per_day),2), round(mean(ds_4$price_p_gdp),2)),
  SD = c(round(sd(ds_3$cigs_per_adult_per_day),2), round(sd(ds_4$price_p_gdp),2)),
  Maximum = c(round(max(ds_3$cigs_per_adult_per_day),2), round(max(ds_4$price_p_gdp),2))
)

# display in table
kable(sum_stats_B, format = "html", table.attr ="class='table table striped'")


### NOTE: I have repeated code here because of the manual input.

# assign continent to each unique country 
conts = c()
for (country in unique(ds_3$location)) {
  if (country %in% continent_df$Country) {
    i = which(continent_df$Country == country)
    cont = continent_df$Continent[i]
    conts = c(conts, cont)
  } else {
    conts = c(conts, "manual") # flag mistakes
  }
}
temp_df = data.frame(
  country = unique(ds_3$location),
  continent = conts
)

# look at mistakes
#temp_df[which(temp_df$continent == "manual"),]

### NOTE: I have repeated code here because of the manual input.
# manual input for mistakes
temp_df$continent[which(temp_df$country == "Czechoslovakia")] = "Europe"
temp_df$continent[which(temp_df$country == "North Macedonia")] = "Europe"
temp_df$continent[which(temp_df$country == "USSR")] = "Europe"
temp_df$continent[which(temp_df$country == "Yugoslavia")] = "Europe"

# now assign continents using manual inputs
conts = c()
for (country in ds_3$location) {
  if (country %in% temp_df$country) {
    i = which(temp_df$country == country)
    cont = temp_df$continent[i]
    conts = c(conts, cont)
  } else {
    conts = c(conts, "manual")
  }
}
ds_3$continent = conts

# aggregate by continent and year
agg_df_3 = aggregate(ds_3$cigs_per_adult_per_day, by = list(ds_3$continent, ds_3$year), FUN = mean)
colnames(agg_df_3) = c("continent", "year", "cigs") # rename

# line plot
ggplot(agg_df_3, aes(x = year, y = cigs, col = continent)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Cigarettes Consumed by Continent",
       x = "Year",
       y = "Cigarettes Consumed per Adult per Day",
       col = "Continent")

### NOTE: I have repeated code here because of the manual input.

# assign continent per unique country, flag mistakes
conts = c()
for (country in unique(ds_4$location)) {
  if (country %in% continent_df$Country) {
    i = which(continent_df$Country == country)
    cont = continent_df$Continent[i]
    conts = c(conts, cont)
  } else {
    conts = c(conts, "manual") # for mistakes
  }
}
temp_df = data.frame(
  country = unique(ds_4$location),
  continent = conts
)

# look at mistakes
#temp_df[which(temp_df$continent == "manual"),]

### NOTE: I have repeated code here because of the manual input.
# manual input for mistakes
temp_df$continent[which(temp_df$country == "North Macedonia")] = "Europe"
temp_df$continent[which(temp_df$country == "Burkina Faso")] = "Africa"
temp_df$continent[which(temp_df$country == "Cote d'Ivoire")] = "Africa"
temp_df$continent[which(temp_df$country == "Eswatini")] = "Africa"
temp_df$continent[which(temp_df$country == "Myanmar")] = "Asia"
temp_df$continent[which(temp_df$country == "Palestine")] = "Asia"
temp_df$continent[which(temp_df$country == "Micronesia (country)")] = "Oceania"

# now assign continent based on country and manual input
conts = c()
for (country in ds_4$location) {
  if (country %in% temp_df$country) {
    i = which(temp_df$country == country)
    cont = temp_df$continent[i]
    conts = c(conts, cont)
  } else {
    conts = c(conts, "manual")
  }
}
ds_4$continent = conts

# aggregate by continent and year
agg_df_4 = aggregate(ds_4$price_p_gdp, by = list(ds_4$continent, ds_4$year), FUN = mean)
colnames(agg_df_4) = c("continent", "year", "price")


# plot
ggplot(agg_df_4, aes(x = year, y = price, col = continent)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Price of Cigarettes by Continent",
       x = "Year",
       y = "Price of 100 Packs (% of GDP)",
       col = "Continent")


# calculate correlation by country
corrs = c()
for (country in unique(ds_A$location)) {
  ds_A2 = ds_A[which(ds_A$location == country),]
  corr = cor(ds_A2$p_smoke, ds_A2$p_lung_death)
  corrs = c(corrs, corr)
}

# make dataframe of correlation
corr_df = data.frame(
  location = unique(ds_A$location),
  corr = corrs
)

# merge with wrld geometries
sv_C = terra::merge(wrld, corr_df, by = "location", all.x = FALSE)
r = rast(ext(sv_C), res = 0.5)
rast1 = rasterize(sv_C, r, "corr")
sr_C = c(rast1)

# plot
plot(sr_C)
plot(wrld, add = TRUE, border = "darkgray")
title(main = "Correlation of Smoking and Lung Cancer")

# calculate overall correlation
overall_cor = cor(ds_A$p_smoke, ds_A$p_lung_death)


# gdp dataset
ds_C = read.csv("https://ourworldindata.org/grapher/gdp-per-capita-worldbank.csv?v=1&csvType=full&useColumnShortNames=true")

ds_C = aggregate(ds_C$ny_gdp_pcap_pp_kd, by = list(ds_C$Entity), FUN = "mean")

colnames(ds_C) = c("location", "gdp")

# merge correlation df with gdp dataset
corr_df = merge(corr_df, ds_C, by = "location")

# regressions

lm_C = lm(corr ~ log(gdp), corr_df) # linear

loess_C = loess(corr ~ log(gdp), corr_df, span = 0.7) # loess
corr_df$loess = predict(loess_C)

plm_C = lm(corr ~ poly(log(gdp), 2), corr_df) # polynomial 
corr_df$plm = predict(plm_C)

# graph
ggplot(corr_df, aes(x = log(gdp), y = corr)) +
  geom_point() +
  labs(title = "Correlation of Lung Cancer and Smoking Status by GDP", 
       x = "log(GDP per Capita)",
       y = "Pearson's correlation value",
       subtitle = "Pink: loess model, Blue: linear model, Green: polynomial model") +
  geom_line(aes(y = loess), color = "hotpink", size = 1, alpha = 0.5) +
  geom_abline(slope = 0.23805, intercept = -2.6743, col = "steelblue", size = 1, alpha = 0.5) +
  geom_line(aes(y = plm), color = "springgreen3", size = 1, alpha = 0.5) +
  theme_minimal()


# compare regressions (AIC)

## linear
aic_lm = AIC(lm_C)

## loess
rss_loess <- sum((corr_df$corr - corr_df$loess)^2)
tss <- sum((corr_df$corr - mean(corr_df$corr))^2)
r2_loess <- 1 - (rss_loess/tss)
n <- nrow(corr_df)
sigma_loess <- sqrt(rss_loess / n)
aic_loess <- n * log(sigma_loess^2) + 2 * 2

## polynomial
aic_plm = AIC(plm_C)

# display AICs in table

aic_df = data.frame(
  regression = c("Linear", "Loess", "Polynomial"),
  AIC = c(aic_lm, aic_loess, aic_plm)
)

kable(aic_df, format = "html", table.attr ="class='table table striped'")


#plot
ggplot(ct_A, aes(x = smoking, y = proportion, fill = lung_cancer)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(
    title = "Lung Cancer by Smoking Status",
    x = "Smoking Status",
    y = "Proportion",
    fill = "Has Lung Cancer"
  ) +
  scale_fill_manual(values = c("darkgray", "slategray")) +
  theme_minimal()


# format data for chi sq test
ct_A_mat = matrix(c(41, 114,
                    99, 163,
                    54, 33), nrow = 3, byrow = TRUE)
rownames(ct_A_mat) = c("current", "former", "never")
colnames(ct_A_mat) = c("no", "yes")

# test
ct_A_chi = chisq.test(ct_A_mat)


# make new dataframe
ds_B2 = data.frame(
  location = unique(ds_B$location)
)

# get difference for each location
diff_prices = c()
diff_cigs = c()
for (loc in ds_B2$location) {
  rows = ds_B[which(ds_B$location == loc),]
  diff_cig = rows$cigs_per_adult_per_day[2] - rows$cigs_per_adult_per_day[1]
  diff_price = rows$price_p_gdp[2] - rows$price_p_gdp[1]
  
  diff_prices = c(diff_prices, diff_price)
  diff_cigs = c(diff_cigs, diff_cig)
}

# add to dataframe
ds_B2 = data.frame(
  location = unique(ds_B$location),
  diff_price = diff_prices,
  diff_cigs = diff_cigs
)

# remove countries with less than 2 values
ds_B2 = remove_missing(ds_B2)

# regressions
lm_B2 = lm(diff_cigs ~ diff_price, ds_B2) # linear

plm_B2 = lm(diff_cigs ~ poly(diff_price, 2), ds_B2) # polynomial

# plot
ggplot(ds_B2, aes(x = diff_price, y = diff_cigs)) + 
  geom_point() +
  labs(
    title = "Difference in Price vs. Number Consumed (2012 to 2014)", 
    y = "Difference in Cigarettes Consumed/Day",
    x = "Difference in Price (Percentage of GDP)", 
    subtitle = "y = -1.46959x - 0.19214") +
  geom_abline(slope = -1.46959, intercept = -0.19214, col = "hotpink") +
  theme_minimal()


appendix_1a
appendix_1b

appendix_1c
appendix_1d

summary(lm_C)

summary(plm_C)

summary(loess_C)

summary(lm_B2)

summary(plm_B2)

ct_A_chi

plot(sr_A$p_smoke2000, range = rg1)
plot(wrld, add = TRUE, border = "black")
title(main = "Percentage of Smokers, 2000")

plot(sr_A$p_smoke2005, range = rg1)
plot(wrld, add = TRUE, border = "black")
title(main = "Percentage of Smokers, 2005")

plot(sr_A$p_smoke2010, range = rg1)
plot(wrld, add = TRUE, border = "black")
title(main = "Percentage of Smokers, 2010")

plot(sr_A$p_smoke2015, range = rg1)
plot(wrld, add = TRUE, border = "black")
title(main = "Percentage of Smokers, 2015")

plot(sr_A$p_smoke2018, range = rg1)
plot(wrld, add = TRUE, border = "black")
title(main = "Percentage of Smokers, 2018")

plot(sr_A$p_smoke2019, range = rg1)
plot(wrld, add = TRUE, border = "black")
title(main = "Percentage of Smokers, 2019")

plot(sr_A$p_smoke2020, range = rg1)
plot(wrld, add = TRUE, border = "black")
title(main = "Percentage of Smokers, 2020")

plot(sr_A$p_lung_death2000, range = rg2)
plot(wrld, add = TRUE, border = "black")
title(main = "Percentage of Deaths from Lung Cancer, 2000")

plot(sr_A$p_lung_death2005, range = rg2)
plot(wrld, add = TRUE, border = "black")
title(main = "Percentage of Deaths from Lung Cancer, 2005")

plot(sr_A$p_lung_death2010, range = rg2)
plot(wrld, add = TRUE, border = "black")
title(main = "Percentage of Deaths from Lung Cancer, 2010")

plot(sr_A$p_lung_death2015, range = rg2)
plot(wrld, add = TRUE, border = "black")
title(main = "Percentage of Deaths from Lung Cancer, 2015")

plot(sr_A$p_lung_death2018, range = rg2)
plot(wrld, add = TRUE, border = "black")
title(main = "Percentage of Deaths from Lung Cancer, 2018")

plot(sr_A$p_lung_death2019, range = rg2)
plot(wrld, add = TRUE, border = "black")
title(main = "Percentage of Deaths from Lung Cancer, 2019")

plot(sr_A$p_lung_death2020, range = rg2)
plot(wrld, add = TRUE, border = "black")
title(main = "Percentage of Deaths from Lung Cancer, 2020")
