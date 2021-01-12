---
title: "COVID-19 October Project - Predicting Deaths in November"
author: "Nicholas Archambault, Melissa Lu"
output: html_notebook
---

# Clean the script
rm(list = ls())

# Libraries
library(ggplot2)
library(ggpubr)
library(leaflet)
library(gridExtra)
library(gpairs)
library(grid)
library(knitr) # knit to PDF
library(usmap) # maps
library(maps)
library(scales) # scale for ggplot
library(mapproj)
library(plotly)
library(corrplot) # correlation plots
library(PerformanceAnalytics) # correlation plots

# Read in full data sets: time series and current picture
covid <- read.csv("covid.full.csv", as.is = TRUE, 
                  stringsAsFactors = FALSE, row.names = 1)
current <- read.csv("current.csv", as.is = TRUE, 
                    stringsAsFactors = FALSE, row.names = 1) 
current_standardized <- read.csv("current_standardized.csv", 
                                 as.is = TRUE, 
                                 stringsAsFactors = FALSE, row.names = 1) 
# FACTORS
#### Define separate dataframe of only categorical variables
categ <- current[, 39:54]

# Step through each categorical variable. Convert to factor and clean. Missing
# values filled with "no data"

table(categ$status.of.reopening) # four levels
categ$status.of.reopening <- factor(tolower(categ$status.of.reopening)) 
levels(categ$status.of.reopening) <- c("restricted", "paused", "reopened", 
                                       "reopened")

table(categ$stay.at.home.order) # five levels
categ$stay.at.home.order <- factor(tolower(categ$stay.at.home.order))
levels(categ$stay.at.home.order) <- c("no data", "lifted", "high risk", 
                                      "high risk", "statewide")

table(categ$non.essential.business.closures) # six levels
categ$non.essential.business.closures <- 
  factor(tolower(categ$non.essential.business.closures))
levels(categ$non.essential.business.closures) <- c("no data", "all reopen", 
                                                   "all restricted reopen", 
                                                   "new closures", 
                                                   "some reopen", 
                                                   "some restricted reopen")

table(categ$large.gatherings.ban) # seven levels
categ$large.gatherings.ban <- factor(tolower(categ$large.gatherings.ban))
levels(categ$large.gatherings.ban) <- c("no data", "above 10 banned", 
                                        "all banned", "above 25 banned", 
                                        "normal", "normal", "new limits")

table(categ$restaurant.limits) # four levels
categ$restaurant.limits <- factor(tolower(categ$restaurant.limits))
levels(categ$restaurant.limits) <- c("no data", "new limits", "reopened", 
                                     "restricted")

table(categ$bar.closures) # four levels
categ$bar.closures <- factor(tolower(categ$bar.closures))
levels(categ$bar.closures) <- c("closed", "new limits", "closed", "reopened")

table(categ$face.covering.requirement) # five levels
categ$face.covering.requirement <- 
  factor(tolower(categ$face.covering.requirement))
levels(categ$face.covering.requirement) <- c("no data", "none", 
                                             "employees", "employees", 
                                             "public")

# Convert region to lower case
categ$region <- tolower(categ$region)

# Fill NA values in lockdown start and end date columns with "no data"
categ[which(is.na(categ$start.of.stay.at.home)), 8] <- "no data"
categ[which(is.na(categ$end.of.stay.at.home)), 9] <- "no data"

# If a state has a defined lockdown start date but no defined lockdown end date,
# assume the lockdown is ongoing. Fill the end date column with the most recent
# date, as taken from the 'covid.latest' dataframe
categ$end.of.stay.at.home[(categ$start.of.stay.at.home != "no data") & 
                            (categ$end.of.stay.at.home == "no data")] <- 
  format(covid.latest$date[1], "%d-%b")

# Calculate the duration of each state's lockdown
d1 <- as.Date(categ$start.of.stay.at.home, format = "%d-%b")
d2 <- as.Date(categ$end.of.stay.at.home, format = "%d-%b")

categ$duration <- as.numeric(difftime(d2, d1, units = "days"))

categ$duration[which(is.na(categ$duration))] <- "no data"

# Reinsert cleaned categorical data into 'current'
current <- cbind(current[, 1:38], categ, current[, 55:ncol(current)])

# Render appropriate variables as factors
for (i in c(1, 26:27, 40:54)) {current[, i] <- factor(current[, i])}

#DATA EXPLORATION AND VISUALIZATION 

# Correlation
current.num <- current[,-c(1, 26:27, 39:55)]

# table of correlations
round(cor(current.num), 3)

# To see this much more easily, we can use corrplot
corrplot(cor(current.num), method = "ellipse", tl.cex = 0.5)

# Deaths
boxplot(deaths_per_100k ~ region, data = current, 
        col = c("forestgreen"), horizontal = TRUE, 
        main = "Deaths Per 100K By Region", xlab = "Deaths Per 100K", 
        ylab = "Region", names = c("Midwest", "Northeast", "South", "West"))
mtext("National Median", side = 3, at = median(current$deaths_per_100k), 
      cex = 0.8, col = "red", font = 2)
mtext(round(median(current$deaths_per_100k)), side = 1, 
      at = median(current$deaths_per_100k), cex = 0.8, col = "red", font = 2)
abline(v = median(current$deaths_per_100k), lty = 1, lwd = 3, col = "red")

# People tested
boxplot(people_tested_per_100k ~ region, data = current, 
        col = c("gold"), horizontal = TRUE, 
        main = "People Tested Per 100K By Region", 
        xlab = "People Tested Per 100K", 
        ylab = "Region", names = c("Midwest", "Northeast", "South", "West"))
mtext("National Median", side = 3, at = median(current$people_tested_per_100k), 
      cex = 0.8, col = "red", font = 2)
mtext(round(median(current$people_tested_per_100k)), side = 1, 
      at = median(current$people_tested_per_100k), cex = 0.8, col = "red", 
      font = 2)
abline(v = median(current$people_tested_per_100k), lty = 1, lwd = 3, 
       col = "red")

#### RADAR PLOT

# Create dataframe of means of variables we want to use
agg <- aggregate(current[, c("density", "people_tested_per_100k", 
                             "hospitals_per_100k", "beds_per_100k", 
                             "frequently_per_100k", "always_per_100k")], 
                 by = list(current$region), 
                 FUN = mean)

# Create new metrics pertaining to population density, testing capacity, access
# to healthcare, and compliance with social distancing protocols. These are 
# groups of factors that likely effect how well a region is dealing with the 
# virus

density <- agg$density     # states' population density (lower is better)
testing <- agg$people_tested_per_100k     # people tested per 100K
# access to hospitals and beds per 100K
healthcare <- agg$hospitals_per_100k + agg$beds_per_100k   
# people per 100K who always or frequently comply with social distancing
socialdist <- agg$frequently_per_100k + agg$always_per_100k

# Populate a dataframe with these values
regional <- as.data.frame(matrix(NA, ncol = 4, nrow = 4))
names(regional) <- c("Population Density", "Testing", "Healthcare", 
                     "Social\nDistancing")

regional[, 1] <- density
regional[, 2] <- testing
regional[, 3] <- healthcare
regional[, 4] <- socialdist

# Label rows by geographic region
rownames(regional) <- c("Midwest", "Northeast", "South", "West")

# Standardize each variable to be between 0 and 1
for (i in 1:ncol(regional)) {
  regional[, i] <- (regional[, i] - min(regional[, i])) / 
    (max(regional[, i] - min(regional[, i])))
}

# Flip the density variable -- a higher density corresponds to a lower
# 'score', since higher density means greater risk of transmission
regional$`Population Density`<- 1 - regional$`Population Density`

# Include the maxes and mins of each column
maxes <- apply(regional, 2, max)
mins <- apply(regional, 2, min)
regional <- rbind(maxes, mins, regional)

# Set plot colors
colors_border <- c(rgb(153/255, 0/255, 76/255, 0.9), 
                   rgb(51/255, 153/255, 1, 0.9) , 
                   rgb(1, 153/255, 51/255, 0.9), 
                   rgb(0, 102/255, 51/255, 0.9))
colors_in <- c(rgb(153/255, 0/255, 76/255, 0.15), 
               rgb(51/255, 153/255, 1, 0.15) , 
               rgb(1, 153/255, 1/255, 0.15), 
               rgb(0, 102/255, 51/255, 0.15))

# Create plot and add legend
radarchart(regional, axistype = 1, pcol = colors_border, pfcol = colors_in, 
           plwd = 3, plty = 1, cglcol = "gray", cglty = 1, axislabcol = "gray", 
           cglwd = 1, vlcex = 0.9, caxislabels = seq(0, 125, 25),
           title = "Regions' Areas of Strength in
           Mitigating COVID's Spread")
legend(x = 0.9, y = 1, legend = rownames(regional[-c(1, 2), ]), bty = "n", 
       pch = 20, col = colors_border , text.col = "grey", cex = 1.2, pt.cex = 3)

#### BARPLOT

# Create new column in 'current'
current$politics <- NA

# We will use the results of the 2016 presidential election as a proxy for 
# assessing states' political affiliations. Group the percent differences in
# voting into six categories representing how liberal or conservative a state
# is, depending on its 2016 election results
if(current$pct_diff <= -20) {current$politics <- "Strong Conservative"}

current$politics[current$pct_diff <= -20] <- "Strong Conservative"
current$politics[(current$pct_diff > -20 & current$pct_diff <= -10)] <- 
  "Conservative"
current$politics[(-10 < current$pct_diff & current$pct_diff <= 0)] <- 
  "Slight Conservative"
current$politics[(0 < current$pct_diff & current$pct_diff <= 10)] <- 
  "Slight Liberal"
current$politics[(10 < current$pct_diff & current$pct_diff <= 20)] <- 
  "Liberal"
current$politics[(20 < current$pct_diff)] <- 
  "Strong Liberal"

# Create new aggregated dataframe of means of confirmed cases for 
# each political subcategory
agg_poli <- aggregate(current$confirmed_per_100k, by = list(current$politics), 
                      FUN = mean)

# Change level names of groups
agg_poli$Group.1 <- factor(c("Strong Liberal\n(Clinton won by > 20%)",
                             "Liberal\n(Clinton won by 10% - 19%)",
                             "Slight Liberal\n(Clinton won by 0% - 9%)",
                             "Slight Conservative\n(Trump won by 0% - 9%)",
                             "Conservative\n(Trump won by 10% - 19%)",
                             "Strong Conservative\n(Trump won by > 20%)"))

# Specify which value corresponds to which level
agg_poli$x <- as.numeric(c(agg_poli[6, 2], agg_poli[2, 2], agg_poli[4, 2], 
                           agg_poli[3, 2], agg_poli[1, 2], agg_poli[5, 2]))
# Create extra column with factor levels in desired order
agg_poli$ordered <- 
  factor(agg_poli$Group.1, c("Strong Liberal\n(Clinton won by > 20%)",
                             "Liberal\n(Clinton won by 10% - 19%)",
                             "Slight Liberal\n(Clinton won by 0% - 9%)",
                             "Slight Conservative\n(Trump won by 0% - 9%)",
                             "Conservative\n(Trump won by 10% - 19%)",
                             "Strong Conservative\n(Trump won by > 20%)"))

# Set color parameters for use in barplot
bar_colors <- c(rgb(0, 0, 204/255, 1), rgb(0, 0, 204/255, 0.75), 
                rgb(0, 0, 204/255, 0.5), 
                rgb(204/255, 0, 0, 0.5), rgb(204/255, 0, 0, 0.75), 
                rgb(204/255, 0, 0, 1))

# Create barplot which clearly visualizes discrepancy in confirmed cases 
# depending on states' political leanings
ggplot(agg_poli, aes(x = ordered, y = x)) +
  geom_bar(stat = "identity", width = 0.5, fill = bar_colors) + 
  labs(title = "Confirmed Cases by States' Political Affiliation", 
       x = "2016 Presidential Election Results", 
       y = "Confirmed Cases Per 100K People, Per State") +
  coord_flip()

# Save politics as a factor
table(current$politics) # six levels
current$politics <- factor(tolower(current$politics)) 
levels(current$politics) <- c("Strong Liberal\n(Clinton won by > 20%)",
                              "Liberal\n(Clinton won by 10% - 19%)",
                              "Slight Liberal\n(Clinton won by 0% - 9%)",
                              "Slight Conservative\n(Trump won by 0% - 9%)",
                              "Conservative\n(Trump won by 10% - 19%)",
                              "Strong Conservative\n(Trump won by > 20%)")

#### BUBBLE PLOT

# Create new dataframe containing all categorical assessments of social
# distancing procedures implemented in each state
dist <- current[, c("status.of.reopening", "stay.at.home.order",
                    "non.essential.business.closures", "large.gatherings.ban",
                    "large.gatherings.ban", "restaurant.limits", 
                    "bar.closures", "face.covering.requirement")]

# At present, levels of each factor are descriptors. Convert these levels to 
# numbers, where higher numbers represent more severe restrictions for a given 
# type of social distancing practice
levels(dist$status.of.reopening) <- c(3, 2, 1)
levels(dist$stay.at.home.order) <- c(0, 1, 2, 3)
levels(dist$non.essential.business.closures) <- c(0, 1, 3, 4, 2, 5)
levels(dist$large.gatherings.ban) <- c(0, 4, 5, 2, 1, 3)
levels(dist$restaurant.limits) <- c(0, 2, 1, 3)
levels(dist$bar.closures) <- c(3, 2, 1)
levels(dist$face.covering.requirement) <- c(0, 1, 2, 3)

# Convert these numerical values to actual numbers
for (i in 1:ncol(dist)) {dist[, i] <- as.numeric(dist[, i])}

# Assign each state a 'score' based on the amalgamation of all the above social
# distancing procedures. The score represents the raw total of how many points
# each state earned for their social distancing measures. A higher score
# indicates stricter public health and safety protocols to mitigate the spread
# of the virus. Scores are squared to provide more spread
current$score <- rowSums(dist) ** 2

# Standardize score to a 0-10 scale
current$score <- 10 * (current$score - min(current$score)) / 
  (max(current$score) - min(current$score))

# Create small subset of data for later use in labeling bubble plot
label_subset <- subset(current, (positive_per_100k < 500 | score < 1))
label_subset$state <- setNames(state.name, state.abb)[label_subset$state]

# Create bubble plot of per capita values for positive tests and ignorance of
# mask-wearing precautions, colored by party, where larger circular area 
# represents stricter public health mandates 
ggplot(current, aes(x = never_per_100k, y = positive_per_100k, size = score, 
                    color = party)) + 
  geom_point(alpha = 0.5) +
  scale_size(range = c(1, 12), 
             name = "Severity of State-Implemented
             Social Distancing Restrictions") + 
  scale_color_manual(values = c(rgb(0/255, 17/255, 250/255), 
                                rgb(161/255, 0/255, 0/255))) +
  xlab("Population That 'Never' Wears Mask, Per 100k") + 
  ylab("Positive Tests, Per 100k") +
  geom_text_repel(data = label_subset[label_subset$state == "Maine", ], 
                  label = "Maine", 
                  size = 4, 
                  direction = "x", 
                  nudge_x = 1600, 
                  show.legend = FALSE) + 
  geom_text_repel(data = label_subset[label_subset$state == "Vermont", ], 
                  label = "Vermont", 
                  size = 4, 
                  nudge_x = -2000, 
                  show.legend = FALSE) +
  geom_text_repel(data = label_subset[label_subset$state == "North Dakota", ], 
                  label = "North Dakota", 
                  size = 4, 
                  direction = "x", 
                  nudge_y = 200, 
                  show.legend = FALSE) + 
  geom_text_repel(data = label_subset[label_subset$state == "South Dakota", ], 
                  label = "South Dakota", 
                  size = 4, 
                  direction = "x", 
                  nudge_y = -150, 
                  show.legend = FALSE)

# PCA on states, standardized data, only numerical variables
current_st.num <- current_standardized[,-c(26:27, 39:55)]

# table of correlations
round(cor(current_st.num[,-1]), 3)

# box plots to check for univariate normality
par(mfrow = c(4,2))
for (i in 2:ncol(current_st.num)) {
  boxplot(current_st.num[,i], main = colnames(current_st.num)[i])
}

# qq plots to check for univariate normality
par(mfrow = c(3,2))
for (i in 2:ncol(current_st.num)) {
  qqnorm(current_st.num[, i], main = colnames(current_st.num)[i])
  qqline(current_st.num[, i])
}

# perform PCA on transformed data
current.un <- current_st.num[, c(1:8, 11:13, 20:23)]
current.tr <- current_st.num[, c(1, 37:58)]

# check for multivariate normality with a Chi-squared plot on 
# the transformed and untransformed data

# get online function
source("http://www.reuningscherer.net/STAT660/R/CSQPlot.r.txt")

# run the function
par(mfrow = c(1,1))
# untransformed - not multivariate normal at all
CSQPlot(current.un[, -1], label = "Current Covid Data by State (Untransformed)")
# transformed - plot indicates data has mutlivarariate normal distribution 
# (forms a line)
CSQPlot(current.tr[, -1], label = "Current Covid Data by State (Transformed)")

# we now perform PCA
pc1 <- princomp(current.tr[, -1], cor = TRUE)

#print results
print(summary(pc1), digits = 2, loadings = pc1$loadings, cutoff=0)

# num of variables: 23 variables, including state
ncol(current.tr)

# eigenvalues
v <- round(pc1$sdev^2,2)
v

# How many PCs to use? 
sum(v > 1) # only the first 3 PCs have eigenvalues > 1

# we want to explain >80% of the variance. since we 
# standardized, the variance of each column is 1
.8*(ncol(current.tr)-1) 

sum(v[1:2]) # with just the first 2 PCs we can explain > 80% variance
v[1]/(ncol(current.tr)-1) # the 1st PC explains 68% of var
sum(v[1:2])/(ncol(current.tr)-1) # first 2 PC explains .828

# finally, another way to find # of PCs to use is screeplot
screeplot(pc1, type="lines", col = "red", lwd = 2, pch = 19, cex = 1,
          main="Scree Plot of Current COVID State Data")

# There appears to be an elbow at 2, 3 so 1 or 2 PCs may be 
# best number of PCs to retain. 

# Based on these 3 assessments, 2 PCs seem best.
# Let's look at what composes these 2 PCs
# Recall the eigenvalues of the first 2 PCs
v[1:2]

# PC 1
sort(pc1$loadings[,1], decreasing = TRUE)
# The first component seems to be heavily influenced by 
# log_population, log_num_beds, log_frequently, log_num_colleges,
# log_always, log_negative, log_deaths, log_people_tested, 
# log_totaltestresults, log_confirmed, log_positive, 
# log_sometimes, log_num_hospitals, log_never, 
# log_active, log_rarely. We can think of PC1 as a state's
# "covid-precautions given density" (testing, mask wearing, 
# density as measured by population and num of colleges).

# PC2
sort(pc1$loadings[,2], decreasing = TRUE)
# The second component seems to be heavily influenced by 
# log_state_area_sqmiles, log_hospitals_per_100k,
# log_num_hospitals, log_beds_per_100k. We can think of PC2 as a 
# measure of "health care system capacity."

# visualization of 95% confidence ellipse for 1st two components
# get function online
source("http://reuningscherer.net/stat660/r/ciscoreplot.R.txt")

#run the function
ciscoreplot(pc1, c(1,2), current.tr[,1]) # good fit

############# MODELING STARTS HERE #################
# MERGE TIME SERIES DATA with CURRENT DATA
current_static <- current[, -c(2:10, 56:63, 78:84)]
covid.full <- merge(covid, current_static, by = "state")

# check for negative or zero values in the data (i.e. this will mess up 
# logs). Negative values are illogical, and need to be fixed, while zero 
# values could be rational but need to be adjusted for logging purposes.
colSums(covid.full[, c("confirmed", "deaths", "active", 
                       "people_tested", "negative", "positive", 
                       "totaltestresults", "mortality_rate",
                       "incident_rate")] < 0)

# There is 1 negative value for active

# ACTIVE: We take a look at the data for ACTIVE cases and find that the 
# negative value is completely off - assume this is an error in the data, and 
# make value equal to latest day (this is reasonable, given the numbers 
# did not change much day to day. For example, the day before and the day prior
# are the same value)
covid.full$active[which(covid.full$active<0)] <- 
  covid.full$active[which(covid.full$active<0)-1]

# check if fixed now
if(sum(covid.full$active < 0) > 0) warning("Negative values in ACTIVE cases")

colSums(covid.full[, c("confirmed", "deaths", "active", 
                       "people_tested", "negative", "positive", 
                       "totaltestresults", "mortality_rate",
                       "incident_rate")] == 0)

# There is 1 zero value for deaths, and hence for mortality rate (which
# is calculated off of deaths)

# DEATHS: this is because on 4/12, there were still 0 deaths in WY, a 
# relatively remote state. We want to keep 0 for deaths, but want to transform 
# it for logging.

# plot a histogram of deaths to see what transformation might be appropriate
hist(covid.full$deaths)

# Create new columns with log transformations
log_cols <- covid.full[, c("confirmed", "deaths", "active", 
                           "people_tested", "negative", "positive", 
                           "totaltestresults")]

# LOG TRANSFORMS
log_cols$deaths <- log_cols$deaths + 0.00000001
colnames(log_cols) <- paste0("log_", colnames(log_cols))
log_cols <- log(log_cols)

# PER 100K TRANSFORMS
capita_cols <- covid.full[, c("confirmed", "deaths", "active", "people_tested", 
                              "negative", "positive", "totaltestresults")]
colnames(capita_cols) <- paste0(colnames(capita_cols), "_per_100k")
capita_cols <- round(((100000 * capita_cols) / covid.full$population), 2)

# cbind together the transformations
covid.full <- cbind(covid.full, log_cols, capita_cols)

# check that cbind worked
if(nrow(log_cols) != nrow(covid.full)) warning("Lost rows in merge")

# Time Series Visualization
# Use facetting:
# Reformat data, change it from a wide format with each variable in its own column 
# to a long format where you use one column for your measures, another for a key 
# variable telling us which measure we use in each row

covid_long <- gather(covid.full, key="measure", value="value", c("state"))
covid_long$date <- as.Date(covid_long$date)

# confirmed per 100k
confirmed.t <- ggplot(covid_long, aes(x = date, y = confirmed_per_100k)) + 
  ggtitle("Confirmed per 100k in Each State During Pandemic") +
  geom_point(color = "steelblue") +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b")) + 
  facet_wrap(~ value, ncol = 5)
confirmed.t

# deaths per 100k
deaths.t <- ggplot(covid_long, aes(x = date, y = deaths_per_100k)) +
  ggtitle("Deaths per 100k in Each State During Pandemic") +
  geom_point(color = "steelblue") +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b")) + 
  facet_wrap(~ value, ncol = 5)
deaths.t

# people_tested per 100k
people_tested.t <- ggplot(covid_long, aes(x = date, y = people_tested_per_100k)) + 
  ggtitle("People Tested per 100k in Each State During Pandemic") + 
  geom_point(color = "steelblue") +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b")) + 
  facet_wrap(~ value, ncol = 5)
people_tested.t

# MAPS VISUALIZATION
plot_usmap(data = current, values = "confirmed_per_100k", color = "red") + 
  labs(title = "Confirmed Cases across the United States") + 
  scale_fill_continuous(low = "white", high = "red", name = "Confirmed Per 100k", 
                        label = scales::comma) + 
  theme(legend.position = "right")

plot_usmap(data = current, values = "deaths_per_100k", color = "red") + 
  labs(title = "COVID-related Deaths across the United States") + 
  scale_fill_continuous(low = "white", high = "red", name = "Deaths Per 100k", 
                        label = scales::comma) + 
  theme(legend.position = "right")

plot_usmap(data = current, values = "people_tested_per_100k", color = "blue") + 
  labs(title = "Number of People Tested across the United States") + 
  scale_fill_continuous(low = "white", high = "blue", name = "People Tested Per 100k", 
                        label = scales::comma) + 
  theme(legend.position = "right")

us_states <- map_data("state") # grab the geographic data
party_colors <- c("#2E74C0", "#CB454A") # dem blue and rep red

# order by ascending
ordered.current <- current[order(current$deaths_per_100k),]
ordered.current <- mutate(ordered.current, 
                          mytext = 
                            paste("State: ", state, "\n", 
                                  "Confirmed per 100k: ", confirmed_per_100k, 
                                  "\n",
                                  "Density: ", density, "\n",
                                  "Status of Reopening: ", status.of.reopening, 
                                  "\n", 
                                  "Masks Required: ", masks.required., "\n"))

# merge data onto us_states
us_states$state <- setNames(state.abb, tolower(state.name))[us_states$region]
us_covid <- merge(us_states, ordered.current, by=c("state"))

# Interactive chlorpleth map
p <- ggplot() + geom_polygon(data = us_states, aes(x = long, y = lat, group = group), 
                             alpha = 0.3, fill = "orange") + 
  geom_point(data = us_covid, 
             aes(x = long, y = lat, size = deaths_per_100k, color = party, 
                 text = mytext,
                 alpha = deaths_per_100k), shape = 20, stroke = FALSE) + 
  scale_alpha_continuous(name="Deaths per 100k", range=c(0.1, .4)) +
  scale_color_manual(values = party_colors) +
  theme_void() + coord_map() + 
  guides( colour = guide_legend()) +
  ggtitle("Deaths per 100k in the United States") +
  theme(
    legend.position = c(0.85, 0.8),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", 
                              margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )

p <- ggplotly(p, tooltip = "text")
p

################# MODELING STARTS HERE ###########################
# STORY
# Predicting log(deaths)
# 
# bubble plot: party / politics / pct_diff is important, "never wear masks" 
# is positively correlated with positive tests
# bar plot: politics matter
# radar plot: 1. density 2. people_tested_per_100k 3. hospitals_per_100k + 
# beds_per_100k 4. mask wearing: frequently_per_100k + always_per_100k 
# boxplots: regions matter
#
# Hypothesis: 
# num_colleges
# per_100k more insightful

# NEW VARIABLES CREATED HERE
# Variable: Frequently + always new variable
covid.full$frequently_more_per_100k <- covid.full$frequently_per_100k + 
  covid.full$always_per_100k

# Variable: number of days since start 
covid.full$date <- as.Date(covid.full$date)
covid.full$length <- covid.full$date - min(covid.full$date)

if(sum(covid.full$length < 0) != 0) 
  warning("Length not calculated correctly")

# # remove remaining NAs from data
# covid.full <- covid.full[-is.na(covid.full),]

# LAGGED DATA
lagged_cols <- covid.full[, c(3:11, 79:92)]
lagged2_cols <- covid.full[, c(3:11, 79:92)]
lagged7_cols <- covid.full[, c(3:11, 79:92)]
lagged14_cols <- covid.full[, c(3:11, 79:92)]

# LAGGED TRANSFORMS
colnames(lagged_cols) <- paste0("lagged_", colnames(lagged_cols))
colnames(lagged2_cols) <- paste0("lagged2_", colnames(lagged2_cols))
colnames(lagged7_cols) <- paste0("lagged7_", colnames(lagged7_cols))
colnames(lagged14_cols) <- paste0("lagged14_", colnames(lagged14_cols))

lagged_cols <- rbind(rep(NA, ncol(lagged_cols)), lagged_cols[-nrow(lagged_cols), ])
lagged2_cols[-c(1:2),] <- lagged2_cols[-((nrow(lagged2_cols) - 1):nrow(lagged2_cols)), ]
lagged2_cols[1:2,] <- NA
lagged7_cols[-c(1:7),] <- lagged7_cols[-((nrow(lagged7_cols) - 6):nrow(lagged7_cols)), ]
lagged7_cols[1:7,] <- NA
lagged14_cols[-c(1:14),] <- lagged14_cols[-((nrow(lagged14_cols) - 13):nrow(lagged14_cols)), ]
lagged14_cols[1:14,] <- NA

# cbind together the transformations
covid.lags <- cbind(covid.full, lagged_cols)
covid.lags2 <-  cbind(covid.full, lagged_cols, lagged2_cols, lagged7_cols, lagged14_cols)

# SPLIT DATA INTO TRAINING DATA AND TESTING DATA
covid.train <- 
  covid.lags[covid.lags$date <= max(as.Date(covid.lags$date)) - 14, ]

covid.test <- 
  covid.lags[covid.lags$date > max(as.Date(covid.lags$date)) - 14, ]

if(length(unique(covid.test$date)) != 14) warning("Error in test data")
if(length(unique(covid.lags$date)) - length(unique(covid.train$date)) != 14) 
  warning("Error in training data")


###################### MODEL ##################################
# One model to fit all states
library(tidyverse)
library(caret)
library(leaps)
library(MASS)

# Include only lagged logs and constant data + log_deaths, the outcome var
covid.obs <- covid.train[, c(1, 12, 18:19, 23, 28, 40:41, 51, 55, 64:68,
                             80:81, 104:110)]
names(covid.obs) # variables of analysis

# Fit the full model first
full.model <- lm(log_deaths ~ ., data = covid.obs)
summary(full.model)
# Notes about stepAIC: 
# forward and stepwise selection can be applied in high-dimensional 
# configurations (i.e. n can be inferior in size to number of 
# predictors p). In other words, forward and stepwise is more forgiving.
# But backward selection only works if sample size n is much larger
# than number of variables p so that the full model can be fit.
# Here, we have many observations so we can use any method.

# We use AIC for model selection because it rewards goodness of fit
# as assessed by teh likelihood function but also penalizes as 
# function of the number of estimated paramters, which discourages
# overfitting. (i.e. increasing the number of parameters in a 
# model almost always improves goodness of fit, i.e. because
# it's more information)

# AIC = 2k - 2ln(L)

# "Both" selection
step.both <- stepAIC(full.model, direction = "both", trace = FALSE)
s.both <- summary(step.both)
s.both

# "Backward" selection
step.back <- stepAIC(full.model, direction = "backward", trace = FALSE)
s.back <- summary(step.back)
s.back

# "Forward" selection - doesn't work well (don't include in write-up)
min.model <- lm(log_deaths ~ 1, data = covid.obs)
step.fwd <- stepAIC(min.model, direction = "forward")
s.fwd <- summary(step.fwd)
s.fwd 

# State is overwhelmingly the most important factor - what if 
# we don't have state?

# Include only lagged logs and constant data + log_deaths, the outcome var
# Fit the full model first
full.model <- 
  lm(log_deaths ~ ., 
     data = covid.train[, c(12, 18:19, 23, 28, 40:41, 51, 55, 64:68, 
                            80:81, 104:110)])

# "Both" selection
nostate.both <- stepAIC(full.model, direction = "both", trace = FALSE)
s.both <- summary(nostate.both)
s.both

# explanatory power
s.both$coefficients[order(abs(s.both$coefficients[, 1]), decreasing = TRUE), ]

# "Backward" selection
nostate.back <- stepAIC(full.model, direction = "backward", trace = FALSE)
s.back <- summary(nostate.back)
s.back

# helper function to compute rmse on matrices
rmse <- function(pred, obs){
  sqrt(mean(unlist((pred-obs)^2)))
}

mae <- function(pred, obs){
  mean(unlist(abs(pred-obs)))
}

# Assume that all parameters hold constant, except length (which
# increments by one each day) and lagged_log_deaths is updated
# with predicted values
evaluate_model_const <- function(model){
  current.train <- covid.train[covid.train$date == max(covid.train$date), ]
  
  # Predictions for 14 days, 50 states
  aic.pred <- data.frame(matrix(ncol = 14, nrow = 50))
  colnames(aic.pred) <- seq(max(covid.train$date) + 1, 
                            max(covid.train$date) + 14, by = 1)
  for(i in 1:14){
    current.train$length <-  current.train$length + 1
    current.train$lagged_log_deaths <- current.train$log_deaths
    aic.pred[, i] <- predict(model, current.train) # * place of change * 
    current.train$log_deaths <- aic.pred[, i]
  }
  
  # Reformat the observed data to be compatible with the predictions
  aic.obs <- matrix(covid.test$log_deaths, ncol = 14, byrow = TRUE)
  colnames(aic.obs) <- colnames(aic.pred)
  
  # Return different metrics of evaluating the model
  l <- list("predicted.values" = aic.pred, 
            "true.values" = aic.obs,
            "residuals" = aic.pred - aic.obs, 
            "RMSE" = rmse(aic.pred, aic.obs), 
            "MAE" = mae(aic.pred, aic.obs),
            "r.squared" = summary(model)$r.squared,
            "adj.r.squared" = summary(model)$adj.r.squared,
            "anova" = model$anova, 
            "AIC" = model$anova$AIC[1])
  
  return(l)
} 

# Evaluate our stepwise regression models
results.both <- evaluate_model_const(step.both) # sequential
evaluate_model_const(step.back) # backwards
evaluate_model_const(step.fwd) # forwards (just intercept)

# Evaluate our stepwise regression models with no state
evaluate_model_const(nostate.both) # sequential
evaluate_model_const(nostate.back) # backwards
# better fit with no state (lower AIC)

evaluate_model_const(step.both)$RMSE

# Visualize - pretty good fit here (looking for 1-to-1)
par(mfrow = c(2,2))
for (i in 1:ncol(results.both$predicted.values)) {
  plot(results.both$predicted.values[, i], results.both$true.values[, i], 
       main = colnames(results.both$predicted.values)[i])
}

# Assume that all parameters move based on simple AR, except length (which
# increments by one each day) and lagged_log_deaths is updated
# with predicted values

current.train <- covid.train[covid.train$date == max(covid.train$date), ]

log_cols <- grep("lagged_log", names(covid.train))

covid.short <- covid.train[covid.train$date > (max(covid.train$date) - 90), ]
cv.short <- split(covid.short, covid.short$state)

beta_coefficients <-
  data.frame(matrix(NA, nrow = 50, ncol = length(log_cols)))
names(beta_coefficients) <- names(covid.short[, log_cols])
rownames(beta_coefficients) <- names(cv.short)

intercepts <- 
  data.frame(matrix(NA, nrow = 50, ncol = length(log_cols)))
names(intercepts) <- names(covid.short[, log_cols])
rownames(intercepts) <- names(cv.short)

for(i in 1:length(cv.short)){
  for(j in 1:length(log_cols)){
    ar1 <- ar.ols(cv.short[[i]][(log_cols[j])], order.max = 1, demean = FALSE, 
                  intercept = TRUE)
    if(ar1$order == 0){
      beta_coefficients[i,j] <- NA
      intercepts[i, j] <- NA
    }
    else{
      beta_coefficients[i,j] <- ar1$ar[1]
      intercepts[i, j] <- ar1$x.intercept
    }
  }
}

# predict the next days using the AR models from above
nextday_predict <- function(df, beta_coefficients, intercepts){
  nextday <- df[, log_cols] * colMeans(beta_coefficients, na.rm = TRUE) + 
    colMeans(intercepts, na.rm = TRUE)
}

evaluate_model_ar <- function(model, current.df){
  
  # Predictions for 14 days, 50 states
  aic.pred <- data.frame(matrix(ncol = 14, nrow = 50))
  colnames(aic.pred) <- seq(max(covid.train$date) + 1, 
                            max(covid.train$date) + 14, by = 1)
  for(i in 1:14){
    current.train$length <-  current.train$length + 1
    current.train$lagged_log_deaths <- current.train$log_deaths
    aic.pred[, i] <- predict(model, current.train) # * place of change * 
    
    # update
    current.train$log_deaths <- aic.pred[, i]
    current.train[, log_cols] <- 
      nextday_predict(current.train, beta_coefficients, intercepts)
  }
  
  # Reformat the observed data to be compatible with the predictions
  aic.obs <- matrix(covid.test$log_deaths, ncol = 14, byrow = TRUE)
  colnames(aic.obs) <- colnames(aic.pred)
  
  # Return different metrics of evaluating the model
  l <- list("predicted.values" = aic.pred, 
            "true.values" = aic.obs,
            "residuals" = aic.pred - aic.obs, 
            "RMSE" = rmse(aic.pred, aic.obs), 
            "MAE" = mae(aic.pred, aic.obs),
            "r.squared" = summary(model)$r.squared,
            "adj.r.squared" = summary(model)$adj.r.squared,
            "anova" = model$anova, 
            "AIC" = model$anova$AIC[1])
  
  return(l)
} 

# Evaluate on training data
current.train <- covid.train[covid.train$date == max(covid.train$date), ]
evaluate_model_ar(step.both, current.train)$RMSE
evaluate_model_const(step.both)$RMSE

evaluate_model_ar(step.both, current.train)$MAE
evaluate_model_const(step.both)$MAE

# no state
evaluate_model_ar(nostate.both, current.train)$RMSE
evaluate_model_const(nostate.both)$RMSE

# Predict with Constant and States because it is better on RMSE and MAE
my_rolling_predictions_const <- function(model, current.df){
  aic.pred <- data.frame(matrix(ncol = 14, nrow = 50))
  names(aic.pred) <- seq(max(current.df$date) + 1, 
                            max(current.df$date) + 14, by = 1)
  rownames(aic.pred) <- unique(current.df$state)
  
  for(i in 1:14){
    current.df$length <-  current.df$length + 1
    current.df$lagged_log_deaths <- current.df$log_deaths
    aic.pred[, i] <- predict(model, current.df) # * place of change * 
    
    # update
    current.df$log_deaths <- aic.pred[, i]
  }
  
  return(list("predictions" = aic.pred))
}

# predict based off the latest day - in our data, Oct 25
current.df <- covid.lags[covid.lags$date == max(covid.lags$date), ]

# ANSWER: Predictions for next 14 days
nov_pred <- data.frame(my_rolling_predictions_const(step.both, current.df))
names(nov_pred) <- seq(max(current.df$date) + 1, 
                          max(current.df$date) + 14, by = 1)

# Save predictions as a .csv
predloc <- 'predictions'    # directory
if (!dir.exists(predloc)) dir.create(predloc)
write.csv(nov_pred, file.path(predloc, "log_deaths_predictions_by_state.csv"))

# Transform into deaths - undo transformation from before
nov_deaths <- round(exp(nov_pred) - 0.00000001)
write.csv(nov_deaths, file.path(predloc, "deaths_predictions_by_state.csv"))

# These models are pretty good! Now, let's try some time 
# series models: MA, MA with exponential smoothing, AR, VAR
# this is time series data. 

