# Clean the script; import packages
rm(list = ls())
library(ggplot2)
library(ggpubr)
library(leaflet)
library(gridExtra)
library(gpairs)
library(grid)
library(knitr) # knit to PDF
library(fmsb)
library(forcats)
library(ggrepel)

# Read in data (source: Atlantic)
covid <- read.csv("all-states-history.csv", as.is = TRUE, 
                  stringsAsFactors = FALSE)

# Dimensions: 13,213 observations, 43 variables 
dim(covid)

# Standardize date
covid$date <- as.Date(covid$date, "%m/%d/%y")
# Did date convert correctly? 
if (sum(is.na(covid$date) > 0)) warning("The date has NAs!")

# We abbreviate the data to 4/12/20 to create a more complete data 
# set across various sources
covid <- covid[covid$date >= "2020-04-12", ]

# Dimensions: 11,032 observations, 43 variables 
dim(covid)

# Summary of data
summary(covid)

# Keep only the 50 states
covid$state <- as.character(covid$state)
covid <- covid[!(covid$state %in% c("VI", "GU", "AS", "PR", "MP", "DC")), ]

# Check for NAs and remove unneccessary and incomplete data
# We only keep the 50 states (remove territories). We remove 
# variables with more than 1000 NAs
colSums(is.na(covid))
covid <- covid[, colSums(is.na(covid)) < 1000]

# Re-examine the data: We now have 9,850 observations, 17 relatively 
# complete variables
dim(covid) 

# Variables 
colnames(covid)

# We remove postiveScore (not relevant)
covid <- covid[, -12]

# Re-summarize the COVID data set
summary(covid)

# Order by state and date
covid <- covid[order(covid$state, covid$date), ]

# Visualize the data
head(covid)

# Get file list (source: Johns Hopkins University)
fileloc <- 'raw_files'
file_list <- list.files(path = fileloc)

# Read all .csv files in the folder and create a list of dataframes
ldf <- lapply(file.path(fileloc, file_list), read.csv, as.is = TRUE, 
              stringsAsFactors = FALSE)

# Combine each dataframe in the list into a single dataframe
x <- do.call("rbind", ldf)

# Save as a master raw .csv file in working directory
write.csv(x, "master_raw.csv")

# Take a look at the data 
head(x)

# Rename the columns
colnames(x)[1:5] <- c("State", "Country", "Date", "Lat", "Long")
colnames(x) <- tolower(colnames(x))

# Remove the territories (including District of Columbia)
y <- x[-which(x$state %in% c("Diamond Princess", "District of Columbia",
                             "Grand Princess", "Guam", 
                             "Puerto Rico", "American Samoa", 
                             "Northern Mariana Islands", "Recovered",  
                             "Virgin Islands")), ]

# Standardize state names with other data set (abbreviations)
y$state <- setNames(state.abb, state.name)[y$state]

# Standardize the date
y$date <- as.Date(y$date)

# Check for NAs
colSums(is.na(y)) # amount of NAs
colSums(is.na(y)) / nrow(y) # percent NAs

# We want to take out People_Hospitalized and Hospitalization_Rate 
# since nearly 50% of the column is NAs; keep Recovered for now and 
# see if there is a way to impute some of the NAs
z <- y[, -which(colSums(is.na(y)) / nrow(y) > 0.40)]
z <- z[, -2] # remove country
summary(z)

# Variables for analysis
colnames(z)

# Prepare Atlantic data to combine with John Hopkins data
colnames(covid) <- tolower(colnames(covid))
covid <- covid[, c(2, 1, 3:ncol(covid))]

# Compare the data sets
if (covid$date[nrow(covid)] + 1 != z$date[nrow(z)]) {
  warning("Data come from different dates! Atlantic: ", covid$date[nrow(covid)], 
          " Johns Hopkins: ", z$date[nrow(z)])
} 

#### MERGE the data
covid.full <- merge(z, covid, by = c("state", "date"))

# Run some checks before proceeding
if(sum(is.na(covid.full$date)) > 0) warning ("Uh oh! Something went wrong 
                                        with the dates")
if(sum(is.na(covid.full$state)) > 0) warning ("Uh oh! Something went wrong 
                                        with the states")
if(ncol(covid.full) != ncol(covid) + ncol(z) - 2) warning ("Uh oh! Dropped
                                        some columns from data during merge")
# Check for NAs and report them
colSums(is.na(covid.full))[colSums(is.na(covid.full)) > 0]

covid.full <- covid.full[, c("state", "date", "confirmed", "deaths", 
                             "active", "people_tested", "negative", "positive", 
                             "totaltestresults")]

# create own variables
covid.full$mortality_rate <- covid.full$deaths / covid.full$confirmed
covid.full$incident_rate <- covid.full$confirmed / covid.full$people_tested

if(sum(is.na(covid.full$mortality_rate)) > 0) warning ("Nas in mortality")
if(sum(is.na(covid.full$incident_rate)) > 0) warning ("Nas in incident rate")

# Save as a .csv
cleanloc <- 'cleanfiles'    # directory
if (!dir.exists(cleanloc)) dir.create(cleanloc)
write.csv(covid.full, file.path(cleanloc, "covid.full.csv"))

# Covid.full is our panel (time series) data for each state and day since 
# 4/12/2020
# Covid.latest is the current picture -- the latest totals for all columns
covid.latest <- covid.full[covid.full$date == max(covid.full$date), ]

#### COLLEGE DATA

# Read in data, only keeping the columns we know we will need: state, number of
# undergrads, cost (if public), cost (if private).

# Full dataset contains nearly 2,000 columns and takes ~15 seconds to read in
colleges <- read.csv("colleges.csv", as.is = TRUE, stringsAsFactors = FALSE, 
                     colClasses = c(rep("NULL", 5), NA, rep("NULL", 284), NA, 
                                    rep("NULL", 25), NA, NA, rep("NULL", 1668)))

dim(colleges)

# Eliminate schools with no entries for state or undergrad population
colleges <- colleges[colleges$UGDS != "NULL", ]
colleges <- colleges[colleges$STABBR != "NULL", ]

# Convert costs to numeric
colleges$NPT4_PUB <- as.numeric(colleges$NPT4_PUB)
colleges$NPT4_PRIV <- as.numeric(colleges$NPT4_PRIV)

# Eliminate entries without a cost listed
colleges <- colleges[!((is.na(colleges$NPT4_PUB)) & 
                         (is.na(colleges$NPT4_PRIV))) , ]

# Convert remaining NA's to zeros. Since a college cannot be both private and
# public, all schools will have an NA for one of these two columns
colleges[is.na(colleges$NPT4_PUB), 3] <- 0
colleges[is.na(colleges$NPT4_PRIV), 4] <- 0

# With one column now as simply a zero, sum the costs to generate a new column
# for each school's cost, regardless of public or private status
colleges$cost <- colleges$NPT4_PUB + colleges$NPT4_PRIV
head(colleges)

# Eliminate previous cost columns
colleges <- colleges[, -c(3, 4)]

# Convert undergrad population to numeric
colleges$UGDS <- as.numeric(colleges$UGDS)
summary(colleges)

# Check negative cost values; eliminate them
sort(colleges$cost[1:50], decreasing = FALSE)
colleges <- colleges[colleges$cost > 0, ]

# Change column names
colnames(colleges) <- c("state", "students", "cost")

# Restrict data to only 50 states
colleges <- colleges[!(colleges$state %in% c("AS", "DC", "FM", "GU", "MH", "MP", 
                                             "PR", "VI", "PW")), ]

# Convert states to factors and split dataframe by state
colleges$state <- factor(colleges$state)
bystate <- split(colleges[, 3], colleges$state)

# The number of colleges is the length of each state's entry in the split 
# dataframe, since each row represents an individual college
num_colleges <- lengths(bystate)

# Find average cost of schools in each state
avg_cost <- unlist(lapply(bystate, mean))

# Create and populate matrix with number and avg. cost of colleges by state
college.data <- matrix(NA, nrow = 50, ncol = 3, byrow = FALSE)
college.data[, 1] <- levels(colleges$state)
college.data[, 2] <- num_colleges
college.data[, 3] <- avg_cost

# Convert to data frame
college.data <- as.data.frame(college.data, stringsAsFactors = FALSE)
names(college.data) <- c("state", "num_colleges", "avg_cost") 
college.data$num_colleges <- as.numeric(college.data$num_colleges)
college.data$avg_cost <- round(as.numeric(college.data$avg_cost), 2)


#### ELECTION 

# Read in
election <- read.csv("president.csv", as.is = TRUE, stringsAsFactors = FALSE)

# Restrict only to 2016 election
election <- election[election$year == 2016, ]

# Check that we're only considering presidential race
unique(election$office)

# Eliminate other parties
election <- election[(election$party == "democrat") | 
                       (election$party == "republican") , ]

# Eliminate any candidate besides Clinton and Trump
election <- election[election$writein == FALSE, ]
table(election$candidate)

# Keep only state, party, candidate votes, and total votes columns
election <- election[, c(3, 9, 11, 12)]

# Change column name; restrict to 50 states since we aren't considering D.C.
colnames(election)[1] <- "state"
election <- election[election$state != "DC", ]

# Find percentage of total vote each candidate won in each state
election$percent <- round(election$candidatevotes / election$totalvotes, 4)

# Order data by state and by party, so that Clinton's totals appear first
# uniformly across all state entries
election <- election[order(election$state, election$party), ]

# Convert states to factor; split dataframe by state
election$state <- factor(election$state)
breakdown <- split(election, election$state)

# Define vectors to hold differences in voting totals and percentages
vote_diff <- rep(NA, length(breakdown))
pct_diff <- rep(NA, length(breakdown))

# For each state, calculate difference between total Clinton and Trump votes,
# as well as the percentage of voters who chose each. 

# If negative, Trump won state
for (i in 1:length(breakdown)) {
  state <- as.data.frame(breakdown[i])
  vote_diff[i] <- (state[1, 3] - state[2, 3])
  pct_diff[i] <- (state[1, 5] - state[2, 5])
}

# Create and populate dataframe
voting <- as.data.frame(matrix(data = NA, nrow = 50, ncol = 4))
voting[, 1] <- unique(election$state)
voting[, 2] <- vote_diff
voting[, 3] <- pct_diff * 100

# Assign political affiliation of each state. Negative values indicate
# Republican-won states
voting[, 4] <- "D"
voting[voting[, 3] < 0, 4] <- "R"

# Change column names
names(voting) <- c("state", "vote_diff", "pct_diff", "party")

# Add binary dummy variable for party association
voting$republican <- as.numeric(voting$party == "R")

#### DENSITY
pop <- read.csv("density.csv", as.is = TRUE, stringsAsFactors = FALSE)

# Convert col names to lower case
colnames(pop) <- tolower(colnames(pop))

# Change state names to abbreviations
pop$state <- setNames(state.abb, state.name)[pop$state]

# Omit NA entries for state names -- these comprise Washington D.C. and Puerto
# Rico, neither of which we are considering in this project
pop <- na.omit(pop)

# Order by state, not density
pop <- pop[order(pop$state), ]


#### HOSPITALS


# Read in data
hospitals <- read.csv("Hospitals.csv", as.is = TRUE, stringsAsFactors = FALSE)
colnames(hospitals) <- tolower(colnames(hospitals))

# Check that data is included from each state
length(table(hospitals$state))

# Keep only state, type, and number of beds
hospitals <- hospitals[, c(8, 12, 32)]

# Eliminate hospitals outside 50 states
hospitals <- hospitals[!(hospitals$state %in% c("AS", "DC", "FM", "GU", "MH", 
                                                "MP", "PR", "VI", "PW")), ]
if(length(unique(hospitals$state)) != 50) 
  warning("Contains more than 50 states")

table(hospitals$type)
# Restrict to Critical Access and General Acute Care types
hospitals <- hospitals[(hospitals$type == "GENERAL ACUTE CARE") | 
                         (hospitals$type == "CRITICAL ACCESS") , ]

# Notice extreme negative outliers
summary(hospitals$beds)
table(hospitals$beds)[1:10]

# Not sure what these outliers are supposed to be. Fill them with 0
hospitals[hospitals[, 3] == -999, 3] <- 0

# Convert states to factors; split data by state
hospitals$state <- factor(hospitals$state)
hospitals_split <- split(hospitals, hospitals$state)

# Get number of hospitals per state
num_hospitals <- unlist(lapply(hospitals_split, nrow))

# Get number of beds per state
num_beds <- rep(NA, length(hospitals_split))
for (i in 1:length(hospitals_split)) {
  state <- as.data.frame(hospitals_split[i])
  num_beds[i] <- sum(state[, 3])
}

# Create and fill data frame
hospital_totals <- as.data.frame(matrix(NA, nrow = 50, ncol = 9))
hospital_totals[, 1] <- sort(unique(hospitals$state)) # state
hospital_totals[, 2] <- as.numeric(num_hospitals) # hospitals
hospital_totals[, 3] <- as.numeric(num_beds) # beds
hospital_totals[, 4] <- pop$pop # state population 

# hospitals per 100K people
hospital_totals[, 5] <- round((hospital_totals[, 2] / 
                                 hospital_totals[, 4]) * 100000, 2) 
# beds per 100K people
hospital_totals[, 6] <- round((hospital_totals[, 3] / 
                                 hospital_totals[, 4]) * 100000, 2) 
hospital_totals[, 7] <- pop$landarea # state area

# hospitals per 1000 square miles
hospital_totals[, 8] <- round((hospital_totals[, 2] / 
                                 hospital_totals[, 7]) * 1000, 2) 
# beds per 1000 square miles
hospital_totals[, 9] <- round((hospital_totals[, 3] / 
                                 hospital_totals[, 7]) * 1000, 2)

# Change variable names
names(hospital_totals) <- c("state", "num_hospitals", "num_beds", 
                            "state_population", "hospitals_per_100k",
                            "beds_per_100k", "state_area_sqmiles", 
                            "hospitals_per_1000sqmile", "beds_per_1000sqmile")


#### MASK DATA

# Read in mask data
masks <- read.csv("mask-use-by-county.csv", as.is = TRUE, 
                  stringsAsFactors = FALSE)

# Change column names
colnames(masks) <- tolower(colnames(masks))
colnames(masks)[1] <- "combined"

# Read in data that will allow us to identify each county and state by FIPS code
fips <- read.csv("US_FIPS_Codes.csv", colClasses = "character")
colnames(fips) <- tolower(colnames(fips))

# Convert code to character in order to eliminate leading 0, matching format of
# mask data
fips$statecode <- as.integer(fips$statecode)
head(fips)

# Create combined state & county FIPS code by pasting individual components 
fips$combined <- paste0(as.character(fips$statecode), fips$countycode)

# Merge dataframes on the unique FIPS code of each county
masks <- merge(masks, fips, by = "combined")

# Drop original state and county code columns
masks <- masks[, -c(9, 10)]

# Rename
masks <- masks[, c("combined", "state", "county", "never", "rarely", 
                   "sometimes", "frequently", "always")]
masks$state <- setNames(state.abb, state.name)[masks$state]

# Omit entries for Washington D.C. by omitting NA's (only found in D.C.'s rows)
masks <- na.omit(masks)

# Read in data with FIPS code and population of each county
counties <- read.csv("PopulationEstimates.csv", as.is = TRUE, 
                     stringsAsFactors = FALSE, header = TRUE)

# Keep only the necessary columns: FIPS code, state, county, and population
counties <- counties[, c(1, 2, 3, 20)]
colnames(counties) <- c("combined", "state", "name", "population")

# Eliminate unnecessary head 
counties <- counties[5:nrow(counties), ]

# Clean population column as a numeric
counties$population <- as.numeric(gsub(",", "", counties$population))
head(counties)

# This dataset contains entries for entire states among the county entries. Each
# state entry is denoted with a FIPS code ending in three zeros. We can identify
# and eliminate all whole-state rows by eliminating rows with this FIPS pattern
counties <- counties[which(!grepl("000$", counties$combined)), ]

# Convert code to numeric
counties$combined <- as.numeric(counties$combined)

# Merge mask and county data by unique FIPS code
safety <- merge(masks, counties, by = "combined")
safety <- safety[, -c(9,10)]
colSums(is.na(safety))

# Multiply percentages in each category of mask-wearing frequency by county
# population in order to generate the number of people in each category
for (i in c(4, 5, 6, 7, 8)) {safety[, i] <- round(safety[, i] * safety[, 9])}

# Split data by state
split_counties <- split(safety, safety$state.x)

# Define vectors to hold cumulative totals of each category for each state
c_never <- rep(NA, length(split_counties))
c_rarely <- rep(NA, length(split_counties))
c_sometimes <- rep(NA, length(split_counties))
c_frequently <- rep(NA, length(split_counties))
c_always <- rep(NA, length(split_counties))
c_population <- rep(NA, length(split_counties))

# Populate vectors with state-wide totals based on the sum of totals for all
# counties in state
for (i in 1:length(split_counties)) {
  state <- as.data.frame(split_counties[i])
  c_never[i] <- sum(state[, 4])
  c_rarely[i] <- sum(state[, 5])
  c_sometimes[i] <- sum(state[, 6])
  c_frequently[i] <- sum(state[, 7])
  c_always[i] <- sum(state[, 8])
  c_population[i] <- sum(state[, 9])
}

# Fill data frame
mask_wearing <- as.data.frame(matrix(data = NA, nrow = 50, ncol = 7))
mask_wearing[, 1] <- sort(unique(safety$state.x))
mask_wearing[, 2] <- c_never
mask_wearing[, 3] <- c_rarely
mask_wearing[, 4] <- c_sometimes
mask_wearing[, 5] <- c_frequently
mask_wearing[, 6] <- c_always
mask_wearing[, 7] <- c_population
names(mask_wearing) <- c("state", "never", "rarely", "sometimes", "frequently", 
                         "always", "population")

# Generate state-wide percentages for each mask-wearing category
mask_wearing$pct_never <- 100 * round(mask_wearing$never / 
                                        mask_wearing$population, 4)
mask_wearing$pct_rarely <- 100 * round(mask_wearing$rarely / 
                                         mask_wearing$population, 4)
mask_wearing$pct_sometimes <- 100 * round(mask_wearing$sometimes / 
                                            mask_wearing$population, 4)
mask_wearing$pct_frequently <- 100 * round(mask_wearing$frequently / 
                                             mask_wearing$population, 4)
mask_wearing$pct_always <- 100 * round(mask_wearing$always / 
                                         mask_wearing$population, 4)


#### REOPENING DATA

# Read in
reopening <- read.csv("reopening.csv", as.is = TRUE, 
                      stringsAsFactors = FALSE, na.strings = c("", NA))
colnames(reopening) <- tolower(colnames(reopening))
head(reopening)

# Remove Washington D.C.
reopening <- reopening[reopening$state != "District of Columbia", ]

# Convert state names to abbreviations
reopening$state <- setNames(state.abb, state.name)[reopening$state]

# Add binary dummy variables
reopening$had.stay.at.home <-
  as.numeric(!is.na(reopening$start.of.stay.at.home))
reopening$ongoing.stay.at.home <-
  as.numeric(is.na(reopening$end.of.stay.at.home) & 
               reopening$had.stay.at.home == 1)



#### HEALTH REQUIREMENT DATA

# Read in
healthreq <- read.csv("healthreq.csv", as.is = TRUE, 
                      stringsAsFactors = FALSE)
colnames(healthreq) <- tolower(colnames(healthreq))

# Remove D.C.
healthreq <- healthreq[healthreq$state != "District of Columbia", ]

# Convert state names to abbreviations
healthreq$state <- setNames(state.abb, state.name)[healthreq$state]



#### REGIONS DATA

# Read in
regions <- read.csv("regions.csv", as.is = TRUE, 
                    stringsAsFactors = FALSE)
colnames(regions) <- tolower(colnames(regions))

# Remove DC
regions <- regions[regions$state != "District of Columbia", ]

# Set state.code as state name (so we have abbreviations) and then remove 
# state.code
regions$state <- regions$state.code
regions <- regions[, -2]

#### MERGE ALL CURRENT DATA TOGETHER BY STATE
# covid.latest, college.data, voting, pop, hospital_totals, mask_wearing, 
# reopening, healthreq, regions
frames <- list(covid.latest, pop, hospital_totals, college.data, voting, 
               mask_wearing, reopening, healthreq, regions)

current <- Reduce(function(x, y) merge(x = x, y = y, by = "state"), frames)


# Eliminate columns that won't be used. These are either redundant, missing a 
# large percentage of data, irrelevant (i.e. pertaining to insurance or 
# technical governmental guidelines), or not useful in comparing states to each
# other (i.e. gross numbers as opposed to percentages, which are better for 
# head-to-head comparison)

current <- current[, -c(2, 55:63, 65)] 

# Create new columns with log transformations
log_cols <- current[, c("confirmed", "deaths", "active", 
                        "people_tested", "negative", "positive", 
                        "totaltestresults", "density", 
                        "num_hospitals", "num_beds", "hospitals_per_100k", 
                        "beds_per_100k",
                        "state_area_sqmiles", "hospitals_per_1000sqmile", 
                        "beds_per_1000sqmile", "num_colleges", "never", 
                        "rarely", "sometimes", "frequently", "always", 
                        "population")]

# check for zeros or negatives in the data
if(sum(colSums(log_cols <= 0)) != 0) warning("There are non-positive values
                                             in the columns to be logged")
if(sum(is.na(log_cols)) != 0) warning("There are NA values
                                             in the columns to be logged")
if(sum(is.nan(unlist(log_cols))) != 0) warning("There are NaN values
                                             in the columns to be logged")

# Take log of all columns that need transforming. Store in separate dataframe
colnames(log_cols) <- paste0("log_", colnames(log_cols))
log_cols <- log(log_cols)

# Bind dataframes together. 'Current' now has both raw and log values of 
# the appropriate variables
current <- cbind(current, log_cols)

# Create new columns with values per 100k people
colnames(current)
capita_cols <- current[, c("confirmed", "deaths", "active",
                           "people_tested", 
                           "negative", "positive", "totaltestresults",  
                           "num_colleges", "never", "rarely", "sometimes", 
                           "frequently", "always")]

# Store per 100k values in new dataframe
colnames(capita_cols) <- paste0(colnames(capita_cols), "_per_100k")
capita_cols <- round(((100000 * capita_cols) / current$population), 2)

# Row bind dataframes together
current <- cbind(current, capita_cols)

# Save this dataframe with unstandardized quantitative variable as a .csv
cleanloc <- 'cleanfiles'    # directory
if (!dir.exists(cleanloc)) dir.create(cleanloc)
write.csv(current, file.path(cleanloc, "current.csv"))


#### Cleaning, transforming, and standardizing numerical variables in 
#### aggregated dataframe, 'current'

# Create duplicate, which will become the standardized dataset. It's probably
# worth saving both versions, just in case
current.standard <- current

# Isolate numerical variables
current.numerical <- current.standard[, unlist(lapply(current.standard, 
                                                      is.numeric))]

# Specify which numerical variables are percentages. These require a different
# type of standardization
percents <- current.numerical[, grep("^pct.*", colnames(current.numerical))]
percents <- abs(percents) / 100

# Rescale percentages based on min and max. All will fall between 0 and 1
for (i in 1:ncol(percents)) {
  percents[, i] <- (percents[, i] - min(percents[, i])) / 
    (max(percents[, i] - min(percents[, i])))
}
# Replace original data in dataframe with standardized data
current.numerical[, grep("^pct.*", colnames(current.numerical))] <- percents

# Define 'values' variable for all non-percentage columns
values <- current.numerical[, !(current.numerical %in% percents)]

# Normalize each column by subtracting column mean from each value and dividing
# by column standard deviation
for (i in 1:ncol(values)) {
  values[, i] <- (values[, i] - mean(values[, i])) / sd(values[, i])
}
# Replace data with standardized version
current.numerical[, !(current.numerical %in% percents)] <- values

# Replace all cleaned numerical variables in full standardized dataframe
current.standard[, unlist(lapply(current.standard, is.numeric))] <- 
  current.numerical

# Save standardized version as a .csv
cleanloc <- 'cleanfiles'    # directory
if (!dir.exists(cleanloc)) dir.create(cleanloc)
write.csv(current.standard, file.path(cleanloc, "current_standardized.csv"))



