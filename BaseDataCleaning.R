##### Cleaning NK provocation, SK election, and US data to create a combined 
##### single data set disaggregated into weeks
#####===========================================================================

##### Load libaries
#####===========================================================================
library(dplyr)
library(lubridate)

#### PROVOCATIONS
####============================================================================

### Cleaning NK provocations data
###=============================================================================
NK.Provocations_cleaned <- read.csv("NK Provocations_cleaned.csv")
provocations <- NK.Provocations_cleaned

## Fixing the data format
##==============================================================================
provocations$Date <- as.Date(provocations$Date, format = "%m/%d/%y")

provocations %>%
  mutate(
    year = year(Date),
    month = month(Date),
    year = ifelse(year > 2023, year - 100, year),
    Date = as.Date(paste(year, month, mday(Date),sep="-")),
    week  = week(Date)
  ) -> provocations

## Saving NK provocations data file
##==============================================================================
save(provocations, file = "provocations.rda") 

## Creating provocation count data set
##==============================================================================
provocations %>%
  group_by(year, week) %>%
  count() -> 
  count_prov
colnames(count_prov)[3] <- "number_events" # Renaming provocation count column

# Creating nulcear provocation variable column
count_prov$nuclear <- sapply(1:nrow(count_prov), function(i){
  provocations %>%
    filter(week == count_prov$week[i] & year == count_prov$year[i]) ->
    temp
  as.numeric(any(temp$Type == "Nuclear Provocation"))
})

# Creating missile provocation variable column
count_prov$missile <- sapply(1:nrow(count_prov), function(i){
  provocations %>%
    filter(week == count_prov$week[i] & year == count_prov$year[i]) ->
    temp
  as.numeric(any(temp$Type == "Missile Provocation"))
})

# Creating other provocation variable column
count_prov$other_prov <- sapply(1:nrow(count_prov), function(i){
  provocations %>%
    filter(week == count_prov$week[i] & year == count_prov$year[i]) ->
    temp
  as.numeric(any(temp$Type == "Other Provocation"))
})

## Creating a data frame from 1958~2023 in a weekly disaggregated format
##==============================================================================
data.frame(Date = as.Date("1958-01-01") : as.Date("2023-03-22")) %>%
  mutate(
    Date = as.Date(Date, origin = "1970-01-01"),
    year = year(Date),
    week = week(Date)
  ) %>%
  group_by(year, week) %>%
  summarise() %>%
  ungroup() ->
  weekly_data

# Merge "count_prov" (provocation count data set) & "weekly_data" (weekly df)
weekly_data %>%
  left_join(count_prov, by = c("year", "week")) ->
  weekly_data
weekly_data[is.na(weekly_data)] <- 0
weekly_data

#### ELECTIONS
####============================================================================

### SK elections
###=============================================================================
`ROK.Elections.&.Provocations_cleaned` <- read.csv("ROK Elections & Provocations_cleaned.csv")
elections_sk <- `ROK.Elections.&.Provocations_cleaned`

## Cleaning the data & disaggregating it into weeks
##==============================================================================
elections_sk %>%
  select(Date = Election.Year, Election.Type) %>%
  distinct() %>%
  mutate(
    Date = as.Date(Date, format = "%m/%d/%y"),
    year = year(Date),
    year = ifelse(year > 2023, year - 100, year),
    Date = as.Date(paste(year, month(Date), mday(Date),sep="-")),
    week  = week(Date)
  ) -> election_sk

## Saving SK elections data file
##==============================================================================
save(election_sk, file = "election_sk.rda")

## Merge "election_sk" (SK election data set) & "weekly_data" (weekly df)
##==============================================================================
weekly_data <- left_join(weekly_data,
                         select(election_sk, -Date),
                         by = c("year", "week"))

# Rename "NA" values in "Election.Type" as "No Election"
weekly_data$Election.Type[is.na(weekly_data$Election.Type)] <- "No Election"

# Rename election column name
colnames(weekly_data)[7] <- "Election.SK"

### US elections
###=============================================================================
elections_us <- read.csv("US Elections & Provocations_cleaned.csv")

## Cleaning the data & disaggregating it into weeks
##==============================================================================
elections_us %>%
  select(Date = Election.Year, Election.US = Election.Type) %>%
  distinct() %>%
  mutate(
    Date = as.Date(Date, format = "%m/%d/%y"),
    year = year(Date),
    year = ifelse(year > 2023, year - 100, year),
    Date = as.Date(paste(year, month(Date), mday(Date),sep="-")),
    week  = week(Date)
  ) -> election_us

## Saving US elections data file
##==============================================================================
save(election_us, file = "election_us.rda")

## Merge "election_us" (US election data set) & "weekly_data" (weekly df)
##==============================================================================
weekly_data <- left_join(weekly_data,
                         select(election_us, -Date),
                         by = c("year", "week"))

# Rename "NA" values in "Election.US" as "No Election"
weekly_data$Election.US[is.na(weekly_data$Election.US)] <- "No Election"

# Saving the cleaned provocation & election data set disaggregated into weeks
save(weekly_data, file = "weekly_data.rda")
