##### Additional data cleaning for SK election & NK provocation analysis
#####===========================================================================

##### Load libaries
#####===========================================================================
library(dplyr)
library(margins)
library(stargazer)

### Creating a separate SK data set for analysis 
###=============================================================================
weekly_data |>
  mutate( was_event = as.numeric(weekly_data$number_events > 0)) |> # Creating event(NK provocation) occurrence variable
  select( -Election.US) |> # Eliminating US elections to focus on SK elections first
  mutate( election0 = as.numeric(Election.SK != "No Election")) -> # Creating SK election occurrence variable
  weekly_sk

## To analyze the relationship between NK provocations & past and upcoming SK elections, 
## we lag SK election data 
##==============================================================================

# Creating 16 different columns via lagging the data
#===============================================================================
for (t in 1:15){
  variable_name <- paste0("election", t) 
  weekly_sk[[variable_name]] <- c( 
    rep(NA, t), 
    weekly_sk$election0[1:(nrow(weekly_sk) - t)] 
  )
} 

# Next, create 4 columns that aggregatd the data into 4 week chunks
#===============================================================================
for (t in 1:4){
  variable_name <- paste0("election_M", t)
  start <- (t-1)*4 # creating our range
  end   <- start + 3 
  val   <- rowSums(weekly_sk[, paste0("election", start:end)]) 
  val[val>0] <- 1 
  weekly_sk[[variable_name]] <- val 
}

# Looking 1 month ahead into the future
#===============================================================================
for (t in 1:4){
  variable_name <- paste0("electionP", t) 
  weekly_sk[[variable_name]] <- c(
    weekly_sk$election0[(t+1):(nrow(weekly_sk))],
    rep(NA, t)
  )
}

val   <- rowSums(weekly_sk[, paste0("electionP", 1:4)]) # Aggregate
val[val>0] <- 1
weekly_sk[["election_PP"]] <- val

### Analysis
###=============================================================================

## Logit Model

# Election within past 1-4 weeks
glm(was_event ~ year + election_M1, data = weekly_sk, family=binomial) -> reg_log1
summary(reg_log2)

# Election within past 5-8 weeks
glm(was_event ~ year + election_M2, data = weekly_sk, family=binomial) -> reg_log2
summary(reg_log2)

# Election within past 9-12 weeks
glm(was_event ~ year + election_M3, data = weekly_sk, family=binomial) -> reg_log3
summary(reg_log3)

# Election within past 13-16 weeks
glm(was_event ~ year + election_M4, data = weekly_sk, family=binomial) -> reg_log4
summary(reg_log4)

# Election within upcoming 1-4 weeks
glm(was_event ~ year + election_PP, data = weekly_sk, family=binomial) -> reg_log5
summary(reg_log1)

# Calculating logit coefficients
margins(reg_log1)
margins(reg_log2)
margins(reg_log3)
margins(reg_log4)
margins(reg_log5)

# We're finding that:
# NK is responding either 1 mth before an election or 2-3 mths after an election
# (2-3 mths after an election is approximately when the elected officials are
# actually sworn into office)

stargazer(reg_log1, reg_log2, reg_log3, reg_log4, reg_log5, type = "latex")

# Looking at nuclear provocations
# ---> No nuclear response to elections
lm(nuclear ~ election_PP, data = weekly_sk, subset= year >= 2006) |>
  summary()

lm(nuclear ~ election_M1, data = weekly_sk, subset= year >= 2006) |>
  summary()
