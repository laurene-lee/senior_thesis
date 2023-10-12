##### Additional data cleaning for analysis on US elections & NK provocations
#####===========================================================================

##### Load libaries
#####===========================================================================
library(dplyr)
library(margins)
library(stargazer)

### Creating a separate US data set for analysis 
###=============================================================================
weekly_data |>
  mutate( was_event = as.numeric(weekly_data$number_events > 0)) |> # Creating event(NK provocation) occurrence variable
  select( -Election.SK) |> # Eliminating SK elections to focus on US elections 
  mutate( election0 = as.numeric(Election.US != "No Election")) -> # Creating US election occurrence variable
  weekly_us

## To analyze the relationship between NK provocations & past and upcoming US elections, 
## we lag US election data 
##==============================================================================

# Creating 16 different columns via lagging the data
#===============================================================================
for (t in 1:15){
  variable_name <- paste0("election", t) 
  weekly_us[[variable_name]] <- c(
    rep(NA, t),
    weekly_us$election0[1:(nrow(weekly_us) - t)]
  )
}

# Next, create 4 columns that aggregatd the data into 4 week chunks
#===============================================================================
for (t in 1:4){
  variable_name <- paste0("election_M", t)
  start <- (t-1)*4
  end   <- start + 3
  val   <- rowSums(weekly_us[, paste0("election", start:end)])
  val[val>0] <- 1
  weekly_us[[variable_name]] <- val
}

# Looking 1 month ahead into the future
#===============================================================================
for (t in 1:4){
  variable_name <- paste0("electionP", t) 
  weekly_us[[variable_name]] <- c(
    weekly_us$election0[(t+1):(nrow(weekly_us))],
    rep(NA, t)
  )
}

val   <- rowSums(weekly_us[, paste0("electionP", 1:4)])
val[val>0] <- 1
weekly_us[["election_PP"]] <- val

### Analysis
###=============================================================================

## Logit Model

# Election within past 1-4 weeks
glm(was_event ~ year + election_M1, data = weekly_us, family=binomial) -> reg_log1
summary(reg_log2)

# Election within past 5-8 weeks
glm(was_event ~ year + election_M2, data = weekly_us, family=binomial) -> reg_log2
summary(reg_log2)

# Election within past 9-12 weeks
glm(was_event ~ year + election_M3, data = weekly_us, family=binomial) -> reg_log3
summary(reg_log3)

# Election within past 13-16 weeks
glm(was_event ~ year + election_M4, data = weekly_us, family=binomial) -> reg_log4
summary(reg_log4)

# Election within upcoming 1-4 weeks
glm(was_event ~ year + election_PP, data = weekly_us, family=binomial) -> reg_log5
summary(reg_log1)

# Calculating logit coefficients
margins(reg_log1)
margins(reg_log2)
margins(reg_log3)
margins(reg_log4)
margins(reg_log5)

stargazer(reg_log1, reg_log2, reg_log3, reg_log4, reg_log5, type = "latex")

# Looking at nuclear provocations
# ---> No nuclear response to elections
lm(nuclear ~ election_PP, data = weekly_us, subset= year >= 2006) |>
  summary()

lm(nuclear ~ election_M1, data = weekly_us, subset= year >= 2006) |>
  summary()
