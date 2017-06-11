#
# 03_microsim_level1.R
#
# Reed Sorensen
# May 2016
#

require(dplyr)
require(data.table)
require(stringr)

project_folder <- paste0(
  "C:/Users/rsoren/Documents/prog/projects/hello_world_paper/"
)

# load utility functions
source(paste0(project_folder, "custom_r_datatable/02_functions.R")) 

# load rates and life table
rates_tmp <- readRDS(paste0(project_folder, "custom_r_datatable/data/rates.RDS"))
life_table <- readRDS(paste0(project_folder, "custom_r_datatable/data/life_table.RDS"))


# set simulation parameters
start_year <- 1990
end_year <- 2013

death_state <- "died"
other_states <- NULL

disability_weights <- data.table(
  id_number = 0,
  weight = 0.0,
  description = "Healthy"
)


# set up baseline population
pop <- readRDS(paste0(project_folder, "custom_r_datatable/data/pop_level1.RDS"))

sims <- select(pop, id = simulant_id, age, sex) %>%
  .[, age_start := age] %>%
  .[, cost := 0] %>%
  .[, disability_state := as.integer(0)] %>%
  .[, ihd := as.integer(0)] %>% # patch
  .[, (death_state) := as.integer(9) ] %>%
  .[, (paste0(death_state, "_history")) := ""] %>%
  .[, disability_state_history := ""]

run_microsim <- function(intervention = FALSE, seed = 123) {

  # intervention <- TRUE; seed <- 123 # dev variable

  set.seed(seed)

  # define intervention
  rates <- as.data.table(copy(rates_tmp))

  if (intervention) {
    rates[age >= 25 & year >= 1995, rate_mortality := rate_mortality / 2]
  }

  # convert rates to probabilities, and retain only the needed vars
  probs <- rate2prob(rates)
  prob_vars <- c("prob_mortality")
  probs <- probs[, (c("year", "age", "sex", prob_vars)), with = FALSE]
  setkeyv(probs, c("sex", "age")) # set key for merge


  # run simulation

  system.time(for (yr in start_year:end_year) {

    # yr <- 1990 # dev variable

    # update age and get new probabilities
    sims[, age := age_start + yr - start_year]
    setkeyv(sims, c("sex", "age"))
    probs2 <- probs[year == yr][, year := NULL]
    sims <- probs2[sims]

    # apply changes
    sims[died != 1, died := rbinom(.N, 1, prob_mortality)]

    # record changes
    sims[, died_history := paste0(died_history, died)]

    # add cost
    sims[age >= 25 & yr >= 1995 & died != 1 & intervention, cost := cost + 2]


    sims[, (prob_vars) := NULL] # remove yr-specific probs

  })

  # calculate DALYs and costs
  sims2 <- summarize_results(sims, d_weights = disability_weights)

  return(sims2)

}


system.time(result <- get_ICER(seed = 125))

