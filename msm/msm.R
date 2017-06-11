#
# msm.R
#
# msm is a good example of a system not designed to handle time-variable rates
# -- this implementation updates the rates on each iteration of the time loop
#

library(msm)

# minimal test
# df1 <- expand.grid(time = seq(0, 20, by = 1), subject = 1:100)
# qmatrix <- as.matrix(rbind(c(1, 0.1, 0.3), c(0, 1, 0.3), c(0, 0, 1)))
# system.time(df2 <- simmulti.msm(df1, qmatrix))


# now using GBD population and age-specific mortality

library(dplyr)
library(mgcv)

project_folder <- "C:/Users/rsoren/Documents/prog/projects/hello_world_paper/"

# set parameters
n_simulants <- 10000
start_year <- 1990
n_years <- 23 # this goes from 1990 through 2013


run_intervention <- FALSE


# create baseline population
pop <- read.csv(paste0(project_folder, "/_data/Hello_World_Population.csv")) %>%
  select(subject = simulant_id, age = age, sex = sex)

df3 <- expand.grid(subject = 1:n_simulants, time = 0:n_years) %>%
  arrange(subject, time) %>%
  left_join(pop, by = "subject") %>%
  group_by(subject) %>%
  mutate(
    year = time + 1990,
    age = first(age) + time,
    state = ifelse(time == 0, 1, NA),
    cost = 0) %>%
  filter(!is.na(age)) %>%
  as.data.frame(.)

# apply intervention cost
if (run_intervention) {
  df3 <- df3 %>%
  mutate(cost = ifelse(year >= 1995 & age >= 25, 2, 0))
}


# individual-age life table for calculating YLLs
ltable3 <- read.csv(paste0(project_folder, "_data/interpolated_reference_life_table.csv")) %>%
  rename(yll = ex)


states <- c("well", "dead")


# create function that returns mortality rate for a given age/year combination
mort_infile <- read.csv(
  file = paste0(project_folder, "_data/Mortality_Rates.csv")
)

mort1 <- mort_infile %>% select(year_id = Year, age = Age, sex, mortality_rate = Mortality_Rate)

tmp_mort <- expand.grid(age = 80:120, sex = 1:2, year_id = 1990:2013)

tmp_mort2 <- mort1 %>%
  filter(age == 80) %>%
  right_join(tmp_mort, by = c("year_id", "sex")) %>%
  select(year_id, age = age.y, sex, mortality_rate)

mort2 <- rbind(mort1, tmp_mort2) %>%
  rename(year = year_id) %>%
  arrange(year, sex, age, mortality_rate)



# transition rates
trans <- mort2 %>%
  rename(well_dead = mortality_rate)


# apply intervention, cutting mortality rate in half for age >25 starting in 1995
if (run_intervention) {
  trans <- trans %>%
    mutate(
      well_dead = ifelse(age >= 25 & year >= 1995, well_dead / 2, well_dead) )
}


# function for getting qmatrix, given age and year
get_qmatrix <- function(agevar, yearvar, sexvar) {

#   agevar <- 34
#   yearvar <- 1991
#   sexvar <- 2
  mat <- diag(1, nrow = length(states), ncol = length(states))
  colnames(mat) <- rownames(mat) <- states
  dat <- subset(trans, age == agevar & year == yearvar & sex == sexvar)
  nms <- names(dat)[!names(dat) %in% c("age", "year", "sex")]

  for (nm in nms) {
    nm <- "well_dead"
    nm2 <- strsplit(nm, split = "_")[[1]]
    mat[nm2[1], nm2[2]] <- dat[1, nm]
  }

  return(mat)

}




# run the simulation
# --

set.seed(127)

system.time(

for (yr in (start_year + 0:n_years)) {

  # yr <- 1990 # dev variable
  cat(paste0(yr, "\n"))

  tmp <- lapply(split(df3, df3$age, df3$sex), function(x) { try({

    x <- subset(x, year == yr) %>%
      rename(start = state)

#     x <- subset(df3, age == 75 & sex == 2) # dev variables
#     x <- subset(x, year == yr) %>%
#       rename(start = state)

    agetmp <- x[1, "age"]
    sextmp <- x[1, "sex"]
    cat(paste0(agetmp, "\n"))

    qmatrix <- get_qmatrix(agevar = agetmp, yearvar = yr, sexvar = sextmp)

    next_year <- x %>%
      select(subject, time) %>%
      mutate(time = time + 1)

    x2 <- rbind(x[, c("subject", "time")], next_year) %>%
      arrange(subject, time)

    x3 <- simmulti.msm(x2, qmatrix, start = x$start) %>%
      filter(time == max(time))

    return(x3)


  }, silent = TRUE) })

  nodata <- sapply(tmp, is.character)
  tmp <- do.call("rbind", tmp[!nodata])


  df3 <- df3 %>%
    left_join(tmp, by = c("subject", "time")) %>%
    mutate(state = ifelse(is.na(state.x), state.y, state.x)) %>%
    select(subject, time, age, sex, year, state, cost, -keep)

}

)


df3 <- df3 %>%
  mutate(cost = ifelse(state == 2, 0, cost))

tmp <- subset(df3, state == 2) %>%
  group_by(subject) %>%
  summarize(
    age = first(age),
    age_at_death = first(age),
    year_at_death = first(year)) %>%
  mutate(years_til_2013 = 2013 - year_at_death) %>%
  left_join(ltable3, by = "age") %>%
  # mutate(yll = ifelse(yll < years_til_2013, yll, years_til_2013)) %>% # this line is for cutting off YLLs at the end of the sim
  select(subject, age, age_at_death, yll)


df4 <- df3 %>%
  mutate(yld = ifelse(state == 2, 0.0, 0)) %>% # setting disability weight to zero for comparability
  left_join(tmp, by = c("subject", "age")) %>%
  mutate(yll = ifelse(is.na(yll), 0, yll))

df5 <- df4 %>%
  group_by(subject) %>%
  summarize(
    yld = sum(yld),
    yll = sum(yll),
    cost = sum(cost)) %>%
  mutate(daly = yld + yll)


(cost_total <- sum(df5$cost, na.rm = TRUE))
# Without intervention = 0
# With intervention = 203632


(daly_total <- sum(df5$daly, na.rm = TRUE))
# Without intervention = 54351
# With intervnetion = 39437








