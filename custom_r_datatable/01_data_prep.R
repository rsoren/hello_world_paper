#
# 01_data_prep.R
#
# Get data from the shared cost effectiveness folder
# Prepare it for microsimulation and save locally for future use
#
# Reed Sorensen
# May 2016
#

require(data.table)
require(dplyr)

# OS-dependent file paths
# jpath <- ifelse(Sys.info()["sysname"] == "Windows", "J:/", "/home/j/")
# project_folder <- paste0(jpath, "Project/Cost_Effectiveness/dev/")

project_folder <- paste0(
  "C:/Users/rsoren/Documents/prog/projects/hello_world_paper/"
)

# choose whether to save the data, or just read it in
save_data <- TRUE


# read in data on mortality and incidence
mort_infile <- fread(paste0(project_folder, "_data/Mortality_Rates.csv")) %>%
  select(year = Year, age = Age, sex, rate_mortality = Mortality_Rate)

mort_ihd_infile <- fread(paste0(project_folder, "_data/ihd_mortality_rate.csv")) %>%
  select(year = Year, age = Age, sex, rate_mortality_ihd = Mortality_Rate)

ihd_incidence_infile <- fread(paste0(project_folder, "_data/IHD incidence rates.csv")) %>%
  select(year = Year, age = Age, sex, rate_incidence_ihd = Incidence)

rates <- mort_infile %>%
  left_join(mort_ihd_infile, by = c("year", "age", "sex")) %>%
  left_join(ihd_incidence_infile, by = c("year", "age", "sex")) %>%
  mutate(rate_mortality_ihd_plus = rate_mortality + rate_mortality_ihd)

if (save_data) saveRDS(rates, paste0(project_folder, "custom_r_datatable/data/rates.RDS"))


# read in life table
life_table <- fread(paste0(project_folder, "_data/interpolated_reference_life_table.csv")) %>%
  dplyr::rename(yll = ex)

if (save_data) saveRDS(life_table, paste0(project_folder, "custom_r_datatable/data/life_table.RDS"))



# read in population files

# -- level 1
pop_level1 <- fread(paste0(project_folder, "_data/Hello_World_Population.csv"))
if (save_data) saveRDS(pop_level1, paste0(project_folder, "custom_r_datatable/data/pop_level1.RDS"))


# -- level 2
pop_level2 <- fread(paste0(project_folder, "_data/level2_population.csv"))
if (save_data) saveRDS(pop_level2, paste0(project_folder, "custom_r_datatable/data/pop_level2.RDS"))

