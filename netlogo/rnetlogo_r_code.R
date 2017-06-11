#
# NOTE: Users will need to change filepaths 
# -- data can be found in the "~/data" folder
# -- NetLogo installation location or name may vary by user (line 21)
#
# Download NetLogo at https://ccl.northwestern.edu/netlogo/index.shtml
#


###################################################
### Load the RNetLogo package
###################################################
library("RNetLogo")
library(dplyr)


###################################################
### Start NetLogo 
### (for faster production run it headless, i.e. gui=FALSE)
### Lines below load the microsimulation
###################################################
nlDir <- "C:/Program Files (x86)/NetLogo 5.0.5"
setwd(nlDir)
nl.path <- getwd()
NLStart(nl.path, gui = T)
NLLoadModel("C:/Users/emumford/Documents/Hello_World/model_netlogo_code.nlogo")

###################################################
# Determine whether or not to run intervention
##################################################
run_intervention <- T

# Set up the simulation (needs to be done before every run)
NLCommand("setup") 

# import population csv
population<-read.csv("C:/Users/emumford/Documents/Hello_World/data2/Hello_World_Population.csv",stringsAsFactors = F)

# set simulants ages, sexes, simulant_id, and year (year = start of simulation)
# from the population file (this will ensure that ages/sexes are correlated)

sexes<-population$sex
NLSetAgentSet("turtles",sexes,var.name = "sex")

ages <- population$age
NLSetAgentSet("turtles",ages,var.name = "age")

sim_ids<-population$simulant_id
NLSetAgentSet("turtles",sim_ids,var.name='simulant_id')

# PUT THE YEAR THAT YOU WANT THE SIMULATION TO START HERE
year_start_of_simulation <- 1990
year <- rep(year_start_of_simulation,times=10000)
NLSetAgentSet("turtles",year,var.name='year')

# Let's add mortality rates from GBD
# Pull in a csv with age/year/sex specific mortality rates
# Data is in long form w/ one row for each age/sex/year
# Convert sex to numeric variables (Male=1, Female=2)

mortality_rates<-read.csv("C:/Users/emumford/Documents/Hello_World/data2/Mortality_Rates.csv", stringsAsFactors = F)

mortality_rates$sex[mortality_rates$sex=="Male"] <- "1"
mortality_rates$sex[mortality_rates$sex=="Female"] <- "2"
mortality_rates$sex <- as.numeric(mortality_rates$sex)

# We need to extrapolate the mortality rate for people over 80 years old
# For now, we're just extending the mortality rates of 80 year olds to people
# age 80+. We'll go up to age 103, since that is the oldest age anyone can reach
# in the current simulation (start at age 80 and then sim runs 23 years)
# expand_ages<-seq(81,103)
# expand_years<-seq(1990,2013)
# expand_sexes<-c(1,2)
# expand_table <- expand.grid(Age=expand_ages,Year=expand_years,sex=expand_sexes)
# dup_table <- mortality_rates[mortality_rates$Age==80,]
# dup_table$Age <- NULL
# age80plus <- merge(expand_table,dup_table,by=c("Year","sex"))

# Now append the mortality rates for 80+ year olds
# mortality_rates<-rbind(mortality_rates,age80plus)


# Apply the intervention
# Also apply cost
if (run_intervention) {
  mortality_rates <- mortality_rates %>%
    mutate(
      Mortality_Rate = ifelse(Age >= 25 & Year >= 1995, Mortality_Rate / 2, Mortality_Rate)
    )
}

# Calculate Probability of Dying
# Probability of Dying = 1 - e^(-mortality rate * time period)
mortality_rates$prob_die<-1-exp(-mortality_rates$Mortality_Rate)

# Initialize a dataframe to hold all of the dead people
dead_df<-data.frame(age=integer(),sex=integer(),simulant_id=integer(),year=integer())

# Initialize a dataframe to hold all simulant information for each year
simulation_total_df <- data.frame(age=integer(),sex=integer(),simulant_id=integer(),year=integer())

#######################################################################
#######################################################################
###################     RUN THE SIMULATION    #########################
#######################################################################
#######################################################################

# SET NUMBER OF YEARS THAT YOU WANT THE SIMULATION TO RUN
# for 1990-2013, set 24
number_of_years_that_simulation_will_run<- 24

system.time(
  for (i in 1:number_of_years_that_simulation_will_run)
  {
    # Now create a dataframe of our simulants characteristics
    GetAgentSet<-NLGetAgentSet(c("age","sex","simulant_id","year"),"turtles")
    str(GetAgentSet)
    
    # Now determine the probability of death for every simulant in the population
    # based on each simulant's age/year/sex
    # merge the simulant characteristics with the table of mortality rates to get the
    # rate at each timestep
    
    mortality_rate_for_timestep<- merge(mortality_rates,GetAgentSet, by.x=c("Year","Age","sex"),by.y=c("year","age","sex"))
    
    # sort the probabilities of death in the same way that GetAgentSet is sorted so that mortality rates are correctly applied in next line of code
    mortality_rate_for_timestep <- mortality_rate_for_timestep[match(GetAgentSet$simulant_id, mortality_rate_for_timestep$simulant_id),]
    
    # apply the mortality rate to the "prob" variable
    # the prob variable determines the probability of dying
    probs<- mortality_rate_for_timestep$prob_die
    NLSetAgentSet("turtles",probs,var.name='prob')
    
    # "go" tells the simulation to start the next time step
    NLCommand("go") 
    
    
    # Now we need to create a dataframe of the people that died
    # Read in the new dataframe of simulants and compare to the old
    # dataframe to see who is no longer part of the simulation.
    # Those that are not part of the simulation are dead.
    
    # use the dataframe of all people that are still alive
    current_timestep<-NLGetAgentSet(c("age","sex","simulant_id","year"),"turtles")
    
    # anti-join to the dataframe of people that were alive before the timestep
    recently_deceased<-anti_join(GetAgentSet,current_timestep,by=c("simulant_id"))
    
    # rbind the recently_deceased to dead_df everytime step to aggregate dead people
    dead_df <- rbind(dead_df,recently_deceased)
    
    # Create a dataframe full of each year's worth of data
    simulation_total_df <- rbind(simulation_total_df,GetAgentSet)
  }
)

# Apply cost to population that received the intervention
if (run_intervention) {
  simulation_total_df <- simulation_total_df %>%
    mutate(
      Cost = ifelse(age >= 25 & year >= 1995, 2, 0))
  write.csv(dead_df, "C:/Users/emumford/Documents/Hello_World/data2/NetLogo_With_Intervention_Results_June_9.csv",row.names = F)
} else {
  write.csv(dead_df, "C:/Users/emumford/Documents/Hello_World/data2/NetLogo_Without_Intervention_Results_June_9.csv",row.names = F)
}


# Get the total cost of the intervention
sum(simulation_total_df$Cost)
#[1] 204830


###################################################
## Determine YLLs
###################################################

# Read in with and without intervention results
With_intervention <-read.csv("C:/Users/emumford/Documents/Hello_World/data2/NetLogo_With_Intervention_Results_June_9.csv")
Without_intervention <-read.csv("C:/Users/emumford/Documents/Hello_World/data2/NetLogo_Without_Intervention_Results_June_9.csv")

# Read in the reference life table
ref_lifetable<- read.csv("C:/Users/emumford/Documents/Hello_World/data2/interpolated_reference_life_table.csv")

# Merge the reference life table and results files to determine YLLs
With_intervention<-merge(With_intervention,ref_lifetable,by="age")
Without_intervention<-merge(Without_intervention,ref_lifetable,by="age")

# Sum up YLLs and determine difference
Without_Intervention_YLLs <- sum(Without_intervention$ex)
With_Intervention_YLLs<- sum(With_intervention$ex)
Without_Intervention_YLLs - With_Intervention_YLLs
# [1] 17396.13

###################################################
### Close NetLogo (if still open)
###################################################
NLQuit()

