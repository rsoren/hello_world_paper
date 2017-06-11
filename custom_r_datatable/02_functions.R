#
# 02_functions.R
#
# Utility functions for microsimulation
#
# Reed Sorensen
# May 2016
#



rate2prob <- function(x, time_length = 1) {

  # Convert all rates to probabilities in a given data table,
  #   according to: prob = 1 - exp(-rate * time_length)
  # The unit for time_length is years

  require(data.table)
  # x <- rates; time_length = 1 # dev variables

  if (!is.data.table(x)) stop("'x' must be in data.table format")

  rate_names <- names(x)[grepl("rate_", names(x))]
  for (nm in rate_names) { x[, (nm) := 1 - exp(-get(nm) * time_length)] }
  names(x)[names(x) %in% rate_names] <- gsub("rate_", "prob_", rate_names)

  return(x)

}


summarize_results <- function(x, d_weights, seed = 123) {

  require(stringr)
  # x <- sims; d_weights <- disability_weights # dev variables

  sims2 <- x %>%
    as.data.frame(.) %>%
    mutate(
      age_at_death = str_count(died_history, "0") + age_start) %>%
    left_join(life_table, by = c("age_at_death" = "age")) %>%
    mutate(
      yll = ifelse(died == 1, yll, 0),
      yld = 0 )

  for (num in d_weights$id_number) {

    # num <- 1 # dev variable

    wt <- d_weights[id_number == num, weight]
    num <- as.character(num)

    sims2 <- sims2 %>%
      mutate(yld = yld + str_count(disability_state_history, num) * wt)
  }

  sims2 <- sims2 %>%
    mutate(
      daly = yll + yld,
      any_ihd = ifelse(str_count(ihd, "1") > 0, 1, 0) ) %>%
    summarize(
      daly = sum(daly),
      yll = sum(yll),
      yld = sum(yld),
      cost = sum(cost),
      any_ihd = sum(any_ihd)
    )

  return(sims2)
}



get_ICER <- function(seed = 123, check_ihd = FALSE) {

  x1 <- run_microsim(intervention = FALSE, seed = seed)
  yll_control <- x1[["yll"]]
  yld_control <- x1[["yld"]]
  daly_control <- x1[["daly"]]
  cost_control <- x1[["cost"]]

  x2 <- run_microsim(intervention = TRUE, seed = seed)
  yll_int <- x2[["yll"]]
  yld_int <- x2[["yld"]]
  daly_int <- x2[["daly"]]
  cost_int <- x2[["cost"]]

  icer <- abs((cost_int - cost_control) / (daly_int - daly_control))
  icer_yld <- abs((cost_int - cost_control) / (yld_int - yld_control))
  cost_diff <- abs(cost_int - cost_control)
  daly_diff <- abs(daly_int - daly_control)

  out <- round(c(cost_diff, daly_diff, icer, icer_yld), digits = 2)
  names(out) <- c("cost_diff", "daly_diff", "icer", "icer_yld")

  if (check_ihd) {
    out <- append(out, c(ihd_int = x2[["any_ihd"]], ihd_control = x1[["any_ihd"]]) )
  }

  return(out)

}


cat("Loaded functions:
  rate2prob
  summarize_results
  get_ICER"
)

