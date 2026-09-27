#' Extract REMIND variable values from MIF
#'
#' Reads REMIND MIF and extracts values for specific variables that map to PIAM parents
#'
#' @param remind_data Quitte data frame with REMIND data (pre-loaded)
#' @param mapping Data frame with columns: piam_variable (REMIND source), variable (PIAM target), piam_factor
#' @param piam_parent The PIAM parent variable to get REMIND sources for
#' @param scenario Scenario to filter by (optional)
#' @param region Region to filter by (optional)
#' @param period Period to filter by (optional)
#'
#' @return Data frame with columns: model, scenario, region, period, piam_variable, piam_unit, value, piam_factor
#'   Where piam_variable is the REMIND variable name and piam_factor is the mapping factor
#'
#' @keywords internal
#' @export
extractRemindValues <- function(remind_data, mapping, piam_parent, scenario = NULL, region = NULL, period = NULL) {

  # Get REMIND variables that map to this PIAM parent
  # Note: The mapping may have "+" characters that were removed from MIF data
  # So we clean the mapping variables to match the MIF variables
  remind_vars_for_parent <- mapping %>%
    filter(.data$variable == piam_parent) %>%
    distinct(.data$piam_variable, .data$piam_factor) %>%
    mutate(piam_variable_clean = piamutils::deletePlus(.data$piam_variable))

  if (nrow(remind_vars_for_parent) == 0) {
    return(data.frame(
      model = character(),
      scenario = character(),
      region = character(),
      period = integer(),
      piam_variable = character(),
      piam_unit = character(),
      value = numeric(),
      piam_factor = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  # Convert to regular data frame to ensure filtering works properly
  result <- as.data.frame(remind_data)

  # Extract REMIND context - start with variable filter
  # Use cleaned variable names to match the MIF data (which has + removed)
  result <- result %>%
    filter(.data$variable %in% remind_vars_for_parent$piam_variable_clean)

  # Apply context filters - compare data columns with parameters
  if (!is.null(scenario)) {
    scenario <- stringr::str_trim(as.character(scenario))
    result <- result %>% filter(as.character(.data$scenario) == !!scenario)
  }
  if (!is.null(region)) {
    region <- stringr::str_trim(as.character(region))
    result <- result %>% filter(as.character(.data$region) == !!region)
  }
  if (!is.null(period)) {
    period <- as.integer(period)
    result <- result %>% filter(.data$period == !!period)
  }

  # Rename and join with factors
  result <- result %>%
    select("model", "scenario", "region", "period", "variable", "unit", "value") %>%
    distinct() %>%
    rename("piam_variable" = "variable", "piam_unit" = "unit") %>%
    left_join(
      remind_vars_for_parent %>%
        select(.data$piam_variable_clean, .data$piam_factor) %>%
        rename(piam_variable = piam_variable_clean),
      by = "piam_variable"
    ) %>%
    select("model", "scenario", "region", "period", "piam_variable", "piam_unit", "value", "piam_factor")

  result
}
