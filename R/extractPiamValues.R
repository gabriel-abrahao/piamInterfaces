#' Extract PIAM variable values from export file
#'
#' Reads PIAM export file and extracts values for specified variables at specific context
#'
#' @param piam_export_file Path to PIAM export MIF/CSV/XLSX file
#' @param variables Vector of PIAM variable names to extract
#' @param scenario Scenario to filter by (optional)
#' @param region Region to filter by (optional)
#' @param period Period to filter by (optional)
#'
#' @return Data frame with columns: model, scenario, region, period, variable, unit, value
#'
#' @keywords internal
#' @export
extractPiamValues <- function(piam_export_file, variables, scenario = NULL, region = NULL, period = NULL) {

  # Load PIAM data
  if (grepl("\\.xlsx?$", piam_export_file)) {
    piam_all <- read.quitte(piam_export_file)
  } else if (grepl("\\.csv$", piam_export_file)) {
    piam_all <- read.quitte(piam_export_file)
  } else if (grepl("\\.mif$", piam_export_file)) {
    piam_all <- deletePlus(readMifs(piam_export_file))
  } else {
    stop("Unsupported PIAM export file format: ", piam_export_file)
  }

  # Filter by variable
  result <- piam_all %>%
    filter(.data$variable %in% variables)

  # Filter by context if provided
  if (!is.null(scenario)) {
    result <- result %>% filter(.data$scenario == scenario)
  }
  if (!is.null(region)) {
    result <- result %>% filter(.data$region == region)
  }
  if (!is.null(period)) {
    result <- result %>% filter(.data$period == period)
  }

  # Return with standard columns
  result %>%
    select("model", "scenario", "region", "period", "variable", "unit", "value") %>%
    distinct()
}
