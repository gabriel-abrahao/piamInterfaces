#' Extract N-level prefix from pipe-separated variable name
#'
#' @param variable Character string with pipe-separated variable name
#' @param n Number of levels to extract (1 = first level, 2 = first two levels, etc.)
#'
#' @return Character string with first n levels joined by pipes
#'
#' @keywords internal
#' @export
getPipePrefix <- function(variable, n = 2) {
  parts <- strsplit(variable, "\\|")[[1]]
  if (length(parts) < n) {
    return(variable)
  }
  paste(parts[1:n], collapse = "|")
}


#' Get unique N-level prefixes from a vector of variable names
#'
#' @param variables Character vector of pipe-separated variable names
#' @param n Number of levels to extract
#'
#' @return Character vector of unique prefixes
#'
#' @keywords internal
#' @export
getUniquePrefixes <- function(variables, n = 2) {
  prefixes <- sapply(variables, getPipePrefix, n = n, USE.NAMES = FALSE)
  unique(prefixes)
}


#' Find REMIND MIF variables matching given prefixes
#'
#' @param remind_data Quitte data frame with REMIND data (pre-loaded)
#' @param prefixes Character vector of variable prefixes to match
#' @param scenario Scenario to filter by
#' @param region Region to filter by
#' @param period Period to filter by
#'
#' @return Data frame with columns: variable, value
#'
#' @keywords internal
#' @export
findMatchingRemindVariables <- function(remind_data, prefixes, scenario = NULL, region = NULL, period = NULL) {

  if (length(prefixes) == 0) {
    return(data.frame(variable = character(), value = numeric(), stringsAsFactors = FALSE))
  }

  # Convert to dataframe if needed
  result <- as.data.frame(remind_data)

  # Filter by context
  if (!is.null(scenario)) {
    scenario <- stringr::str_trim(as.character(scenario))
    result <- result %>% dplyr::filter(stringr::str_trim(as.character(.data$scenario)) == !!scenario)
  }
  if (!is.null(region)) {
    region <- stringr::str_trim(as.character(region))
    result <- result %>% dplyr::filter(stringr::str_trim(as.character(.data$region)) == !!region)
  }
  if (!is.null(period)) {
    period <- as.integer(period)
    result <- result %>% dplyr::filter(.data$period == !!period)
  }

  # Find variables matching any of the prefixes
  matching <- data.frame(variable = character(), value = numeric(), stringsAsFactors = FALSE)

  for (prefix in prefixes) {
    # Create regex pattern: match variables starting with prefix followed by | or end of string
    pattern <- sprintf("^%s(\\||$)", stringr::str_escape(prefix))
    prefix_matches <- result %>%
      dplyr::filter(grepl(pattern, .data$variable)) %>%
      dplyr::select("variable", "value") %>%
      dplyr::distinct()

    if (nrow(prefix_matches) > 0) {
      matching <- dplyr::bind_rows(matching, prefix_matches)
    }
  }

  # Remove duplicates and sort by value descending
  matching <- matching %>%
    dplyr::distinct() %>%
    dplyr::arrange(dplyr::desc(.data$value))

  matching
}
