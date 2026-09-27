#' List all unique equations in a summation file
#'
#' Extracts the unique parent equation names from a summation CSV file.
#' Equations are identified by the parent column.
#'
#' @param summationsFile Path to summation file or data frame
#'
#' @return Character vector of unique parent equations (with number suffixes if present)
#'
#' @export
listEquationsInSummationFile <- function(summationsFile) {
  if (is.character(summationsFile)) {
    summations <- read.csv(summationsFile, sep = ";", stringsAsFactors = FALSE)
  } else {
    summations <- summationsFile
  }

  unique_parents <- unique(summations$parent)
  unique_parents <- unique_parents[!is.na(unique_parents) & unique_parents != ""]
  sort(unique_parents)
}


#' Get the clean parent variable name for an equation
#'
#' Removes any space-separated number suffix from an equation name.
#' For example: "Emissions|N2O 2" -> "Emissions|N2O"
#'
#' @param equation Equation name (possibly with number suffix)
#'
#' @return Clean parent variable name without number suffix
#'
#' @export
getParentVariable <- function(equation) {
  gsub(" [0-9]+$", "", equation)
}


#' Get PIAM child variables for a given equation
#'
#' Returns the list of PIAM child variables that should sum to the given equation,
#' as defined in the summation file.
#'
#' @param equation Equation name (possibly with number suffix)
#' @param summationsFile Path to summation file or data frame
#'
#' @return Character vector of child PIAM variable names, or character(0) if equation not found
#'
#' @export
getPiamChildrenForEquation <- function(equation, summationsFile) {
  if (is.character(summationsFile)) {
    summations <- read.csv(summationsFile, sep = ";", stringsAsFactors = FALSE)
  } else {
    summations <- summationsFile
  }

  summation_rules <- summations %>%
    filter(.data$parent == equation &
           !is.na(.data$child) & .data$child != "")

  if (nrow(summation_rules) == 0) {
    return(character(0))
  }

  unique(summation_rules$child)
}
