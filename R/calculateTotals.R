#' Calculate REMIND and PIAM totals with debugging
#'
#' @param diag Diagnostic element from diagnoseSummationErrors
#' @return List with calculation details and totals
#'
#' @keywords internal
#' @export
calculateRemindTotal <- function(diag) {

  remind_data <- diag$remind_data

  # Show what we're working with
  result <- list(
    input_rows = nrow(remind_data),
    variables = remind_data %>%
      select(piam_variable, value, piam_factor) %>%
      mutate(
        factor_used = ifelse(is.na(piam_factor), 1, piam_factor),
        adjusted_value = value * factor_used,
        is_na_value = is.na(value),
        is_na_factor = is.na(piam_factor)
      )
  )

  # Calculate total
  result$total_with_na_as_1 <- sum(
    remind_data$value * ifelse(is.na(remind_data$piam_factor), 1, remind_data$piam_factor),
    na.rm = TRUE
  )

  result$total_with_na_as_zero <- sum(
    remind_data$value * ifelse(is.na(remind_data$piam_factor), 1, remind_data$piam_factor),
    na.rm = FALSE
  )

  # Show which rows have NA values
  result$rows_with_na_value <- remind_data %>%
    filter(is.na(value)) %>%
    select(piam_variable, value, piam_factor)

  result$rows_with_na_factor <- remind_data %>%
    filter(is.na(piam_factor)) %>%
    select(piam_variable, value, piam_factor)

  return(result)
}


#' Calculate PIAM total with debugging
#'
#' @param diag Diagnostic element
#' @return List with calculation details
#'
#' @keywords internal
#' @export
calculatePiamTotal <- function(diag) {

  piam_data <- diag$piam_data

  result <- list(
    input_rows = nrow(piam_data),
    variables = piam_data %>%
      select(variable, value) %>%
      mutate(is_na = is.na(value))
  )

  result$total_ignoring_na <- sum(piam_data$value, na.rm = TRUE)
  result$total_with_na <- sum(piam_data$value, na.rm = FALSE)

  result$rows_with_na <- piam_data %>%
    filter(is.na(value)) %>%
    select(variable, value)

  return(result)
}
