#' Apply Mapping to REMIND Data
#'
#' Transforms REMIND data to PIAM variables by applying a mapping definition.
#' This is a utility function that extracts the core mapping transformation logic,
#' allowing mappings to be applied programmatically without generating files.
#'
#' @param remindData A quitte object or data frame with columns: model, scenario, region, period, variable, unit, value
#' @param mapping A data frame or path to mapping file with columns: variable, unit, piam_variable, piam_unit, piam_factor
#'                If NULL, the user is prompted to choose from available mappings
#' @param model Optional model name to set/override in output data
#' @param scenario Optional scenario name pattern/replacement (not yet implemented)
#' @param timesteps Vector of periods to include (default: all periods in input data)
#' @param naAction Function to handle NA values during aggregation (default: "na.omit")
#'
#' @return A quitte object or data frame with transformed data using PIAM variables
#'
#' @details
#' The function performs the following steps:
#' 1. Loads the mapping definition (from file, memory, or user selection)
#' 2. Renames REMIND variables to piam_variable names
#' 3. Joins with mapping to get transformation factors
#' 4. Applies piam_factor to each value
#' 5. Aggregates by (model, scenario, region, period, variable, unit)
#'
#' @examples
#' \dontrun{
#' # Apply a mapping to REMIND data
#' remind_data <- readMifs("path/to/remind/output.mif")
#' piam_data <- applyMappingToData(remind_data, mapping = "AR6")
#'
#' # Or with a custom mapping data frame
#' custom_mapping <- data.frame(
#'   variable = "Emissions|CO2",
#'   unit = "Mt CO2/yr",
#'   piam_variable = "Emissions|CO2|Energy",
#'   piam_unit = "Mt CO2/yr",
#'   piam_factor = 1
#' )
#' piam_data <- applyMappingToData(remind_data, mapping = custom_mapping)
#' }
#'
#' @keywords internal
#' @export
#' @importFrom quitte as.quitte quitteSort
#' @importFrom dplyr filter mutate select bind_rows inner_join distinct .data
#' @importFrom stringr str_trim
applyMappingToData <- function(remindData, mapping = NULL, model = NULL, scenario = NULL,
                               timesteps = NULL, naAction = "na.omit") {

  # Ensure input is a quitte object / data frame with required columns
  if (!is.data.frame(remindData)) {
    stop("remindData must be a data frame or quitte object")
  }

  required_cols <- c("model", "scenario", "region", "period", "variable", "unit", "value")
  if (!all(required_cols %in% colnames(remindData))) {
    missing <- setdiff(required_cols, colnames(remindData))
    stop("remindData is missing required columns: ", paste(missing, collapse = ", "))
  }

  message("# Applying mapping to REMIND data...")

  # Load mapping
  if (is.null(mapping)) {
    message("# No mapping provided, prompting user selection...")
    mapping <- gms::chooseFromList(names(piamInterfaces::mappingNames()), type = "mappings", addAllPattern = FALSE)
  }

  if (is.character(mapping)) {
    message("# Loading mapping from file: ", mapping)
    mapData <- getMapping(mapping) %>%
      filter(!.data$piam_variable %in% "", !is.na(.data$piam_variable)) %>%
      mutate(
        "piam_variable" = removePlus(.data$piam_variable),
        "piam_factor" = ifelse(is.na(.data$piam_factor), 1, as.numeric(.data$piam_factor))
      )
  } else if (is.data.frame(mapping)) {
    message("# Using provided mapping data frame")
    mapData <- mapping %>%
      mutate(
        "piam_variable" = removePlus(.data$piam_variable),
        "piam_factor" = ifelse(is.na(.data$piam_factor), 1, as.numeric(.data$piam_factor))
      )
  } else {
    stop("mapping must be a character path, NULL, or a data frame")
  }

  # Validate mapping structure
  required_mapping_cols <- c("variable", "unit", "piam_variable", "piam_unit", "piam_factor")
  if (!all(required_mapping_cols %in% colnames(mapData))) {
    missing <- setdiff(required_mapping_cols, colnames(mapData))
    stop("Mapping is missing required columns: ", paste(missing, collapse = ", "))
  }

  # Prepare data: filter timesteps and clean variables
  mifdata <- remindData %>%
    as.data.frame()

  if (!is.null(timesteps)) {
    mifdata <- mifdata %>% filter(.data$period %in% timesteps)
  }

  # Keep the original variable/unit names as piam_variable for joining with mapping
  mifdata <- mifdata %>%
    mutate(
      "piam_variable" = str_trim(.data$variable),
      "piam_unit_data" = str_trim(.data$unit)
    ) %>%
    select("model", "scenario", "region", "period", "piam_variable", "piam_unit_data", "value") %>%
    distinct()

  # Apply mapping: join REMIND variables with PIAM targets
  submission <- mifdata %>%
    inner_join(mapData, by = "piam_variable", relationship = "many-to-many") %>%
    # Verify unit consistency (warn if different)
    mutate(
      "unit_mismatch" = .data$piam_unit_data != .data$piam_unit,
      "unit" = .data$piam_unit  # Use mapping's unit for output
    )

  # Warn about unit mismatches
  mismatches <- submission %>%
    filter(.data$unit_mismatch) %>%
    select("variable", "unit") %>%
    distinct()

  if (nrow(mismatches) > 0) {
    warning("Unit mismatches found between REMIND data and mapping. ",
            "Using mapping units for output. Check your data and mapping.\n",
            "Affected variables: ",
            paste(unique(mismatches$variable), collapse = ", "))
  }

  # Continue with transformation and final selection
  submission <- submission %>%
    # Apply transformation factor
    mutate("value" = .data$piam_factor * .data$value) %>%
    # Select final columns
    select("model", "scenario", "region", "period", "variable", "unit", "value")

  # Aggregate: sum by dimensions
  submission <- aggregate(
    value ~ model + region + scenario + period + variable + unit,
    data = submission,
    FUN = "sum",
    na.action = naAction
  )

  # Apply model override if provided
  if (!is.null(model)) {
    submission <- submission %>%
      mutate("model" = model)
  }

  # Sort and return as quitte if possible
  submission <- quitteSort(submission)
  message(sprintf("# Mapping applied. Generated %d rows of PIAM data", nrow(submission)))

  return(submission)
}
