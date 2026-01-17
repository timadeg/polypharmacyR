#' Load Sample CPRD Data
#'
#' Convenience function to load the sample CPRD datasets included with the package.
#' These are synthetic datasets based on UK CPRD structure for testing and examples.
#'
#' @param table_name Character. Name of the table to load. Options are:
#'   "drug_issue", "patient", "product_dict", "practice", "staff", "observation",
#'   "consultation", "problem", "referral", or "all" to load all tables.
#'
#' @return If table_name is specified, returns a data frame. If table_name is "all",
#'   returns a named list containing all CPRD tables.
#'
#' @examples
#' \dontrun{
#' # Load individual tables
#' drug_issue <- load_cprd_data("drug_issue")
#' patient <- load_cprd_data("patient")
#'
#' # Load all tables at once
#' cprd_data <- load_cprd_data("all")
#' drug_issue <- cprd_data$drug_issue
#' }
#'
#' @export
load_cprd_data <- function(table_name = "all") {

  # Define available tables and their file names
  table_mapping <- list(
    "drug_issue" = "DrugIssue",
    "patient" = "Patient",
    "product_dict" = "ProductDictionary",
    "practice" = "Practice",
    "staff" = "Staff",
    "observation" = "Observation",
    "consultation" = "Consultation",
    "problem" = "Problem",
    "referral" = "Referral"
  )

  available_tables <- names(table_mapping)

  if (table_name == "all") {
    # Load all tables and return as named list
    result <- list()
    for (table in available_tables) {
      file_name <- table_mapping[[table]]
      file_path <- system.file("extdata", paste0(file_name, ".csv"), package = "polypharmacyR")
      if (file.exists(file_path)) {
        result[[table]] <- utils::read.csv(file_path)
        message("Loaded ", table, " from ", file_name, ".csv (", nrow(result[[table]]), " rows)")
      } else {
        message("Warning: ", file_name, ".csv not found in extdata")
      }
    }
    return(result)

  } else {
    # Load specific table
    if (!table_name %in% available_tables) {
      stop("table_name must be one of: ", paste(available_tables, collapse = ", "), " or 'all'")
    }

    file_name <- table_mapping[[table_name]]
    file_path <- system.file("extdata", paste0(file_name, ".csv"), package = "polypharmacyR")
    if (!file.exists(file_path)) {
      stop("File ", file_name, ".csv not found in package extdata folder")
    }

    data <- utils::read.csv(file_path)
    message("Loaded ", table_name, " from ", file_name, ".csv (", nrow(data), " rows)")
    return(data)
  }
}
