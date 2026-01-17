#' Process CPRD Data for Polypharmacy Analysis
#'
#' This function integrates and cleans multiple CPRD tables to prepare data
#' for polypharmacy analysis. It joins drug issue data with patient demographics,
#' product information, practice details, and staff information.
#'
#' @param drug_issue Data frame containing drug prescription records from CPRD
#' @param patient Data frame containing patient demographic and registration data
#' @param product_dict Data frame containing drug product dictionary with BNF codes
#' @param practice Data frame containing practice information
#' @param staff Data frame containing staff/prescriber information
#' @param default_duration Numeric. Default prescription duration in days when
#'   duration is missing or zero (default: 28)
#' @param grace_period Numeric. Grace period in days for treatment episode
#'   continuity (default: 30)
#'
#' @return A processed data frame with integrated CPRD data, cleaned dates,
#'   and calculated prescription end dates
#'
#' @details
#' The function performs the following operations:
#' \itemize{
#'   \item Filters to include only acceptable patients
#'   \item Joins drug issues with patient demographics and product information
#'   \item Resolves practice ID conflicts between tables
#'   \item Cleans and converts date fields
#'   \item Applies default duration for missing prescription durations
#'   \item Calculates prescription end dates
#'   \item Orders data for subsequent episode detection
#' }
#'
#' @examples
#' \dontrun{
#' processed_data <- process_cprd_data(
#'   drug_issue = drug_issue,
#'   patient = patient,
#'   product_dict = product_dict,
#'   practice = practice,
#'   staff = staff,
#'   default_duration = 28,
#'   grace_period = 30
#' )
#' }
#'
#' @importFrom dplyr inner_join left_join mutate filter select rename arrange group_by ungroup
#' @importFrom lubridate ymd days
#' @importFrom magrittr %>%
#'
#' @export
process_cprd_data <- function(drug_issue, patient, product_dict, practice, staff,
                              default_duration = 28, grace_period = 30) {

  # Multi-table integration and cleaning
  processed_data <- drug_issue %>%
    # Filter to acceptable patients and join core tables
    dplyr::inner_join(patient %>% dplyr::filter(acceptable == "True"), by = "patid") %>%

    # Convert prodcodeid to character to match product dictionary
    dplyr::mutate(prodcodeid = as.character(prodcodeid)) %>%

    # Join with product dictionary for drug information
    dplyr::left_join(product_dict, by = c("prodcodeid" = "ProdCodeId")) %>%

    # Resolve practice ID conflicts
    dplyr::rename(
      pracid_drug = pracid.x,
      pracid_patient = pracid.y
    ) %>%
    dplyr::mutate(pracid = pracid_drug) %>%

    # Join practice and staff data
    dplyr::left_join(practice, by = "pracid") %>%
    dplyr::left_join(staff, by = "staffid", suffix = c("", "_staff")) %>%

    # Clean up columns
    dplyr::select(-pracid_drug, -pracid_patient) %>%
    dplyr::rename(pracid_staff = pracid_staff) %>%

    # Clean dates and durations
    dplyr::mutate(
      issuedate = lubridate::ymd(issuedate),
      duration = ifelse(is.na(duration) | duration <= 0, default_duration, duration),
      end_date = issuedate + lubridate::days(duration),
      grace_period = grace_period
    ) %>%

    # Order for episode detection
    dplyr::arrange(patid, prodcodeid, issuedate) %>%
    dplyr::group_by(patid, prodcodeid) %>%
    dplyr::mutate(row_id = dplyr::row_number()) %>%
    dplyr::ungroup()

  return(processed_data)
}
