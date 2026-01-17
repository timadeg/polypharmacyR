#' Detect Treatment Episodes from CPRD Prescription Data
#'
#' This function identifies continuous treatment episodes from prescription
#' records by detecting gaps between prescriptions that exceed a specified
#' grace period. It groups consecutive prescriptions into episodes and
#' calculates episode-level summaries.
#'
#' @param processed_data Data frame containing processed CPRD prescription data
#'   from \code{process_cprd_data()}. Must contain columns: patid, prodcodeid,
#'   issuedate, end_date, and grace_period.
#'
#' @return A data frame containing treatment episodes with the following columns:
#' \describe{
#'   \item{patid}{Patient identifier}
#'   \item{prodcodeid}{Product code identifier}
#'   \item{episode_id}{Sequential episode number for each patient-drug combination}
#'   \item{episode_start}{Date of first prescription in episode}
#'   \item{episode_end}{Date when last prescription in episode expires}
#'   \item{n_prescriptions}{Number of prescriptions in the episode}
#'   \item{episode_duration}{Duration of episode in days}
#' }
#'
#' @details
#' Treatment episodes are defined as continuous periods of drug exposure,
#' allowing for gaps up to the grace period between prescriptions. A new
#' episode begins when the gap between the end of one prescription and the
#' start of the next exceeds the grace period.
#'
#' The function performs the following steps:
#' \itemize{
#'   \item Calculates gaps between consecutive prescriptions for each patient-drug
#'   \item Flags new episodes when gaps exceed the grace period
#'   \item Assigns sequential episode IDs
#'   \item Summarizes episodes with start/end dates, prescription counts, and duration
#' }
#'
#' @examples
#' \dontrun{
#' # First process the data
#' processed_data <- process_cprd_data(drug_issue, patient, product_dict,
#'                                    practice, staff)
#'
#' # Then detect episodes
#' episodes <- detect_treatment_episodes(processed_data)
#' }
#'
#' @importFrom dplyr mutate lag if_else group_by summarise n
#' @importFrom magrittr %>%
#'
#' @export
detect_treatment_episodes <- function(processed_data) {

  treatment_episodes <- processed_data %>%
    # Flag breaks in continuity between prescriptions
    dplyr::mutate(
      prev_end = dplyr::lag(end_date),
      gap = as.numeric(issuedate - prev_end),
      new_episode = dplyr::if_else(is.na(gap) | gap > grace_period, 1, 0)
    ) %>%

    # Assign episode IDs
    dplyr::group_by(patid, prodcodeid) %>%
    dplyr::mutate(episode_id = cumsum(new_episode)) %>%

    # Create episode summaries
    dplyr::group_by(patid, prodcodeid, episode_id) %>%
    dplyr::summarise(
      episode_start = min(issuedate, na.rm = TRUE),
      episode_end = max(end_date, na.rm = TRUE),
      n_prescriptions = dplyr::n(),
      .groups = "drop"
    ) %>%

    # Calculate episode duration
    dplyr::mutate(episode_duration = as.numeric(episode_end - episode_start + 1))

  return(treatment_episodes)
}
