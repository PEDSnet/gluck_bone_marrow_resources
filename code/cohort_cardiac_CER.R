#' Get all cardiac records for cohort
#'
#' @param cohort cohort of patients of interest
#' @param pmca_codeset codeset from PMCA with body_system identifier to find
#'                     cardiac body system codes
#' @param endpoint_tbl table with endpoint dates for all patients in cohort
#'
#' @return data frame with relevant cardiac diagnosis for patients in the cohort
#'         and the associated date
#' 
get_cardiac_records <- function(cohort,
                                pmca_codeset,
                                endpoint_tbl) {
  
  cohort_w_endpoints <- cohort %>%
    left_join(select(endpoint_tbl, patid, endpoint_date), by = "patid")
  
  dx_cardiac_pmca = pmca_codeset %>%
    filter(body_system == "cardiac") %>%
    compute_new()
  
  cardiac_records <- get_dx_conds(cohort = cohort,
                                  dx_codeset = dx_cardiac_pmca) %>%
    compute_new()
  
  cardiac_records %>%
    inner_join(select(cohort_w_endpoints, patid, endpoint_date), by = "patid") %>%
    mutate(date_after_endpoint = if_else(!is.na(endpoint_date) &
                                           dx_cond_date > endpoint_date,
                                         TRUE, FALSE)) %>%
    filter(date_after_endpoint == FALSE)
  
}

#' Get cohort of cardiac patients
#'
#' @param cohort cohort of all patients of interest
#' @param cardiac_records table output by `get_cardiac_records`
#'
#' @return dataframe with all patients who meet the cardiac
#'         phenotype criteria
#' 
get_cardiac_cohort <- function(cohort,
                               cardiac_records) {
  cardiac_cohort <- cardiac_records %>%
    group_by(patid) %>%
    summarize(
      min_date = min(dx_cond_date, na.rm = TRUE),
      max_date = max(dx_cond_date, na.rm = TRUE),
      days_sep = max(dx_cond_date, na.rm = TRUE) - min(dx_cond_date, na.rm = TRUE),
      n_dates = n_distinct(dx_cond_date)
    ) %>%
    ungroup() %>%
    # filter(days_sep >= 90) %>% # should we require multiple separated by X days?
    compute_new(indexes = list("patid"))
  
  cardiac_cohort %>% return()
  
}