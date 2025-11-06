# Vector of additional packages to load before executing the request
config_append('extra_packages', c())
require('RPostgres')

# Global driver control parameters ----
# Defaulted to Clinical insight report CKD criteria. Modifications here will reflect in the final output

## Age Criteria ----
age_min = 1L
age_max = 25L

## Date Range ----
date_min = '2009-01-01'
date_max = lubridate::today()

## eGFR range ----
egfr_min = 0L
egfr_max = 90L

## Beep Notification, will only work if namespace beepr is fulfilled
notify = T


# Materializing temp tables on result schema | Boolean parameters only ----
## Each step follows an abbreviated letter S and a number of the step as mentioned above
#' By convention only the last step materializes a table named `ckd_cohort` which can be accessed using  `results_tbl('ckd_cohort')`
#' Note that a run will overwrite the `ckd_cohort` table. Should you want to prevent overwriting the table for exploratory purposes,
#' please make sure to set the `S8 <- FALSE` below.
S1 <- FALSE
S2 <- FALSE
S3 <- FALSE
S4 <- FALSE
S5 <- TRUE
#S6 <- FALSE
#S7 <- FALSE
#S8 <- TRUE
attr_log <- TRUE



#' Execute the request
#'
#' This function presumes the environment has been set up, and executes the
#' steps of the ckd phenotype
#'
#'
#' @return a list of all the temp tables of the steps mentioned above
#' @md
.run  <- function() {
  start_time <- Sys.time()
  
  setup_pkgs() # Load runtime packages as specified above
  
  message('Starting execution with framework version ',
          config('framework_version'))
  
  
  rslt <- list()
  
  # Step 1: Getting patients with >=1 in-person visit between 2009-01-01 and YTD --------------------------------------------------------------------------------
  
  ### Message ----
  message('\nComputing Step 1: ', paste0('Patients with >=1 in-person visit', '\n\nTime-period: ', date_min, '- ', date_max, '\n\nAge-range: ',age_min, '- ',age_max))
  
  #### Query----
  rslt$inperson_visits <-
    get_inperson_visit(
      in_person_visit = load_codeset('in_person'),
      date_min = date_min,
      date_max = date_max
    ) %>%
    compute_new(indexes = list('person_id'))
  
  ### attrition log ----
  
  rslt$ckd_log <- attr_log_by_site(
    qry_tbl = rslt$inperson_visits,
    cohort = paste0('Patients with >=1 in-person visit'),
    init = T
  )
  
  if (S1 == T) {
    output_tbl(rslt$inperson_visits, "inperson_visit")
  }
  
  # Step 2:  Exclude patients with no in-person visits with a nephrology provider or facility  -----------------------------------------------------------------
  
  
  ### Message ----
  message('\nComputing Step 2: ', paste0('Excluding patients with no in-person visits with a nephrology provider or facility', '\n\nTime-period: ', date_min, '- ', date_max, '\n\nAge-range: ',age_min, '- ',age_max))
  
  
  ### Query ----
  rslt$inperson_visit_with_neph_spec <-
    get_cohort_with_neph_inperson_spec(
      in_person_codeset = load_codeset('in_person'),
      neph_spec_codeset = load_codeset('nephrology_specialty'),
      cohort = rslt$inperson_visits,
      visit_start = date_min,
      visit_end = date_max
    )
  
  
  ### attrition log ----
  rslt$ckd_log <-
    attr_log_by_site(
      qry_tbl = rslt$inperson_visit_with_neph_spec,
      cohort = paste0(
        'Excluding patients with no in-person visits with a nephrology provider or facility'),
      init = F
    )
  
  if (S2 == T) {
    output_tbl(rslt$inperson_visit_with_neph_spec,
               "inperson_visit_with_neph_spec")
  }
  
  # Step 3: Getting patients with >=1 serum creatinine measurement ------------------------------------------------------------------------------------
  ### Message ----
  message('\nComputing Step 3: ', paste0('Patient with >=1 serum creatinine measurement', '\n\nTime-period: ', date_min, '- ', date_max, '\n\nAge-range: ',age_min, '- ',age_max))
  
  
  
  ### Query ----
  rslt$cohort_serum_creatinine <-
    obtain_serum_creatinine(
      cohort = rslt$inperson_visit_with_neph_spec,
      date_min = date_min,
      date_max = date_max,
      age_min =  age_min,
      age_max =  age_max,
      ser_creat_codeset = load_codeset('serum_creatinine')
    )
  
  ### attrition log ----
  rslt$ckd_log <- attr_log_by_site(
    qry_tbl = rslt$cohort_serum_creatinine,
    cohort = paste0('Patient with >=1 serum creatinine measurement'),
    init = F
  )
  
  if (S3 == T) {
    output_tbl(rslt$cohort_serum_creatinine, "ckd_cohort_serum_creatinine")
  }
  
  
  # Step 4: Getting patients with height measurement available <=180 days of serum creatinine value  ------------------------------------------------------------------------------------
  
  ### Message ----
  message('\nComputing Step 4: ', paste0('Patient meeting height criteria', '\n\nTime-period: ', date_min, '- ', date_max, '\n\nAge-range: ',age_min, '- ',age_max))
  
  ### Query ----
  rslt$cohort_heights <-
    get_heights(serum_creatinine_tbl = rslt$cohort_serum_creatinine)
  
  ### attrition log ----
  rslt$ckd_log <- attr_log_by_site(qry_tbl = rslt$cohort_heights,
                                   cohort = paste0('Patient meeting height criteria'),
                                   init = F)
  
  if (S4 == T) {
    output_tbl(rslt$cohort_heights, "cohort_heights")
  }
  
  
  # Step 5:  Computing egfr values for Patients with height measurement available <=180 days of serum creatinine value  ------------------------------------------------------------------------------------
  ### Message ---- 
  message('\nComputing egfr values: ', paste0('Patient meeting height criteria', '\n\nTime-period: ', date_min, '- ', date_max, '\n\nAge-range: ',age_min, '- ',age_max))
  
  ### Query ----
  rslt$egfr_data <-
    compute_egfr_data(
      cohort_heights = rslt$cohort_heights,
      cohort_ser_creat = rslt$cohort_serum_creatinine,
      conversion_factor = 0.01,
      age_sex_bounds  =  load_codeset("age_sex_bounds0_25",
                                      indexes = list("age"),
                                      col_types = "ciddddd")
    )
  
  ### attrition log ----
  rslt$ckd_log <- attr_log_by_site(qry_tbl = rslt$cohort_heights,
                                   cohort = paste0('Patient egfr within the age-sex bounds'),
                                   init = F)
  
  if (S5 == T) {
    output_tbl(rslt$egfr_data, "egfr_data")
  }
  
  return(rslt)
  
}