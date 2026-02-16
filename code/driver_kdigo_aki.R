# Vector of additional packages to load before executing the request
config_append('extra_packages', c())

#' Execute the request
#'
#' This function presumes the environment has been set up, and executes the
#' steps of the request.
#'
#' In addition to performing queries and analyses, the execution path in this
#' function should include periodic progress messages to the user, and logging
#' of intermediate totals and timing data through [append_sum()].
#'
#' @return The return value is dependent on the content of the request, but is
#'   typically a structure pointing to some or all of the retrieved data or
#'   analysis results.  The value is not used by the framework itself.
#' @md
.run  <- function() {

    setup_pkgs() # Load runtime packages as specified above
  
  message('Starting execution with framework version ',
          config('framework_version'))

  # Set up the step log with as many attrition columns as you need.
  # For example, this call sets up the log with a `persons` count that will be
  # required at each step.
  # By convention, accumulate execution results in a list rather than as
  # independent variables, in order to make returning the entire set easier
 
  
  ##################"KDIGO AKI Criteria"###############################
  require(tidyr)

  # Get the cohort - 
  # The cohort includes patients who have an Inpatient visit or ED to IP visit with an IV fluid infusion, antibiotics and blood culture lab on the same visit (specific to Prompt Bolus Study)
  ## Cohort requires the at least the following columns - 
  #- participant_id - is a unique identifier (visit_occurrence_id can be used here as a replacement or create unique participant_ids as each person can be enrolled multiple times)
  #- person_id 
  #- site
  #- cohort_start_date and cohort_start_datetime (which is basically ED/IP visit_start_date and visit_start_datetime) 
  cohort <- results_tbl('cohort')
  
  
  # Refer to cohort_demographics.R
  
  # Get demographics for patients
  demographics <- get_cohort_demographics(cohort) %>% output_tbl('cohort_demographics')
  
  # Get death records for patients
  death <- get_cohort_death_record(cohort, death_tbl = cdm_tbl('death')) %>% output_tbl('cohort_death')
  
  
  # Capturing patients who have a demographics record ----
  cohort_final <- cohort %>%
    inner_join(demographics %>% select(participant_id), by = "participant_id" )
  
  
  # Refer to cohort_kdigo_aki.R
  
  # KDIGO AKI stage 1, 2 and 3 ----

  # Criteria 1 and 2 ----
  # Get the baseline scr and scr in 7 days of CE
  bscr_scr_7_days <- get_bscr_scr_7_days(cohort = cohort_final,
                                         meas_labs = cdm_tbl("measurement_labs"),
                                         serum_creatinine_codeset = results_tbl('lab_serum_creatinine'),
                                         baseline_scr_values = results_tbl('scr_baseline_values'),
                                         demographics = results_tbl('cohort_demographics')) %>%
    output_tbl('bscr_scr_7_days')
  
  
  #Assign the stages (1, 2 and 3) based on criteria 1 
  aki_stages_criteria1 <-  assign_aki_stages_criteria1(results_tbl('bscr_scr_7_days')) %>%
    output_tbl('aki_stages_criteria1')
  
  
  # Assign the stages (1 and 3) based on criteria 2
  aki_stages_criteria2 <-  assign_aki_stages_criteria2(results_tbl('bscr_scr_7_days')) %>%
    output_tbl('aki_stages_criteria2')
  
  
  
  # Criteria 3 ----
  max_scr_7_days <- get_max_scr_n_days(cohort = cohort_final,
                                 meas_labs = cdm_tbl("measurement_labs"),
                                 serum_creatinine_codeset = results_tbl("lab_serum_creatinine"),
                                 start_day = 0,
                                 end_day = 6) %>%
  output_tbl('max_scr_7_days')
  
  
  
  height_180_days <- get_height_180_days(cohort = cohort_final, 
                                  meas_anthro = cdm_tbl("measurement_anthro"),
                                  height_codeset = results_tbl('height'), 
                                  age_sex_bounds = results_tbl('age_sex_bounds'),
                                  demographics = results_tbl('cohort_demographics')) %>%
    output_tbl('height_180_days')
  
  
  k_coeff <- calculate_k_coeff(demographics = results_tbl('cohort_demographics')) %>%
    output_tbl('k_coeff')
  
  
  egfr_in_7_days <- calculate_egfr_7day_only(cohort = cohort_final, 
                                       max_scr_per_day = results_tbl('max_scr_7_days'),
                                       demographics = results_tbl('cohort_demographics'),
                                       height_in_window = results_tbl('height_180_days'), 
                                       demo_k = results_tbl('k_coeff')) %>%
    output_tbl('egfr_in_7_days')
  
  
  aki_stages_criteria3 <- assign_aki_stages_criteria3(egfr_data = results_tbl('egfr_in_7_days')) %>%
    output_tbl('aki_stages_criteria3')
  
  
  
  # Criteria 4 ----
  
  rrt_procedures_by_day <- get_rrt(cohort = cohort_final,
                                  procedures = cdm_tbl("procedure_occurrence"),
                                  rrt_codeset = results_tbl("proc_rrt_dialysis"),
                                  start_day = 0,
                                  end_day = 6) %>%
    output_tbl('rrt_procedures_by_day')
  
  
  aki_stages_criteria4 <- assign_aki_stages_criteria4(df = results_tbl('rrt_procedures_by_day')) %>%
    #mutate(aki_criteria4_day_6 = as.character("No AKI")) %>%
    output_tbl('aki_stages_criteria4')
  
  
  
  
  # Assign final stages ----
  
  aki_stages_final <- assign_final_aki_stages(cohort = cohort_final,
                                      aki_stages_criteria1 = results_tbl('aki_stages_criteria1'),
                                      aki_stages_criteria2 = results_tbl('aki_stages_criteria2'),
                                      aki_stages_criteria3 = results_tbl('aki_stages_criteria3'),
                                      aki_stages_criteria4 = results_tbl('aki_stages_criteria4'),
                                      death_table = results_tbl('cohort_death')) %>%
    output_tbl('aki_stages_final')
  
  

}