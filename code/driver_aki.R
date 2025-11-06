# Vector of additional packages to load before executing the request
config_append("extra_packages", c("tidyverse"))

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
.run <- function() {
  setup_pkgs() # Load runtime packages as specified above
  
  
  attrition <- NULL
  
  message("Getting Inpatient Visits")
  
  tmp_inpatient_visits <-
    cdm_tbl("visit_occurrence") %>%
    filter(
      visit_concept_id %in% c(9201L, 2000001532L, 2000000048L, 2000000088L),
      visit_start_date >= "2009-01-01", visit_start_date <= "2023-12-31"
    ) %>%
    dplyr::select(person_id, visit_occurrence_id, visit_start_date, visit_concept_id, site) %>%
    output_tbl("tmp_inpatient_visits") #use output tbl to save time, can delete later.
  
  tmp_attrition <-
    results_tbl("tmp_inpatient_visits") %>%
    group_by(site) %>%
    summarise(n = n_distinct(person_id), n_encounter = n()) %>%
    collect_new() %>%
    mutate(step = 1, step_name = "inpatient visits 2009-2023")
  
  attrition <- bind_rows(attrition, tmp_attrition)
  
  attrition %>% output_tbl("running_attrition")
  
  
  message("Getting Creatinine during inpatient visits")
  
  serum_creatinine <- load_codeset("lab_serum_creatinine")
  
  serum_creatinine_codes <-
    serum_creatinine %>%
    distinct(concept_id) %>%
    pull()
  
  serum_creatinine_labs <-
    cdm_tbl("measurement_labs") %>%
    inner_join(results_tbl("tmp_inpatient_visits") %>% distinct(person_id)) %>%
    filter(measurement_concept_id %in% serum_creatinine_codes) %>%
    output_tbl("serum_creatinine_labs")
  
  
  creatinine_limits <- read_codeset("creatinine_limits", col_types = "cdddd")
  
  inpatient_creatinine_labs <-
    results_tbl("tmp_inpatient_visits") %>%
    dplyr::select(visit_occurrence_id, visit_start_date) %>%
    inner_join(results_tbl("serum_creatinine_labs")) %>%
    compute_new(temporary = TRUE) 
  
  
  tmp_attrition <-
    inpatient_creatinine_labs %>%
    group_by(site) %>%
    summarise(n = n_distinct(person_id), n_encounter = n_distinct(visit_occurrence_id)) %>%
    collect_new() %>%
    mutate(step = 2, step_name = "Serum Creatinine lab during Inpatient Encounter")
  
  attrition <- bind_rows(attrition, tmp_attrition)
  
  attrition %>% output_tbl("running_attrition")
  
  tmp_attrition <-
    cdm_tbl("person") %>%
    dplyr::select(person_id, gender_concept_name) %>%
    inner_join(inpatient_creatinine_labs) %>%
    filter(gender_concept_name %in% c("MALE", "FEMALE")) %>%
    group_by(site) %>%
    summarise(n = n_distinct(person_id), n_encounter = n_distinct(visit_occurrence_id)) %>%
    collect_new() %>%
    mutate(step = 3, step_name = "Sex either Male or Female")
  
  attrition <- bind_rows(attrition, tmp_attrition)
  
  attrition %>% output_tbl("running_attrition")
  
  
  message("Checking Creatinine Levels")
  
  
  check_limits <-
    cdm_tbl("person") %>%
    dplyr::select(person_id, gender_concept_name) %>%
    inner_join(inpatient_creatinine_labs) %>%
    inner_join(creatinine_limits, copy = TRUE) %>%
    filter(measurement_age_in_months >= age_ge_months, measurement_age_in_months < age_lt_months, value_as_number > 0.5) %>%
    mutate(stage = case_when(
      value_as_number > 4 ~ 3,
      value_as_number >= 3 * baseline_scr ~ 3,
      value_as_number >= 2 * baseline_scr ~ 2,
      value_as_number >= 1.5 * baseline_scr ~ 1,
      value_as_number >= baseline_scr + 0.3 ~ 1,
      TRUE ~ 0
    )) %>%
    filter(stage > 0) %>%
    output_tbl("check_limits")
  
  
  tmp_attrition <-
    results_tbl("check_limits") %>%
    group_by(site) %>%
    summarise(n = n_distinct(person_id), n_encounter = n_distinct(visit_occurrence_id)) %>%
    collect_new() %>%
    mutate(step = 4, step_name = "meets mKDIGO AKI")
  
  
  attrition <- bind_rows(attrition, tmp_attrition)
  
  attrition %>% output_tbl("running_attrition")
  
  
  message("Getting In-Person Visits")
  
  tmp_inperson_visits <-
    results_tbl("check_limits")  %>%
    distinct(person_id, inpatient_start_date = visit_start_date, inpatient_visit_id = visit_occurrence_id) %>%
    inner_join(cdm_tbl("visit_occurrence")) %>%
    filter(
      visit_concept_id %in% c(9201L:9203L, 2000000469L, 2000001532L, 2000000048L, 2000000088L)
    ) %>%
    output_tbl("tmp_inperson_visits")
  
  tmp_attrition <-
    results_tbl("tmp_inperson_visits") %>%
    filter(visit_start_date - inpatient_start_date >= 365) %>%
    group_by(site) %>%
    summarise(n = n_distinct(person_id), n_encounter = n_distinct(visit_occurrence_id)) %>%
    collect_new() %>%
    mutate(step = 5, step_name = "in-person visit >= 12 months after most recent qualifying inpatient visit")
  
  attrition <- bind_rows(attrition, tmp_attrition)
  
  attrition %>% output_tbl("running_attrition")
  
  
  message("Getting Weights and Heights")
  
  height_codeset <-
    load_codeset("anthro_height")
  
  weight_codeset <-
    load_codeset("anthro_weight")
  
  weights <-
    cdm_tbl("measurement_anthro") %>%
    inner_join(weight_codeset, by = c("measurement_concept_id" = "concept_id")) %>%
    inner_join(tmp_inperson_visits %>% dplyr::select(visit_occurrence_id)) %>%
    distinct(person_id, visit_occurrence_id, weight_date = measurement_date) %>%
    compute_new(temporary = TRUE)
  
  heights <-
    cdm_tbl("measurement_anthro") %>%
    inner_join(height_codeset, by = c("measurement_concept_id" = "concept_id")) %>%
    inner_join(tmp_inperson_visits %>% dplyr::select(visit_occurrence_id)) %>%
    distinct(person_id, visit_occurrence_id, height_date = measurement_date, height = value_as_number) %>%
    compute_new(temporary = TRUE)
  
  check_weight_30months <-
    tmp_inperson_visits %>%
    inner_join(weights) %>%
    filter(
      weight_date - inpatient_start_date < 30 * 365.25 / 12,
      weight_date - inpatient_start_date >= 365
    ) %>%
    compute_new(temporary = TRUE)
  
  
  
  tmp_attrition <-
    check_weight_30months %>%
    group_by(site) %>%
    summarise(n = n_distinct(person_id), n_encounter = n_distinct(inpatient_visit_id)) %>%
    collect_new() %>%
    mutate(step = 6, step_name = "in-person visit with 1+ weight measurement, >= 12 months & < 30 months")
  
  attrition <- bind_rows(attrition, tmp_attrition)
  
  attrition %>% output_tbl("running_attrition")
  
  check_height_60_days <-
    check_weight_30months %>%
    dplyr::select(person_id, weight_date, inpatient_visit_id, visit_occurrence_id, site) %>%
    inner_join(heights) %>%
    filter(abs(weight_date - height_date) <= 60) %>%
    compute_new(temporary = TRUE)
  
  tmp_attrition <-
    check_height_60_days %>%
    group_by(site) %>%
    summarise(n = n_distinct(person_id), n_encounter = n_distinct(inpatient_visit_id)) %>%
    collect_new() %>%
    mutate(step = 7, step_name = "height within 60 days of weight")
  
  attrition <- bind_rows(attrition, tmp_attrition)
  
  attrition %>% output_tbl("running_attrition")
  
  ### egfr
  
  # adapted from "code/cohort_egfr_u25.R"
  # changes are:
  # tables already limited by cohort so join removed
  # labs and anthro saved separately
  # using compute instead of collect
  # creatinine_tbl combined with creatinine_avg
  
  cohort <-
    check_height_60_days %>%
    distinct(person_id) %>%
    inner_join(check_limits) %>%
    group_by(person_id) %>%
    summarise(ced_date = max(visit_start_date)) %>%
    add_site() %>%
    compute_new(temporary = TRUE)
  
  message("Getting and saving eGFR")
  
  
  max_diff_days <- 180 # from DAP, 60 in original code
  
  
  # if more than one creatinine measurement on a given day, take mean
  
  creatinine_avg <- 
    cohort %>%
    dplyr::select(person_id) %>%
    inner_join(serum_creatinine_labs) %>%
    rename(
      scr_date = measurement_date,
      serum_creatinine = value_as_number
    ) %>%
    group_by(person_id, scr_date) %>%
    summarise(serum_creatinine = mean(serum_creatinine)) %>%
    ungroup() %>%
    compute_new(temporary = TRUE)
  
  
  # link up creatinine and heights, check for difference and if multiple eligible
  # heights, pick closest
  heights_scr <- creatinine_avg %>%
    filter(serum_creatinine > 0) %>%
    inner_join(heights, by = "person_id") %>%
    mutate(diff_days = as.numeric(scr_date - height_date)) %>%
    mutate(abs_diff = abs(diff_days)) %>%
    filter(abs_diff <= max_diff_days) %>%
    group_by(person_id, scr_date) %>%
    window_order(abs_diff, diff_days) %>% # changed from arrange since on db end
    filter(row_number() == 1) %>%
    ungroup() %>%
    compute_new(temporary = TRUE)
  
  dem_info <- cdm_tbl("person") %>%
    select(
      person_id,
      birth_date,
      gender_concept_id
    ) %>%
    mutate(
      sex = case_when(
        gender_concept_id == 8532 ~ "F",
        gender_concept_id == 8507 ~ "M",
        TRUE ~ "O"
      )
    ) %>%
    compute_new(temporary = TRUE)
  
  egfr <- heights_scr %>%
    inner_join(dem_info, by = "person_id") %>%
    mutate(age_yrs_cont = as.numeric((scr_date - birth_date)) / 365.25) %>%
    mutate(
      k_coeff_exact = case_when(
        sex == "M" &&
          age_yrs_cont >= 1 &&
          age_yrs_cont < 12 ~ 39.0 * (1.008^(age_yrs_cont - 12)),
        sex == "M" &&
          age_yrs_cont >= 12 &&
          age_yrs_cont < 18 ~ 39.0 * (1.045^(age_yrs_cont - 12)),
        sex == "M" &&
          age_yrs_cont >= 18 ~ 50.8,
        sex == "F" &&
          age_yrs_cont >= 1 &&
          age_yrs_cont < 12 ~ 36.1 * (1.008^(age_yrs_cont - 12)),
        sex == "F" &&
          age_yrs_cont >= 12 &&
          age_yrs_cont < 18 ~ 36.1 * (1.023^(age_yrs_cont - 12)),
        sex == "F" &&
          age_yrs_cont >= 18 ~ 41.4
      ),
      height_meters = height / 100,
      egfr = k_coeff_exact * (height_meters / serum_creatinine),
    ) %>%
    select(
      person_id,
      sex,
      age_yrs_cont,
      scr_date,
      serum_creatinine,
      height_date,
      height,
      height_meters,
      diff_days,
      abs_diff,
      k_coeff_exact,
      egfr
    ) %>%
    compute_new(temporary = TRUE)
  
  
  #### get CKD cohort
  
  first_egfr_90 <-
    egfr %>%
    dplyr::select(person_id, egfr, scr_date) %>%
    filter(egfr < 90) %>%
    rename(first_egfr = scr_date) %>%
    compute_new(temporary = TRUE)
  
  second_egfr_90 <-
    egfr %>%
    inner_join(first_egfr_90) %>%
    filter(scr_date - first_egfr >= 90) %>%
    dplyr::select(person_id, egfr, scr_date, first_egfr) %>%
    group_by(person_id, first_egfr) %>%
    summarise(second_egfr = min(scr_date)) %>%
    ungroup() %>%
    compute_new(temporary = TRUE)
  
  check_over_90 <-
    egfr %>%
    filter(egfr >= 90) %>%
    dplyr::select(person_id, test_date = scr_date) %>%
    inner_join(second_egfr_90) %>%
    filter(test_date >= first_egfr, test_date <= second_egfr) %>%
    compute_new(temporary = TRUE)
  
  
  keep_egfr_pair <-
    second_egfr_90 %>%
    anti_join(check_over_90) %>%
    inner_join(cohort) %>%
    filter(first_egfr <= ced_date) %>%
    compute_new(temporary = TRUE)
  
  nephrology_spec <-
    load_codeset("nephrology")
  
  has_nephrology_site <-
    cohort %>%
    dplyr::select(person_id) %>%
    inner_join(cdm_tbl("visit_occurrence")) %>%
    dplyr::select(person_id, care_site_id) %>%
    inner_join(cdm_tbl("care_site")) %>%
    inner_join(nephrology_spec, by = c("specialty_concept_id" = "concept_id")) %>%
    distinct(person_id) %>%
    compute_new(temporary = TRUE)
  
  has_nephrology_provider <-
    cohort %>%
    dplyr::select(person_id) %>%
    inner_join(cdm_tbl("visit_occurrence")) %>%
    dplyr::select(person_id, provider_id) %>%
    inner_join(cdm_tbl("provider")) %>%
    inner_join(nephrology_spec, by = c("specialty_concept_id" = "concept_id")) %>%
    distinct(person_id) %>%
    compute_new(temporary = TRUE)
  
  confirm_ckd <-
    dplyr::union(has_nephrology_provider, has_nephrology_site) %>%
    inner_join(keep_egfr_pair) %>%
    dplyr::select(person_id, first_egfr) %>%
    inner_join(cohort) %>%
    filter(first_egfr <= ced_date) %>%
    dplyr::select(person_id) %>%
    compute_new(temporary = TRUE)
  
  ### save all tables
  
  message("Saving tables")
  
  
  final_cohort <-
    cohort %>%
    anti_join(confirm_ckd) %>%
    compute_new(temporary = TRUE) %>%
    output_tbl("final_cohort")
  
  tmp_attrition <-
    results_tbl("final_cohort") %>%
    dplyr::select(person_id) %>%
    inner_join(check_height_60_days) %>%
    group_by(site) %>%
    summarise(n = n_distinct(person_id), n_encounter = n_distinct(inpatient_visit_id)) %>%
    collect_new() %>%
    mutate(step = 8, step_name = "drop CKD patients")
  
  attrition <- bind_rows(attrition, tmp_attrition)
  
  attrition %>% output_tbl("attrition")
  
}