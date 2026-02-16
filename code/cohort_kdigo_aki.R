# Aim 1
# KDIGO AKI STAGES

#' For criteria 1 and 2
#' **Primary use function**
#' @param cohort cohort table with person_id, participant_id, cohort_start_date, cohort_start_datetime, site
#' @param meas_labs measurement_labs cdm tbl
#' @param serum_creatinine_codeset lab_serum_creatinine results table 
#' @param baseline_scr_values hard coded serum creatinine with median value
#' @param demographics to get the cohort sex and age 
#' @return Baseline SCR for 0-6 days with 
#' @colnames participant_id, min_scr_prior, creatinine_median, scr_baseline, day_0 ....day_6

#Example usage: bscr_scr_7_days <- get_bscr_scr_n_days(cohort = get_prompt_cohort(),
#                                                meas_labs = cdm_tbl("measurement_labs"),
#                                                serum_creatinine_codeset = results_tbl('lab_serum_creatinine'),
#                                                baseline_scr_values = results_tbl('scr_baseline_values'),
#                                                demographics = results_tbl('cohort_demographics')) %>%
#   output_tbl('bscr_scr_7_days')

require(tidyr)

get_bscr_scr_7_days <- function(
    cohort,
    meas_labs = cdm_tbl("measurement_labs"),
    serum_creatinine_codeset = results_tbl('lab_serum_creatinine'),
    baseline_scr_values = results_tbl('scr_baseline_values'),
    demographics = results_tbl('cohort_demographics')) {
  
  # Filter serum creatinine labs
  creat_labs <- meas_labs %>%
    inner_join(serum_creatinine_codeset, by = c("measurement_concept_id" = "concept_id")) %>%
    filter(!is.na(value_as_number)) %>%
    filter(value_as_number >= 0 & value_as_number <= 50) %>% #should filter for unit too (mg/dl)?
    select(person_id, measurement_datetime, measurement_date, unit_source_value,
           value_as_number, site)
  
  
  # Compute max SCR per hour (0–144 hours after cohort start)
  max_scr_per_day <- cohort %>%
    select(person_id, participant_id, cohort_start_datetime, site) %>%
    left_join(creat_labs, by = c("person_id", "site")) %>%
    mutate(hours_from_csdt = date_diff("hour", cohort_start_datetime, measurement_datetime)) %>%
    filter(hours_from_csdt >= 0 & hours_from_csdt <= 144L) %>%
    mutate(days_from_csdt = sql("CAST(ROUND(hours_from_csdt / 24) AS INTEGER)")) %>%
    group_by(participant_id, days_from_csdt) %>%
    summarize(max_scr = max(value_as_number, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = days_from_csdt, values_from = max_scr, names_prefix = "day_")
    
  
  # Find lowest measured SCR in prior year
  min_scr_prior <- cohort %>%
    inner_join(creat_labs, by = c("person_id", "site")) %>%
    mutate(
      min_scr_window_start = date_add('hour', -24L, cohort_start_datetime),
      min_scr_window_end = date_add('hour', -8760L, cohort_start_datetime)
    ) %>%
    filter(measurement_datetime <= min_scr_window_start & measurement_datetime >= min_scr_window_end) %>%
    group_by(participant_id) %>%
    slice_min(order_by = value_as_number, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(min_scr_prior = value_as_number) %>%
    select(participant_id, min_scr_prior)
  
  # Hardcoded baseline SCR values
  baseline_scr <- cohort %>%
    select(participant_id, person_id, site, cohort_start_datetime) %>%
    left_join(demographics %>% select(participant_id, administrative_sex, scr_age_group, site),
              by = c("participant_id", "site")) %>%
    inner_join(baseline_scr_values, by = c("scr_age_group", "administrative_sex")) %>%
    select(participant_id, creatinine_median)
  
  # Combine baseline values
  baseline_combined <- full_join(min_scr_prior, baseline_scr, by = "participant_id") %>%
    mutate(scr_baseline = coalesce(min_scr_prior, creatinine_median))
  
  # Join baseline to SCR data by day
  scr_data <- max_scr_per_day %>%
    left_join(baseline_combined, by = "participant_id") %>%
    mutate(across(!participant_id, ~ as.numeric(.))) %>%  
    mutate(across(!participant_id, ~ round(.x, 2))) %>%   
    select(participant_id, min_scr_prior, creatinine_median, scr_baseline, order(colnames(.)))
  
  return(scr_data)
  
}





##########################

## Criteria 1
  
#' Stage 1 - increase in serum creatinine of 1.5-1.9x baseline serum creatinine 
#' Stage 2 - increase in serum creatinine of ≥2-2.9x (≥2x and <3x) baseline serum creatinine
#' Stage 3 - increase in serum creatinine of ≥3x baseline serum creatinine
  
# aki_stages_criteria1 <-  assign_aki_stages_criteria1(scr_aki_stages_by_day) %>%
#   output_tbl('aki_stages_criteria1')
#  
  
  # Assign KDIGO stages by day
assign_aki_stages_criteria1 <- function(df) {
    
  df <- df %>% 
    collect()
    
  for (d in 0:6) {
      scr_col <- paste0("day_", d)
      stage_col <- paste0("aki_criteria1_day_", d)
      df[[stage_col]] <- case_when(
        is.na(df[[scr_col]]) ~ 'No AKI',
        df[[scr_col]] >= 3 * df$scr_baseline ~ "Stage 3",
        df[[scr_col]] >= 2 * df$scr_baseline & df[[scr_col]] < 3 * df$scr_baseline~ "Stage 2",
        df[[scr_col]] >= 1.5 * df$scr_baseline & df[[scr_col]] < 2 * df$scr_baseline  ~ "Stage 1",
        TRUE ~ "No AKI"
    )
  }
  return(df)
}
  


  



##########################

# Criteria 2

#' Stage 1 - increase in serum creatinine by >= 0.3 mg/dL
#' Stage 3 - serum creatinine (SCr) > 4.0 mg/dL 

# aki_stages_criteria2 <-  assign_aki_stages_criteria2(scr_aki_stages_by_day) %>%
#   output_tbl('aki_stages_criteria2')
# 
# 


assign_aki_stages_criteria2 <- function(df) {
  
  df <- df %>% collect()
  
  for (d in 0:6) {
    scr_col <- paste0("day_", d)
    stage_col <- paste0("aki_criteria2_day_", d)
    
    df[[stage_col]] <- case_when(
      is.na(df[[scr_col]]) ~ "No AKI",
      df[[scr_col]] > 4.0 ~ "Stage 3",  # Absolute SCr > 4.0 mg/dL
      (df[[scr_col]] - df$scr_baseline) >= 0.3 ~ "Stage 1",  # Increase ≥ 0.3 mg/dL from baseline
      TRUE ~ "No AKI"
    )
  }
  return(df)
}
 
 
 
 
 
#################################################
 
 
# Criteria 3
 
#' Stage 3- decrease in eGFR to <35mL/min/1.73m2 (patients <18 yo, which should be all)

#' **Primary use function**
#' @param cohort cohort table with person_id, participant_id, cohort_start_date, cohort_start_datetime, site
#' @param meas_labs measurement_labs cdm tbl
#' @param serum_creatinine_codeset lab_serum_creatinine results table 
#' @param start_day 
#' @param end_day 
#' @return Maximum serum creatinine values for start_day to end_day
#' @colnames participant_id, day_0 ....day_6

#' #'Example usage: max_scr_7_days <-
#' get_max_scr_n_days(
#'   cohort = get_prompt_cohort(results_tbl('cohort', results_tag = F)),
#'   meas_labs = cdm_tbl("measurement_labs"),
#'   serum_creatinine_codeset = results_tbl("lab_serum_creatinine"),
#'   start_day = 0,
#'   end_day = 6) %>%
#'   output_tbl('max_scr_7_days')

 
get_max_scr_n_days <- function(cohort,
                               meas_labs = cdm_tbl("measurement_labs"),
                               serum_creatinine_codeset = results_tbl("lab_serum_creatinine"),
                               start_day = 0,
                               end_day = 6) {
  
  message("Getting maximum serum creatinine values from day {start_day} to day {end_day}...")
  
  start_hour <- start_day * 24
  end_hour <- end_day * 24
  
  creat_labs <- meas_labs %>%
    inner_join(serum_creatinine_codeset,
               by = c("measurement_concept_id" = "concept_id")) %>%
    filter(!is.na(value_as_number)) %>%
    filter(value_as_number >= 0 & value_as_number <= 50) %>% #should filter for unit too (mg/dl)?
    select(person_id, measurement_datetime, measurement_date, unit_source_value,
           value_as_number, site)
  
  max_scr_per_day <- cohort %>%
    select(person_id, participant_id, cohort_start_datetime, site) %>%
    left_join(creat_labs, by = c("person_id", "site")) %>%
    mutate(hours_from_csdt = date_diff("hour", cohort_start_datetime, measurement_datetime)) %>%
    filter(hours_from_csdt >= !!start_hour &
             hours_from_csdt <= !!end_hour) %>%
    mutate(days_from_csdt = sql("CAST(ROUND(hours_from_csdt / 24) AS INTEGER)")) %>%
    group_by(participant_id, days_from_csdt) %>%
    summarize(max_scr = max(value_as_number, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = days_from_csdt, values_from = max_scr, names_prefix = "day_") %>%
    mutate(across(matches("^day_"), ~ round(as.numeric(.), 2))) %>% 
    select(participant_id, order(colnames(.)))
  
  return(max_scr_per_day)
}



#' **Primary use function**
#' @param cohort cohort table with person_id, participant_id, cohort_start_date, cohort_start_datetime, site
#' @param meas_anthro measurement_anthro cdm tbl
#' @param height_codeset codeset for height
#' @param age_sex_bounds plausible height range based on age and sex
#' @param demographics 
#' @return Height in meters for each participant

# example usage: height_180_days <- get_height_180_days(cohort = get_prompt_cohort(results_tbl('cohort',results_tag = F)), 
#                                      meas_anthro = cdm_tbl("measurement_anthro"),
#                                      height_codeset = results_tbl('height'), 
#                                      age_sex_bounds = results_tbl('age_sex_bounds'),
#                                      demographics = results_tbl('cohort_demographics')) %>%
# output_tbl('height_180_days')



get_height_180_days <- function(cohort, 
                                meas_anthro = cdm_tbl("measurement_anthro"),
                                height_codeset = results_tbl('height'), 
                                age_sex_bounds = results_tbl('age_sex_bounds'),
                                demographics = results_tbl('cohort_demographics')) {
  
  message("Getting height within 180 days prior to cohort start...")
  
  heights_data <- meas_anthro %>%
    inner_join(height_codeset,
               by = c('measurement_concept_id' = 'concept_id')) %>%
    rename('ht' = 'value_as_number') %>%
    select(person_id, measurement_date, measurement_datetime, ht, site)
  
  
  height_in_window <- cohort %>%
    select(participant_id, person_id, cohort_start_date, cohort_start_datetime, site) %>%
    left_join(heights_data, by = c("person_id", "site")) %>%
    left_join(demographics %>% 
                select(participant_id, age_at_enrollment, administrative_sex) %>%
                mutate(age = as.numeric(round(age_at_enrollment))), by = "participant_id") %>%
    left_join(age_sex_bounds, by = c('administrative_sex' = "sex", "age")) %>%
    mutate(
      days_before = date_diff("day", measurement_date, cohort_start_date)
    ) %>%
    filter(days_before >= 0 & days_before <= 180) %>%
    mutate(ht_meters = round(as.numeric(ht) * 0.01, 2)) %>%
    filter(ht_meters >= height_meters_lower & ht_meters <= height_meters_upper) %>%
    group_by(participant_id) %>%
    slice_max(order_by = measurement_date, n = 1, with_ties = FALSE) %>%
    ungroup() 
  
  return(height_in_window)
}


#' **Primary use function**
#' @param demographics 
#' @return Calculated K coeff for each participant based on age and sex

# Example usage:  k_coeff <- calculate_k_coeff(demographics = results_tbl('cohort_demographics')) %>%
#   output_tbl('k_coeff')

calculate_k_coeff <- function(demographics = results_tbl('cohort_demographics')) {
  
  message("Calculating age and k coefficient...")
  
  demo_k <- demographics %>%
    mutate(age = as.numeric(round(age_at_enrollment))) %>%
    mutate(k_coeff = case_when(
        administrative_sex == "Male" & age < 12 ~ 39.0 * (1.008 ^ (age - 12)),
        administrative_sex == "Male" & age < 18 ~ 39.0 * (1.045 ^ (age - 12)),
        administrative_sex == "Male" ~ 50.8,
        administrative_sex == "Female" & age < 12 ~ 36.1 * (1.008 ^ (age - 12)),
        administrative_sex == "Female" & age < 18 ~ 36.1 * (1.023 ^ (age - 12)),
        administrative_sex == "Female" ~ 41.4),
      k_coeff = round(as.numeric(k_coeff), 1)) %>%
    select(participant_id, age, k_coeff, administrative_sex)
  
  return(demo_k)
}


#' **Primary use function**
#' @param cohort cohort table with person_id, participant_id, cohort_start_date, cohort_start_datetime, site
#' @param max_scr_per_day MAX scr from day 0 to day 6
#' @param demo_k k coeff
#' @param height_in_window plausible height range based on age and sex
#' @param demographics 
#' @return Calculated egfr for day 0 to day 6

# Example usage: egfr_in_7_days <- calculate_egfr_7day_only(cohort = get_prompt_cohort(results_tbl('cohort',results_tag = F)), 
#                                                           max_scr_per_day = results_tbl('max_scr_7_days'),
#                                                           demographics = results_tbl('cohort_demographics'),
#                                                           height_in_window = results_tbl('height_180_days'), 
#                                                           demo_k = results_tbl('k_coeff')) %>%
#   output_tbl('egfr_in_7_days')


calculate_egfr_7day_only <- function(cohort, 
                                     max_scr_per_day,
                                     demographics,
                                     height_in_window, 
                                     demo_k) {
  
  message("Calculating eGFR for 7-day values without baseline...")
  
  combined <- cohort %>%
    left_join(max_scr_per_day, by = "participant_id") %>%
    left_join(demographics %>% 
                select(participant_id, age_at_enrollment, administrative_sex),
              by = "participant_id") %>%
    left_join(height_in_window %>% 
                select(participant_id, ht_meters),
              by = "participant_id") %>%
    left_join(demo_k %>% select(participant_id, k_coeff), by = "participant_id") %>%
    select(participant_id, 
           person_id, 
           cohort_start_date, 
           cohort_start_datetime,
           age_at_enrollment,
           administrative_sex,
           ht_meters,
           k_coeff, 
           starts_with('day_'))
  
  
  egfr_data <- combined %>%
    mutate(
      egfr_day_0 = ifelse(is.na(day_0) | is.na(k_coeff) | is.na(ht_meters), NA_real_, round(k_coeff * (ht_meters / day_0), 1)),
      egfr_day_1 = ifelse(is.na(day_1) | is.na(k_coeff) | is.na(ht_meters), NA_real_, round(k_coeff * (ht_meters / day_1), 1)),
      egfr_day_2 = ifelse(is.na(day_2) | is.na(k_coeff) | is.na(ht_meters), NA_real_, round(k_coeff * (ht_meters / day_2), 1)),
      egfr_day_3 = ifelse(is.na(day_3) | is.na(k_coeff) | is.na(ht_meters), NA_real_, round(k_coeff * (ht_meters / day_3), 1)),
      egfr_day_4 = ifelse(is.na(day_4) | is.na(k_coeff) | is.na(ht_meters), NA_real_, round(k_coeff * (ht_meters / day_4), 1)),
      egfr_day_5 = ifelse(is.na(day_5) | is.na(k_coeff) | is.na(ht_meters), NA_real_, round(k_coeff * (ht_meters / day_5), 1)),
      egfr_day_6 = ifelse(is.na(day_6) | is.na(k_coeff) | is.na(ht_meters), NA_real_, round(k_coeff * (ht_meters / day_6), 1))
      #egfr_day_7 = ifelse(is.na(day_7) | is.na(k_coeff) | is.na(ht_meters), NA_real_, round(k_coeff * (ht_meters / day_7), 1))
    )
  
  
  
  return(egfr_data)

} 



# aki_stages_criteria3 <-  assign_aki_stages_criteria3(egfr_data) %>%
#   output_tbl('aki_stages_criteria3')
# 


assign_aki_stages_criteria3 <- function(egfr_data) {
  
  
  # Flag AKI stage 3: eGFR < 35
  egfr_stage <- egfr_data %>%
    mutate(across(starts_with("egfr_day_"),
                  ~ ifelse(. < 35, "Stage 3", "No AKI"),
                  .names = "aki_criteria3_{sub('egfr_', '', .col)}")) %>%
    select(-person_id, -cohort_start_date, -cohort_start_datetime)
  
  return(egfr_stage)
}


#######################################


# Criteria 4 

# Stage 3 - use of new renal replacement therapy.

#' **Primary use function**
#' @param cohort cohort table with person_id, participant_id, cohort_start_date, cohort_start_datetime, site
#' @param procedures CDM tbl procedure_occurrence
#' @param rrt_codeset proc_rrt_dialysis
#' @param satrt_day 
#' @param end_day 
#' @return Flag if a patient has rrt from start day to end day



get_rrt <- function(cohort,
                    procedures = cdm_tbl("procedure_occurrence"),
                    rrt_codeset = results_tbl("proc_rrt_dialysis"),
                    start_day = 0,
                    end_day = 7) {
  
  message("Getting patients with renal replacement therapy....")
  
  start_hour <- start_day * 24
  end_hour <- end_day * 24
  
  # Get RRT procedures
  rrt_procedures <- procedures %>%
    inner_join(rrt_codeset,
               by = c("procedure_concept_id" = "concept_id")) %>%
    select(person_id, procedure_datetime, procedure_date, procedure_concept_name,
           procedure_concept_id, concept_code, vocabulary_id, site)
  
  # Filter out patients with RRT 60 days (1440 hours) before cohort entry
  pre_rrt <- cohort %>%
    select(person_id, participant_id, cohort_start_datetime, site) %>%
    left_join(rrt_procedures, by = c("person_id", "site")) %>%
    mutate(hours_before_csdt = date_diff("hour", procedure_datetime, cohort_start_datetime)) %>%
    filter(hours_before_csdt > 0 & hours_before_csdt <= 1440) %>%
    mutate(rrt_prior_flag = 1L) %>%
    distinct(participant_id, rrt_prior_flag) 
  
  # cohort_filtered <- cohort %>%
  #   left_join(pre_rrt, by = "participant_id")
  
  # RRT during specified window
  rrt_procedures_in_window <- cohort %>%
    select(person_id, participant_id, cohort_start_datetime, site) %>%
    left_join(rrt_procedures, by = c("person_id", "site")) %>%
    mutate(hours_from_csdt = date_diff("hour", cohort_start_datetime, procedure_datetime)) %>%
    filter(hours_from_csdt >= !!start_hour &
             hours_from_csdt <= !!end_hour) %>%
    mutate(days_from_csdt = sql("CAST(ROUND(hours_from_csdt / 24) AS INTEGER)"))
  
  rrt_procedures_by_day <- rrt_procedures_in_window %>%
    mutate(rrt_flag = 1L) %>%
    group_by(participant_id, days_from_csdt) %>%
    summarize(rrt = max(rrt_flag, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = days_from_csdt, values_from = rrt,
                names_prefix = "day_", values_fill = 0) %>%
    ungroup() %>%
    mutate(across(matches("^day_"), ~ round(as.numeric(.), 2))) %>%
    select(participant_id, order(colnames(.)))
  
  final_tbl <- cohort %>%
    select(person_id, participant_id, cohort_start_datetime, site) %>%
    left_join(rrt_procedures_by_day, by = 'participant_id') %>%
    left_join(pre_rrt, by = "participant_id") %>%
    mutate(across(matches("^day_"), ~ ifelse(is.na(.), 0, .)),
           rrt_prior_flag = ifelse(is.na(rrt_prior_flag), 0L, rrt_prior_flag))
  
  
  return(final_tbl)
}



assign_aki_stages_criteria4 <- function(df) {
  
  df <- df %>% collect()
  
  for (d in 0:6) {
    rrt_col <- paste0("day_", d)
    stage_col <- paste0("aki_criteria4_day_", d)
    df[[stage_col]] <- case_when(
      is.na(df[[rrt_col]]) ~ NA_character_,
      df[[rrt_col]] == 1L  ~ "Stage 3",
      TRUE ~ "No AKI"
    )
  }
  
  return(df)
}





###########################################


# Assign final AKI stages now for cohort based on all 4 criteria over 7 days
#' **Primary use function**
# Condition 1 - The criterion must be present for at least 2 calendar days OR
# Condition 2 - present on the day of death if the patient dies prior to 2 calendar days.

assign_final_aki_stages <- function(cohort = get_prompt_cohort(results_tbl('cohort', results_tag = F)),
                                    aki_stages_criteria1 = results_tbl('aki_stages_criteria1'),
                                    aki_stages_criteria2 = results_tbl('aki_stages_criteria2'),
                                    aki_stages_criteria3 = results_tbl('aki_stages_criteria3'),
                                    aki_stages_criteria4 = results_tbl('aki_stages_criteria4'),
                                    death_table = results_tbl('cohort_death')) {
  
  # Helper to reshape criteria tables
  reshape_aki <- function(df, prefix) {
    df %>%
      pivot_longer(cols = starts_with(prefix),
                   names_to = "day",
                   names_prefix = prefix,
                   values_to = "stage") %>%
      select(participant_id, day, stage)
  }
  
  # Reshape all criteria tables
  c1_long <- reshape_aki(aki_stages_criteria1, "aki_criteria1_day_")
  c2_long <- reshape_aki(aki_stages_criteria2, "aki_criteria2_day_")
  c3_long <- reshape_aki(aki_stages_criteria3, "aki_criteria3_day_")
  c4_long <- reshape_aki(aki_stages_criteria4, "aki_criteria4_day_")
  
  # Combine all criteria by participant and day
  all_criteria <- full_join(c1_long, c2_long, by = c("participant_id", "day"), suffix = c("_c1", "_c2")) %>%
    full_join(c3_long, by = c("participant_id", "day")) %>%
    rename(stage_c3 = stage) %>%
    full_join(c4_long, by = c("participant_id", "day")) %>%
    rename(stage_c4 = stage) %>%
    mutate(across(starts_with("stage"), ~ coalesce(., "No AKI")))
  
  # Stage priority function
  resolve_stage <- function(stages) {
    stages <- stages[!is.na(stages)]  
    priority <- c("Stage 3", "Stage 2", "Stage 1")  # Priority order
    for (p in priority) {
      if (p %in% stages) return(p)
    }
    return("No AKI")  
  }
  
  # Apply the priority logic 
  all_final_stages <- all_criteria %>% collect() %>%
    mutate(final_stage = apply(select(., starts_with("stage_c")), 1, resolve_stage))
  
  
  
  final_stage_cond1 <- all_final_stages %>%
    group_by(participant_id) %>%
    arrange(day) %>%
    mutate(
      previous_stage = lag(final_stage),
      previous_stage = ifelse(is.na(previous_stage), "No AKI", previous_stage),
      is_consecutive_aki = (previous_stage %in% c("Stage 1", "Stage 2", "Stage 3")) & (final_stage %in% c("Stage 1", "Stage 2", "Stage 3")), #logic to check consecutive AKI stages
      # If previous stage is "No AKI" and final stage is one of the AKI stages, or vice versa, it is not consecutive
      final_stage_cond1 = ifelse(is_consecutive_aki, resolve_stage(final_stage), "No AKI") # Resolve stage only if consecutive AKI
    ) %>%
    ungroup()
  
  # Incorporate death information
  death_info <- death_table %>% 
    mutate(days_from_csdt = date_diff("day", cohort_start_date, death_date)) %>%
    filter(days_from_csdt <= 6) %>%
    mutate(days_from_csdt = as.character(days_from_csdt)) %>%
    select(participant_id,days_from_csdt) %>% collect()
  
  
  # Apply death condition logic 
  final_stage_cond2 <- all_final_stages %>%
    group_by(participant_id) %>%
    arrange(day) %>%
    ungroup() %>%
    left_join(death_info, by = c('participant_id')) %>%
    mutate(final_stage_cond2 = ifelse(day == days_from_csdt, final_stage, NA))
  
  
  # Join both the condition tables and assign the final stage 
  final_aki_stages_by_day <- final_stage_cond1 %>%
    left_join(final_stage_cond2 %>% select(participant_id, final_stage_cond2, days_from_csdt), by= c('participant_id', 'day' = 'days_from_csdt')) %>%
    mutate(final_aki_stage = ifelse(!is.na(final_stage_cond2), final_stage_cond2, final_stage_cond1))
  
  
  
  # Return in wide format
  final_stages_wide <- final_aki_stages_by_day %>%
    select(participant_id, day, final_aki_stage) %>%
    pivot_wider(names_from = day, values_from = final_aki_stage, names_prefix = "aki_stage_day_", values_fn = list) %>%
    select(participant_id, order(colnames(.))) 
  
  
  resolve_highest_stage <- function(stages) {
    priority <- c("Stage 3", "Stage 2", "Stage 1", "No AKI")  # Priority order
    for (p in priority) {
      if (p %in% stages) return(p)
    }
    return(NA_character_)
  }
  
  final_stages_wide <- final_stages_wide %>%
    rowwise() %>%
    mutate(aki_stage = resolve_highest_stage(c_across(starts_with("aki_stage_day_")))) %>%
    ungroup() %>%
    select(participant_id, aki_stage)
  
  
  return(final_stages_wide)
}
