#' Function to get in-person visits from date_min to date_max defined in the global driver control parameters
#'
#' @param visit_tbl CDM visit table
#' @param start_date defaulted to 2009-01-01
#' @param end_date defaulted to YTD
#' @param in_person_vector requires a vector of inperson concept ids
#'
#' @return a distinct tibble of person_id and visit_occurrence_id for the criteria
#' @md
#'
get_inperson_visit <-
  function(visit_tbl = cdm_tbl('visit_occurrence'),
           date_min,
           date_max,
           in_person_visit) {
    visit_between_2019_ytd <- visit_tbl %>%
      dplyr::filter(visit_start_date >= date_min &
                      visit_start_date <= date_max) %>%
      inner_join(in_person_visit, by = c('visit_concept_id' = "concept_id")) %>%
      ungroup() %>%
      distinct(person_id, site)
    
    if (requireNamespace('beepr',quietly = T) & exists('notify') & notify == T) (beepr::beep())
    
    
    return(visit_between_2019_ytd)
    
  }


#' Exclude patients with no in-person visits with a nephrology provider or facility at any time date_min to date_max defined in the global driver control parameters
#'
#' @param cohort Cohort of interest
#' @param visit_tbl CDM visit table
#' @param care_site CDM care site table
#' @param provider CDM provider table
#' @param in_person_codeset inperson codeset
#' @param neph_spec_codeset nephrology specialty codeset
#'
#' @return
#' @export
#'
#' @examples
get_cohort_with_neph_inperson_spec <-
  function(visit_tbl = cdm_tbl('visit_occurrence'),
           care_site = cdm_tbl('care_site'),
           provider =  cdm_tbl('provider'),
           in_person_codeset,
           neph_spec_codeset,
           visit_start,
           visit_end,
           cohort) {
    visits <- cdm_tbl('visit_occurrence') %>%
      dplyr::filter(visit_start_date >= visit_start &
               visit_end_date <= visit_end)  %>%
      inner_join(in_person_codeset, by = c("visit_concept_id" = "concept_id")) %>%
      inner_join(distinct(cohort, person_id), by = "person_id") 
    
    neph_specialty <- visits %>%
      inner_join(
        select(
          care_site,
          care_site_id,
          specialty_concept_id,
          specialty_concept_name
        ),
        by = 'care_site_id'
      ) %>%
      inner_join(neph_spec_codeset,
                 by = c('specialty_concept_id' = 'concept_id')) %>%
      distinct(person_id,
               site,
               visit_occurrence_id,
               visit_concept_name,
               specialty_concept_name)
    
    neph_provider <- visits %>%
      inner_join(select(
        provider,
        provider_id,
        specialty_concept_id,
        specialty_concept_name
      ),
      by = 'provider_id')  %>%
      inner_join(neph_spec_codeset,
                 by = c('specialty_concept_id' = 'concept_id')) %>%
      distinct(person_id,
               site,
               visit_occurrence_id,
               visit_concept_name,
               specialty_concept_name)
    
    cohort_with_neph_spec <-
      union(neph_specialty, neph_provider) 
    
    
    if (requireNamespace('beepr', quietly = T) &
        exists('notify') & notify == T)
      (beepr::beep())
    
    
    return(cohort_with_neph_spec)
    
  }


#' [Replacing the legacy function with new function translated from PRESERVE SAS/SQL code. The old phenotype lies in cohort_old.R]

#' Function to get serum creatinine values for a given period for a given cohort
#'
#' @param meas_labs_tbl CDM or mini-CDM measurement table
#' @param start_date start date for serum creatinine value to be included
#' @param end_date end date for serum creatinine value to be included
#' @param serum_creatinine_codeset serum creatinine codeset
#' @param cohort cohort of interest
#' @param age_max upper bound for age to be inclded in the cohort
#' @param person_tbl CDM or mini-CDM person_tbl
#'
#' @return returns a temp table with serum creatinine labs with age,sex and birth_date
#'get_serum_creatinine <-
#'  function(meas_labs_tbl = cdm_tbl('measurement_labs'),
#'           start_date = "2009-01-01",
#'           end_date = lubridate::today(),
#'           serum_creatinine_codeset,
#'           cohort,
#'           age_max,
#'           person_tbl = cdm_tbl('person')) {
#'    cohort <-
#'      cohort %>% distinct(person_id, site) %>%
#'      compute_new(indexes = list('person_id'))
#'    
#'    meas_labs <- meas_labs_tbl %>%
#'      select(
#'        person_id,
#'        measurement_id,
#'        visit_occurrence_id,
#'        serum_creat_meas_date = measurement_date,
#'        serum_creat_meas_datetime = measurement_datetime,
#'        measurement_concept_id,
#'        measurement_concept_name,
#'        serum_creat_num = value_as_number
#'      )  %>%
#'      filter(serum_creat_meas_date >= start_date &
#'               serum_creat_meas_date <= end_date) %>%
#'      inner_join(cohort, by = 'person_id') # more earlier for efficiencies when cohort small
#'    
#'    serum_creatinine_raw <- meas_labs %>%
#'      inner_join(serum_creatinine_codeset,
#'                 by = c("measurement_concept_id" = "concept_id")) %>%
#'      group_by(person_id) %>%
#'      mutate(visit_serum_creat_rank = dense_rank(serum_creat_meas_datetime)) %>%
#'      mutate(total_visit_serum_meas = max(visit_serum_creat_rank)) %>%
#'      ungroup() %>%
#'      compute_new(indexes = list('person_id', 'measurement_id'))
#'    
#'    serum_creatinine <- person_tbl %>%
#'      select(person_id, birth_date, gender_concept_name) %>%
#'      mutate(sex = substr(gender_concept_name, 1, 1)) %>%
#'      inner_join(serum_creatinine_raw,
#'                 by = "person_id") %>%
#'      mutate(age_at_measurement = round((serum_creat_meas_date - birth_date) / 365.25)) %>%
#'      filter(between(serum_creat_num, 0, 50)) %>%
#'      filter(between(age_at_measurement, 1, age_max)) %>%
#'      compute_new(indexes = list('person_id', 'measurement_id'))
#'    
#'    return(serum_creatinine)
#'    
#'  }
#'


#' #' Function to dplyr::filter patients with age 0 to 25 at the time of serum measurement
#' #'
#' #' @param person_tbl CDM person table
#' #' @param serum_creatinine_raw_tbl raw table from `get_serum_creatinine_for_visit_2009_ytd()`
#' #'
#' get_serum_creatinine_for_visit_2009_ytd_age_0_25 <-
#'   function(person_tbl = cdm_tbl('person'),
#'            serum_creatinine_raw_tbl,
#'            age_max = 25L) {
#'     serum_creatinine <- cdm_tbl("person") %>%
#'       select(person_id, birth_date, gender_concept_name) %>%
#'       dplyr::mutate(sex = substr(gender_concept_name, 1, 1)) %>%
#'       inner_join(serum_creatinine_raw_tbl,
#'                  by = "person_id", copy = T) %>%
#'       dplyr::mutate(age_at_measurement = round((serum_creat_meas_date - birth_date) / 365.25)) %>%
#'       dplyr::filter(between(age_at_measurement, 0, age_max)) %>%
#'       compute_new(indexes = list('person_id', 'measurement_id'))
#'
#'     return(serum_creatinine)
#'
#'   }

#' Create temporary lab_result_cm table, which can be merged with the serum_creatinine codeset
#' to make our output serum creatinine table, queried for potentially unmapped serum creatinine labs
#' and used to generate distributions of key variables
#'
#' @param lab_tbl PCORnet CDM lab_result_cm
#' @param demographic_tbl PCORnet CDM demographic table
#' @param cohort Cohort of interest
#' @param age_min minimum age at measure
#' @param age_max maximum age at measure
#' @param ser_creat_codeset serum creatinine codeset
#' @param date_min minimum date at measure
#' @param date_max maximum date at measure
#'
#' @return
#' @export
#'
#' @examples
obtain_serum_creatinine <-
  function(lab_tbl = cdm_tbl('measurement_labs'),
           demographic_tbl = cdm_tbl('person'),
           cohort,
           age_min,
           age_max,
           ser_creat_codeset,
           date_min,
           date_max) {
    
    demographic_tbl <-
      distinct(cohort, person_id) %>%
      inner_join(select(demographic_tbl, person_id, gender_concept_name, birth_date))
    
    labs <- lab_tbl %>%
      dplyr::filter(between(measurement_date,
                            date_min,
                            date_max)) %>%
      inner_join(demographic_tbl) 
    
    fil_labs <- labs %>%
      mutate(age_at_measurement = round(measurement_age_in_months / 12)) %>%
      dplyr::filter(value_as_number > 0 & value_as_number <= 50) %>%
      dplyr::filter(between(date_diff('days',birth_date, measurement_date), 365 * age_min, 365.25 * age_max)) %>%
      inner_join(load_codeset('serum_creatinine'),
                 by = c('measurement_concept_id' = 'concept_id')) %>%
      select(
        person_id,
        gender_concept_name,
        serum_creat_lab_date = measurement_date,
        serum_creat_meas_value = value_as_number,
        unit_concept_name,
        range_high,
        range_low,
        measurement_id,
        age_at_measurement,
        birth_date,
        site
      ) 
    
    if (requireNamespace('beepr', quietly = T) &
        exists('notify') & notify == T)
      (beepr::beep())
    
    return(fil_labs)
}

# [Replacing the legacy function with new function translated from PRESERVE SAS/SQL code. The old phenotype lies in cohort_old.R]
#'#' Get Heights requirement for CKD cohort
#
# @param meas_anthro_tbl CDM or mini-CDM measurement table
# @param height_codeset codeset for height concept ids
# @param cohort cohort of interest
# @param bounds_tbl bounds table for heights and serum creatinine value
#
# @return
#get_heights_requirement <-
#  function(meas_anthro_tbl = cdm_tbl('measurement_anthro'),
#           height_codeset,
#           cohort,
#           bounds_tbl =  load_codeset("age_sex_bounds0_25",
#                                      indexes = list("age"),
#                                      col_types = "ciddddd")) {
#    # # this is another way of applying the bounds and relies on measurement_id being the source
#    # # value for the computed z-scores. For ease of interpretation, we may want to use the bounds.
#    # plausible_heights <- meas_anthro_tbl %>%
#    #   filter(measurement_concept_id %in% height_codeset_vctr)  %>%
#    #   filter(between(value_as_number, -5, 4)) %>%
#    #   mutate(measurement_id = as.numeric(substr(value_source_value, 14, 1000))) %>%
#    #   dplyr::select(height_z = value_as_number, measurement_id) %>% compute_new()
#    #
#    
#    height_codeset_list <-
#      height_codeset %>%
#      distinct(concept_id) %>%
#      pull()
#    
#    heights_unchecked <- meas_anthro_tbl %>%
#      dplyr::select(
#        person_id,
#        value_as_number,
#        measurement_date,
#        measurement_datetime,
#        measurement_id,
#        measurement_concept_id
#      ) %>%
#      inner_join(distinct(cohort, person_id), by = "person_id") %>% # move earlier for efficiencies when #'cohort is small
#      filter(measurement_concept_id %in% height_codeset_list)  %>%
#      dplyr::select(
#        person_id,
#        height = value_as_number,
#        height_date = measurement_date,
#        height_datetime = measurement_datetime,
#        height_measurement_id = measurement_id
#      ) %>%
#      compute_new(indexes = list('person_id', 'height_measurement_id'))
#    
#    # heights_checked <-
#    #   heights_unchecked %>%
#    #   inner_join(plausible_heights) %>%
#    #   compute_new(indexes = list('person_id', 'measurement_id'))
#    
#    heights_raw <-
#      heights_unchecked %>%
#      inner_join(cohort, by = "person_id") %>%
#      filter(abs(height_date - serum_creat_meas_date) <= 180) %>%
#      compute_new(indexes = list('person_id', 'measurement_id'))
#    
#    heights <- heights_raw %>%
#      mutate(days_sep = abs(height_date - serum_creat_meas_date)) %>%
#      select(
#        height,
#        serum_creat_num,
#        age_at_measurement,
#        sex,
#        person_id,
#        measurement_id,
#        serum_creat_meas_date,
#        serum_creat_meas_datetime,
#        height_date,
#        height_datetime,
#        days_sep
#      ) %>%
#      group_by(person_id, measurement_id) %>%
#      window_order(days_sep, height_datetime) %>%
#      filter(row_number() == 1) %>% # AGD: Check
#      ungroup() %>%
#      inner_join(cdm_tbl('person') %>% select(person_id, site)) %>%
#      compute_new(indexes = list('person_id', 'measurement_id'))
#    
#    
#    heights <- heights %>%
#      inner_join(bounds_tbl,
#                 by = c("sex" = "sex", "age_at_measurement" = "age")) %>%
#      filter(
#        between(height * 0.01, height_meters_lower, height_meters_upper),
#        # AGD check this
#        serum_creat_num <= serum_creatinine_upper
#      ) %>%
#      select(
#        -c(
#          "k_coeff",
#          "serum_creatinine_upper",
#          "serum_creatinine_lower",
#          "height_meters_upper",
#          "height_meters_lower"
#        )
#      ) %>%
#      compute_new(indexes = list('person_id', 'measurement_id'))
#    
#    return(heights)
#    
#  }

#' Create local table of heights for cohort of patients with >= 1 value in serum creatinine table
#'
#' @param serum_creatinine_tbl output generated from `get_serum_creatinine()`
#' @param measurement_anthro CDM measurement anthro table
#'
#' @return
#' @export
#'
#' @examples
get_heights <- function(serum_creatinine_tbl,
                        measurement_anthro = cdm_tbl('measurement_anthro')) {
  heights_data <- measurement_anthro %>%
    inner_join(load_codeset('height'),
               by = c('measurement_concept_id' = 'concept_id')) %>%
    rename('ht' = 'value_as_number')
  
  
  heights <- serum_creatinine_tbl %>%
    select(person_id, serum_creat_lab_date) %>%
    inner_join(
      heights_data %>%
        dplyr::filter(!is.na(ht)) %>%
        select(person_id, site, height_date = measurement_date, ht),
      by = "person_id"
    ) %>%
    group_by(person_id, site, serum_creat_lab_date) %>%
    summarise(days = abs(date_diff('days', height_date, serum_creat_lab_date))) %>% 
    dplyr::filter(days <= 180) %>%
    inner_join(
      heights_data %>%
        dplyr::filter(!is.na(ht)) %>%
        select(person_id, height_date = measurement_date, ht),
      by = 'person_id'
    ) %>%
    dplyr::filter(abs(date_diff('days', height_date, serum_creat_lab_date)) == days) %>%
    select(person_id, site, serum_creat_lab_date, height_date, ht) %>%
    distinct() %>%
    ungroup() 
  
  if (requireNamespace('beepr', quietly = T) &
      exists('notify') & notify == T)
    (beepr::beep())
  
  return(heights)
  
}


#' Replacing the legacy function with new function translated from PRESERVE SAS/SQL code. The old phenotype lies in cohort_old.R
#' #' Derive egfr
#' #'
#' #' @param full_tbl
#' #' @param bounds_tbl
#' #' @param join_keys
#' #' @param height_col
#' #' @param ser_creat_col
#' #' @param height_conversion_factor
#' 
#' derive_egfr <- function(full_tbl,
#'                         bounds_tbl,
#'                         join_keys = c('sex' = 'sex', 'age_at_measurement' =
#'                                         'age'),
#'                         height_col = 'ht',
#'                         ser_creat_col = 'serum_creat_num',
#'                         height_conversion_factor = 1) {
#'   full_tbl %>%
#'     left_join(bounds_tbl, by = join_keys, copy = T) %>%
#'     dplyr::filter(!!sym(height_col) > 0 &
#'                     !!sym(ser_creat_col) > 0) %>%
#'     dplyr::filter(between(
#'       !!sym(height_col),
#'       height_meters_lower,
#'       height_meters_upper
#'     )) %>%
#'     dplyr::filter(between(
#'       !!sym(height_col),
#'       height_meters_lower,
#'       height_meters_upper
#'     )) %>%
#'     dplyr::mutate(between(
#'       !!sym(ser_creat_col),
#'       serum_creatinine_lower,
#'       serum_creatinine_upper
#'     )) %>%
#'     dplyr::mutate(
#'       height_meters = !!sym(height_col) * height_conversion_factor,
#'       egfr = k_coeff * (height_meters / !!sym(ser_creat_col)),
#'     ) %>%
#'     compute_new()
#' }


#' Create temporary table of heights and ser_creat to merge with egfr_lookup
#'
#' @param cohort_heights Cohort Heights table output from get_cohort_heights()
#' @param conversion_factor Conversion factor to obtain heights in inches.
#' @param cohort_ser_creat Serum creatinine table from get_serum_creatinine()
#' @param age_sex_bounds age sex bound table codeset
#'
#' @return
#' @export
#'
#' @examples
compute_egfr_data <- function(cohort_heights,
                              cohort_ser_creat,
                              conversion_factor,
                              age_sex_bounds) {
  egfr <-
    cohort_heights %>%
    mutate(ht_meters = ht * conversion_factor) %>%
    select(person_id, height_date, ht_meters, serum_creat_lab_date) %>%
    inner_join(
      cohort_ser_creat %>%  select(
        person_id,
        serum_creat_lab_date,
        serum_creat_meas_value,
        sex = gender_concept_name,
        age_at_measurement,
        site
      ),
      by = c('person_id', 'serum_creat_lab_date')
    ) %>%
    mutate(sex = case_when(sex == "MALE" ~ "M",
                           sex == "FEMALE" ~ "F",
                           T ~ NA)) %>%
    inner_join(age_sex_bounds, by = c('sex', 'age_at_measurement' = 'age')) %>%
    dplyr::filter(between(ht_meters, height_meters_lower, height_meters_upper)) %>%
    mutate(egfr = k_coeff * ht_meters / serum_creat_meas_value) 
  
  if (requireNamespace('beepr', quietly = T) &
      exists('notify') & notify == T)
    (beepr::beep())
  
  return(egfr)
  
}

#' Get patients with >=1 eGFR < 90mL/min/1.73m2
#'
#' @param egfr_cohort Cohort with egfr values - output generated from compute_egfr_data()
#' @param egfr_min minimum egfr value for the range
#' @param egfr_max maximum egfr value for the range
#'
#' @return
#' @export
#'
#' @examples
  get_egfr_visit_within_range <- function(egfr_cohort,egfr_min,egfr_max) {
  rslt_tbl <- egfr_cohort %>%
    group_by(person_id) %>%
    dplyr::mutate(egfr_le_90_bin = ifelse(egfr >= egfr_min &
                                            egfr < egfr_max , 1, 0)) %>%
    dplyr::mutate(total_egfr_le_90 = sum(egfr_le_90_bin)) %>%
    dplyr::filter(total_egfr_le_90 >= 1) %>%
    ungroup() 
  
  return(rslt_tbl)
  
}


#' Get patients with >=2 eGFRs >=30 and <90mL/min/1.73m2 which are >=90 days apart (aged >=1 and < 18)
#'
#' @param egfr_1_or_more_less_90 output generated from get_egfr_visit_within_range()
#' @param egfr_min minimum egfr value for the range
#' @param egfr_max maximum egfr value for the range
#'
#' @return
#' @export
#'
#' @examples
get_egfr_pair_sep_90_days <-
  function(egfr_1_or_more_less_90,egfr_min,egfr_max) {
    # is this requiring that consecutive eGFRs be at least 90 days apart? The requirement is any two
    # rslt_tbl <- egfr_1_or_more_less_90 %>%
    #   dplyr::filter(egfr_le_90_bin == 1)  %>%
    #   group_by(person_id) %>%
    #   dplyr::mutate(measurement_rank = dense_rank(serum_creat_meas_date)) %>%
    #   window_order(measurement_rank) %>%
    #   dplyr::mutate(days_between_egfr_le_90 = ifelse(
    #     is.na(serum_creat_meas_date - lag(serum_creat_meas_date)),
    #     0,
    #     serum_creat_meas_date - lag(serum_creat_meas_date)
    #   )) %>%
    #   dplyr::mutate(total_egfr_le_90_days_apart_90 = sum(ifelse(days_between_egfr_le_90 > 90, 1 , 0))) %>%
    #   ungroup() %>%
    #   dplyr::filter(total_egfr_le_90_days_apart_90 >= 2) %>%
    #   compute_new()
    
    rslt_tbl <- egfr_1_or_more_less_90 %>%
      dplyr::filter(egfr >= egfr_min & egfr < egfr_max) %>%
      group_by(person_id, site) %>%
      mutate(max_serum_date = max(serum_creat_lab_date)) %>% 
      mutate(min_serum_date = min(serum_creat_lab_date)) %>% 
      summarise(days_sep = date_diff('days', min_serum_date, max_serum_date)) %>% 
      ungroup() %>%
      dplyr::filter(days_sep >= 90) %>%
      distinct(person_id, site)
    
    if (requireNamespace('beepr', quietly = T) &
        exists('notify') & notify == T)
      (beepr::beep())
    return(rslt_tbl)
    
  }


#' Create vector for patients with >=2 eGFRs >=30 and <90mL/min/1.73m2 which are >=90 days apart, without an intervening eGFR value >= 90 mL/min/1.73m2 (aged >=1 and < 18)
#'
#' @param egfr_2_or_more_less_90_sep_days_90 Output generated from get_egfr_visit_within_range()
#' @param egfr_1_or_more_less_90 Output generated from get_egfr_pair_sep_90_days()
#' @param egfr_min minimum egfr value for the range
#' @param egfr_max maximum egfr value for the range
#'
#' @return
#' @export
#'
#' @examples
  get_qual_egfr_no_int <-
  function(egfr_2_or_more_less_90_sep_days_90,
           egfr_1_or_more_less_90,egfr_min,egfr_max) {
    # rslt_tbl <- egfr_2_or_more_less_90_sep_days_90 %>%
    #   distinct(person_id) %>%
    #   inner_join(egfr_1_or_more_less_90) %>%
    #   group_by(person_id) %>%
    #   dplyr::mutate(measurement_rank = dense_rank(serum_creat_meas_date)) %>%
    #   window_order(measurement_rank) %>%
    #   dplyr::mutate(days_between_egfr_meas = ifelse(
    #     is.na(serum_creat_meas_date - lag(serum_creat_meas_date)),
    #     0,
    #     serum_creat_meas_date - lag(serum_creat_meas_date)
    #   )) %>%
    #   dplyr::mutate(days_between_egfr_meas_bin = ifelse(days_between_egfr_meas > 90, 1 , 0)) %>%
    #   dplyr::mutate(egfr_long_vector = ifelse(egfr_le_90_bin == 1 &
    #                                      days_between_egfr_meas_bin == 1, 1, 0)) %>%
    #   ungroup() %>%
    #   compute_new()
    
    rslt_tbl <- egfr_2_or_more_less_90_sep_days_90 %>%
      distinct(person_id) %>%
      inner_join(egfr_1_or_more_less_90, by = "person_id") %>%
      dplyr::mutate(above_thrs = if_else(egfr >= egfr_max |
                                           egfr <= egfr_min, 1L, 0L)) %>%
      group_by(person_id, site) %>%
      dplyr::mutate(measurement_rank = dense_rank(serum_creat_lab_date)) %>%
      window_order(measurement_rank) %>%
      dplyr::mutate(int_chunk = cumsum(above_thrs)) %>%
      ungroup() %>%
      select(person_id,
             site,
             egfr,
             serum_creat_lab_date,
             int_chunk,
             above_thrs) %>%
      dplyr::filter(above_thrs == 0L,
                    (egfr < egfr_max | egfr >= egfr_max)) %>%
      group_by(person_id, site, int_chunk) %>%
      dplyr::mutate(
        min_egfr_date = min(serum_creat_lab_date, na.rm = TRUE),
        max_egfr_date = max(serum_creat_lab_date, na.rm = TRUE),
        egfr_sep = as.numeric(date_diff('days', min_egfr_date, max_egfr_date))
      ) %>%
      dplyr::filter(egfr_sep >= 90) %>%
      group_by(person_id, site) %>%
      summarize(index_date = min(min_egfr_date, na.rm = TRUE)) %>%
      ungroup() 
    
    return(rslt_tbl)
  }



#' Get last inpatient visit dates for cohort as a follow-up estimate upper bound
#'
#' @param cohort Cohort of interest
#' @param inperson_visit_codeset codedset for inpatient visits
#' @param visit_tbl CDM or mini-CDM visit table
#'
#' @return table with the last in-person visit date
get_cohort_ce_age_fu_date <- function(cohort,
                                 inperson_visit_codeset,
                                 visit_tbl) {

  vec <- inperson_visit_codeset %>% pull(concept_id)
  rslt <-
    visit_tbl %>% 
    filter(visit_concept_id %in% vec) %>%
    inner_join(cohort) %>% 
    group_by(person_id, site,index_date) %>%
    summarise(fu_end_date = max(visit_start_date)) %>%
    inner_join(cdm_tbl('person') %>%
                 select(person_id, birth_date)) %>%
    mutate(ce_age_in_days = date_diff('days', birth_date, index_date)) %>% 
    ungroup() 
  
  return(rslt)
}


#' Function to stage ckd into 1 - 5
#'
#' @param ckd_cohort CKD cohort table `S8`
#' @param egfr_table table with egfr value `S5`
#'
#' @return
#' @export
#'
#' @examples
stage_ckd <- function(ckd_cohort, egfr_table,transplant,dialysis) {
  egfr <- ckd_cohort %>%
    inner_join(egfr_table) %>%
    group_by(person_id) %>%
    mutate(most_rec_egfr_date = max(serum_creat_lab_date), .after = serum_creat_lab_date) %>%
    ungroup() %>%
    mutate(egfr_int = as.integer(round(egfr, digits = 0))) %>%
    mutate(ckd_stage = case_when(
      egfr_int > 90 ~ "Stage 1",
      egfr_int >= 60 & egfr_int <= 90 ~ "Stage 2",
      egfr_int >= 45 & egfr_int <= 59 ~ "Stage 3a",
      egfr_int >= 30 & egfr_int <= 44 ~ "Stage 3b",
      egfr_int >= 15 & egfr_int <= 29 ~ "Stage 4",
      egfr_int <= 15 ~ "Stage 5"
    )) 
  
  
  ckd_stage_most_recent <- egfr %>%
    select(person_id, serum_creat_lab_date, most_rec_egfr_date, egfr_int, ckd_stage_mr = ckd_stage) %>%
    group_by(person_id) %>%
    filter(serum_creat_lab_date == most_rec_egfr_date) %>%
    filter(egfr_int == min(egfr_int)) %>%
    distinct() %>%
    ungroup() %>%
    select(person_id, ckd_stage_mr) 
  
  ckd_stage_most_recent_below_90 <- egfr %>%
    select(person_id, serum_creat_lab_date, most_rec_egfr_date, egfr_int, ckd_stage_mr_below_90 = ckd_stage) %>%
    filter(egfr_int <= 90) %>%
    group_by(person_id) %>%
    filter(serum_creat_lab_date == max(serum_creat_lab_date)) %>%
    filter(egfr_int == min(egfr_int)) %>%
    distinct() %>%
    ungroup() %>%
    select(person_id, ckd_stage_mr_below_90) 
  
  ckd_stage_CE <- egfr %>%
    select(person_id, index_date, serum_creat_lab_date, egfr_int, ckd_stage_ce = ckd_stage) %>%
    group_by(person_id) %>%
    filter(serum_creat_lab_date == index_date) %>%
    filter(egfr_int == min(egfr_int)) %>%
    distinct() %>%
    ungroup() %>%
    select(person_id, ckd_stage_ce) 
  
  
  ckd_cohort_with_stages <- ckd_cohort %>%
    left_join(ckd_stage_most_recent) %>%
    left_join(ckd_stage_most_recent_below_90) %>%
    left_join(ckd_stage_CE) %>%
    left_join(transplant) %>%
    left_join(dialysis) %>%
    mutate(transplant_event = ifelse(is.na(transplant_event),"no transplant",transplant_event),
           dialysis_event = ifelse(is.na(dialysis_event),"no dialysis",dialysis_event),
           dialysis = ifelse(dialysis_event == "no dialysis","No","Yes"),
           transplant = ifelse(transplant_event == "no transplant","No","Yes"),
           final_ckd_stage = case_when(transplant == "Yes" ~ "Kidney Transplant",
                                              dialysis == "Yes" ~ "Chronic Dialysis",
                                              T ~ ckd_stage_mr),
           ce_ckd_stage = case_when(transplant == "Yes" ~ "Kidney Transplant",
                                       dialysis == "Yes" ~ "Chronic Dialysis",
                                       T ~ ckd_stage_ce),
           final_ckd_stage_below_90 = case_when(transplant == "Yes" ~ "Kidney Transplant",
                                    dialysis == "Yes" ~ "Chronic Dialysis",
                                    T ~ ckd_stage_mr_below_90)) 
  
  ckd_cohort_with_stages
}


#' Title
#'
#' @param cohort 
#' @param co 
#' @param po 
#' @param transplant_dx 
#' @param transplant_px 
#'
#' @return
#' @export
#'
#' @examples
tag_Ktransplant_status <- function(cohort,
                                     co = cdm_tbl("condition_occurrence"),
                                     po = cdm_tbl('procedure_occurrence'),
                                     transplant_code = load_codeset('kt_dx_px_code_ratios_final')) {
  
  cohort_transplant_dx <- cohort %>% 
    select(person_id,index_date) %>%
    inner_join(co) %>%
    inner_join(transplant_code, sql_on = paste0(
      "(condition_concept_id = concept_id) OR ",
      "(condition_source_concept_id = concept_id)"
    )) %>%
    distinct(person_id,transplant_date = condition_start_date) 
  
  cohort_transplant_px <- cohort %>% 
    select(person_id) %>%
    inner_join(po) %>%
    inner_join(transplant_code, by = c("procedure_concept_id" = "concept_id")) %>%
    distinct(person_id,transplant_date = procedure_date) 
  
  tranplant <- union(cohort_transplant_dx, cohort_transplant_px) 
  
  pre_ce <- cohort %>%
    inner_join(tranplant %>%
                 distinct(person_id,transplant_date)) %>%
    mutate(transplant_event = ifelse(transplant_date < index_date, "beforeCE","afterCE")) %>%
    filter(transplant_event == "beforeCE") %>%
    distinct(person_id,transplant_event)
  
  post_ce <- tranplant %>% 
    distinct(person_id,transplant_date) %>%
    anti_join(pre_ce) %>%
    inner_join(cohort) %>%
    mutate(transplant_event = ifelse(transplant_date < index_date, "beforeCE","afterCE")) %>%
    mutate(transplant_event == "afterCE") %>%
    distinct(person_id,transplant_event) 
  
  post_ce %>% count(transplant_event) 
  
  final_cohort <- union(pre_ce,post_ce) 
  
  final_cohort
  
}

#' Title
#'
#' @param cohort 
#' @param po 
#' @param dialysis_codeset 
#' @param egfr_table 
#'
#' @return
#' @export
#'
#' @examples
tag_Cdialysis_status <- function(cohort = results_tbl('ckd_cohort'),
                                        po = cdm_tbl('procedure_occurrence'),
                                        dialysis_codeset = load_codeset('broad_chronic_dialysis_codes_with_ratio_with_concept_id'),
                                        egfr_table = results_tbl('egfr_data')) {
  
  
  group_A <- cohort %>% 
    inner_join(po) %>%
    inner_join(dialysis_codeset, by = c('procedure_concept_id' = 'concept_id')) %>%
    filter(true_pos_ratio >= 3.28) %>%
    distinct(person_id,procedure_date,index_date) %>%
    mutate(dialysis_event = ifelse(procedure_date < index_date, "beforeCE","afterCE")) 
  
  group_B_c1_pre_ce <-  cohort %>% 
    inner_join(po) %>%
    inner_join(dialysis_codeset, by = c('procedure_concept_id' = 'concept_id')) %>%
    filter(true_pos_ratio <= 3.28 & procedure_date < index_date) %>% 
    group_by(person_id,index_date) %>%
    summarise(total_codes = n_distinct(procedure_concept_id),procedure_date = min(procedure_date),index_date=index_date) %>%
    filter(total_codes >= 2) %>%
    select(person_id,procedure_date,index_date) %>%
    mutate(dialysis_event = "beforeCE") %>%
    ungroup()
  
  group_B_c2_pre_ce <- cohort %>%
    inner_join(egfr_table) %>%
    filter(egfr < 15 & serum_creat_lab_date < index_date) %>%
    group_by(person_id) %>%
    summarise(total_egfr_measurements = n_distinct(egfr)) %>%
    filter(total_egfr_measurements >= 5) %>%
    select(person_id) %>%
    ungroup() 
  
  groupB_pre_ce <- group_B_c1_pre_ce %>% inner_join(group_B_c2_pre_ce) %>% anti_join(group_A)
  
  group_B_c1_post_ce <-  cohort %>% 
    inner_join(po) %>%
    inner_join(dialysis_codeset, by = c('procedure_concept_id' = 'concept_id')) %>%
    filter(true_pos_ratio <= 3.28 & procedure_date >= index_date) %>% 
    group_by(person_id,index_date) %>%
    summarise(total_codes = n_distinct(procedure_concept_id),procedure_date = min(procedure_date),index_date=index_date) %>%
    filter(total_codes >= 2) %>%
    select(person_id,procedure_date,index_date) %>%
    mutate(dialysis_event = "afterCE") %>%
    ungroup() 
  
  group_B_c2_post_ce <- cohort %>%
    inner_join(egfr_table) %>%
    filter(egfr < 15 & serum_creat_lab_date >= index_date) %>%
    group_by(person_id) %>%
    summarise(total_egfr_measurements = n_distinct(egfr)) %>%
    filter(total_egfr_measurements >= 5) %>%
    select(person_id) %>%
    ungroup() 
  
  groupB_post_ce <- group_B_c1_post_ce %>% inner_join(group_B_c2_post_ce) %>% anti_join(group_A,by = 'person_id') %>% anti_join(groupB_pre_ce, by = "person_id") 
  
  groupB <- union(groupB_post_ce,groupB_pre_ce)
  
  
  
  dialysis <- union(group_A,groupB)
  
  pre_ce <- dialysis %>%
    filter(dialysis_event == "beforeCE") %>%
    distinct(person_id,dialysis_event) 
  
  post_ce <- dialysis %>% 
    anti_join(pre_ce,by = "person_id") %>%
    mutate(dialysis_event == "afterCE") %>%
    distinct(person_id,dialysis_event)
  
  union(pre_ce,post_ce) 
  
}