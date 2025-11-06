


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
      inner_join(distinct(cohort, person_id), by = "person_id") %>%
      compute_new(list("visit_occurrence_id"))
    
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
      union(neph_specialty, neph_provider) %>%
      compute_new()
    
    
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
      inner_join(demographic_tbl) %>%
      compute_new(indexes = list('person_id'))
    
    fil_labs <- labs %>%
      mutate(age_at_measurement = round(measurement_age_in_months / 12)) %>%
      dplyr::filter(value_as_number > 0 & value_as_number <= 50) %>%
      dplyr::filter(between(measurement_date - birth_date, 365 * age_min, 365.25 * age_max)) %>%
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
      ) %>%
      compute_new(indexes = list('person_id'))
    
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
    summarise(days = min(abs(serum_creat_lab_date - height_date))) %>%
    dplyr::filter(days <= 180) %>%
    inner_join(
      heights_data %>%
        dplyr::filter(!is.na(ht)) %>%
        select(person_id, height_date = measurement_date, ht),
      by = 'person_id'
    ) %>%
    dplyr::filter(abs(serum_creat_lab_date - height_date) == days) %>%
    select(person_id, site, serum_creat_lab_date, height_date, ht) %>%
    distinct() %>%
    ungroup() %>%
    compute_new(indexes = list('person_id'))
  
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
    mutate(egfr = k_coeff * ht_meters / serum_creat_meas_value) %>%
    compute_new(name = "egfr", indexes = list('person_id'))
  
  if (requireNamespace('beepr', quietly = T) &
      exists('notify') & notify == T)
    (beepr::beep())
  
  return(egfr)
  
}
