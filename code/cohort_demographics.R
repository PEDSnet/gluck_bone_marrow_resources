#' This function takes in person table and returns
#' the desired four demographic counts with percentages for Age Categorized,
#' Race, Hispanicity, Gender. 
#' @param person_tbl Person  Table
#' @param cohort Cohort table with person_id, participant_id, cohort_start_date, site
#' 
#' 
get_cohort_demographics <- function(cohort,
                             person_tbl = cdm_tbl('person')) {
  
  demog_table <-  cohort %>%
    inner_join(person_tbl, by = c('person_id', 'site')) %>%
    mutate(date = cohort_start_date) %>%
    mutate(age_at_enrollment_days = date_diff('day', birth_date, date),
      age_at_enrollment = age_at_enrollment_days / 365.25,
      
      # Age categories
      age_at_enrollment_cat = case_when(
        age_at_enrollment <= 1 ~ "< 1 year",
        age_at_enrollment > 1 & age_at_enrollment <= 4 ~ "01 - 04 years",
        age_at_enrollment > 4 & age_at_enrollment <= 11 ~ "05 - 11 years",
        age_at_enrollment > 11 & age_at_enrollment <= 18 ~ "12 - 18 years",
        age_at_enrollment > 18 & age_at_enrollment <= 25 ~ "19 - 25 years",
        age_at_enrollment > 25 ~ "25+"
      ),
      administrative_sex = case_when(
        gender_concept_name == "MALE" ~ "Male",
        gender_concept_name == "FEMALE" ~ "Female",
        TRUE ~ "Other/Unknown"
      ),
      
      race = case_when(
        race_concept_name == "White" ~ "White",
        race_concept_name == "Black or African American" ~ "Black or\n African American",
        race_concept_name == "Asian" ~ "Asian",
        race_concept_name == "Multiple race" ~ "Multiple race",
        race_concept_name == "Unknown" ~ "Unknown",
        TRUE ~ "Other"
      ),
      
      ethnicity = case_when(
        ethnicity_concept_name == "Hispanic or Latino" ~ "Hispanic or Latino",
        ethnicity_concept_name == "Not Hispanic or Latino" ~ "Not Hispanic or Latino",
        TRUE ~ "Other/Unknown"
      ),
      
      scr_age_group  = case_when(
        age_at_enrollment <= 1 ~ "<1 year",
        age_at_enrollment >= 1 & age_at_enrollment < 2 ~ "1 to <2 years",
        age_at_enrollment >= 2 & age_at_enrollment < 3 ~ "2 to <3 years",
        age_at_enrollment >= 3 & age_at_enrollment < 4 ~ "3 to <4 years",
        age_at_enrollment >= 4 & age_at_enrollment < 5 ~ "4 to <5 years",
        age_at_enrollment >= 5 & age_at_enrollment < 6 ~ "5 to <6 years",
        age_at_enrollment >= 6 & age_at_enrollment < 7 ~ "6 to <7 years",
        age_at_enrollment >= 7 & age_at_enrollment < 8 ~ "7 to <8 years",
        age_at_enrollment >= 8 & age_at_enrollment < 9 ~ "8 to <9 years",
        age_at_enrollment >= 9 & age_at_enrollment < 10 ~ "9 to <10 years",
        age_at_enrollment >= 10 & age_at_enrollment < 11 ~ "10 to <11 years",
        age_at_enrollment >= 11 & age_at_enrollment < 12 ~ "11 to <12 years",
        age_at_enrollment >= 12 & age_at_enrollment < 13 ~ "12 to <13 years",
        age_at_enrollment >= 13 & age_at_enrollment < 14 ~ "13 to <14 years",
        age_at_enrollment >= 14 & age_at_enrollment < 15 ~ "14 to <15 years",
        age_at_enrollment >= 15 & age_at_enrollment < 16 ~ "15 to <16 years",
        age_at_enrollment >= 16 & age_at_enrollment < 17 ~ "16 to <17 years",
        age_at_enrollment >= 17 & age_at_enrollment < 18 ~ "17 to <18 years"))  %>%
    select(participant_id,
           person_id,
           cohort_start_date, 
           cohort_start_datetime,
           birth_date,
           age_at_enrollment,
           age_at_enrollment_cat,
           administrative_sex,
           race,
           ethnicity, scr_age_group, site) 
  
  return(demog_table)
}




#' This function takes in death table and returns
#' the death record for each participant
#' @param death_tbl CDM death table
#' @param cohort Cohort table with person_id, participant_id, cohort_start_date, cohort_start_datetime, site

get_cohort_death_record <- function(cohort, 
                                    death_tbl = cdm_tbl('death')) {
  
  death_table <-  cohort %>%
    inner_join(death_tbl, by = c('person_id', 'site')) %>%
    group_by(participant_id, death_datetime) %>%
    slice_min(death_datetime, n=1, with_ties = FALSE) %>%
    select(person_id, 
           participant_id, 
           cohort_start_date, 
           cohort_start_datetime, 
           site, 
           death_date, 
           death_datetime, 
           cause_concept_name, 
           cause_source_concept_name, 
           death_age_in_months)
  
}
  