#' Optional function to prep omop codeset version if you want to use it 
#' with pcornet
crosswalk_concept_to_icd <- function(pmca_xwalk){
  rv <- vocabulary_tbl('concept') %>% 
    select(concept_id, concept_code, vocabulary_id) %>% 
    inner_join(pmca_xwalk,by='concept_id') %>%
    rename(dx=concept_code)
  
  return(rv)
}

read_pmca_file <- function(){
  test <- readxl::read_excel(paste0(base_dir, '/', config('subdirs')$spec_dir, '/pmca_dx_code_lists.xlsx'),sheet='icd-9',
                             col_types = c("text", "text", "text", "text", "text")) %>% 
    mutate(dx_nodecimal=as.character(dx_nodecimal),dx_type='09') %>% 
    pivot_longer(cols=c('dx','dx_nodecimal'),values_to = 'dx') %>% 
    union(readxl::read_excel(paste0(base_dir, '/', config('subdirs')$spec_dir, '/pmca_dx_code_lists.xlsx'),sheet='icd-10',
                             col_types = c("text", "text", "text", "text", "text")) %>% 
            mutate(dx_nodecimal=as.character(dx_nodecimal),dx_type='10') %>% 
            pivot_longer(cols=c('dx','dx_nodecimal'),values_to = 'dx')) %>%
    select(-name) %>% distinct()
  return(copy_to_new(df=test))
}

#' Function from pasc25_hazard_ratio / code / cohort_pmca
#' produce PMCA table wiith 3 year lookback
#'
#' @param cohort cohort of patients with patients and observation_date
#' @param pmca_xwalk codeset that has flags for body systems and whether or not progressive
#' @param condition_tbl formatting of the condition occurrence table
#' @param visit_tbl formatting of the visit occurrence table
#'
#' @return table that has conditions and flags for body systems
#' columns:
#' person_id | observation_date | result_derivation | condition_concept_id | condition_concept_name |
#' condition_source_concept_id | condition_source_value | visit_occurrence_id | condition_start_state |
#' description | body_system | progressive | visit_concept_id | site
#'

produce_pmca_lookup <- function (cohort,
                                 icd_codes=read_pmca_file(),
                                 dx_tbl=cdm_tbl('diagnosis'),
                                 enc_tbl=cdm_tbl('encounter')) {
  #icd_codes <- crosswalk_concept_to_icd(pmca_xwalk)
  only_pmca_conds <-
    cohort %>%  
    inner_join(
      select(dx_tbl,
             patid,
             encounterid,
             enc_type,
             dx_date,
             dx,
             dx_type
      ) %>%
        filter(enc_type %in% c('AV','ED','EI','IP','IS','OS','TH')),
      #9202L,9201L,9203L,2000000048L,2000000088L,581399L OMOP concept_id equivalents
      # AGD removed OA as this includes non face-to-face telephone calls
      by=c('patid')
    ) %>% filter(observation_date - dx_date <= 1096L &
                   observation_date - dx_date >= 0L) %>%
    inner_join(
      icd_codes,
      by=c('dx','dx_type')
    )  %>% 
    distinct()
  
  return(only_pmca_conds)
}



#' Function from pasc25_hazard_ratio / code / cohort_pmca
#' compute patient, body system with visit number, and flags for malignancy and progressive
#'
#' @param pmca_lookup_tbl pmca table output from `produce_pmca_lookup`;
#' must contain `observation_date` and `body_system` and `condition_start_date` and flagged conditions
#'
#' @return computes information to be able to apply algorithms; groups by body system and counts visits,
#' with flags for progressive or malignancy for patients
#'
#' person_id | observation_date | result_derivation | body_system |
#' yr_1 | yr_2 | yr_3 | total_visits | progressive | malignancy
#'

compute_pmca_summary <- function(pmca_lookup_tbl) {
  
  
  add_year <-
    pmca_lookup_tbl %>%
    filter(
      ! body_system == 'malignancy'
    ) %>% mutate(
      flag_yr = case_when(
        dx_date < observation_date &
          dx_date >= sql("(observation_date - interval '1 year')::date") ~ 'yr_1',
        dx_date < sql("(observation_date - interval '1 year')::date") &
          dx_date >= sql("(observation_date - interval '2 years')::date") ~ 'yr_2',
        dx_date < sql("(observation_date - interval '2 years')::date") &
          dx_date >= sql("(observation_date - interval '3 years')::date") ~ 'yr_3',
        TRUE ~ 'no_yr'
      )) %>%
    group_by(
      patid,
      observation_date,
      body_system,
      flag_yr
    ) %>% summarise(
      visit_yr_ct=as.integer(n_distinct(dx_date))
    ) %>% pivot_wider(names_from = flag_yr,
                      values_from = visit_yr_ct,
                      values_fill = 0L) %>%
    mutate(total_visits = yr_1 + yr_2 + yr_3) %>%
    ungroup() 
  
  progressive_malignant_pts <-
    pmca_lookup_tbl %>%
    filter(
      progressive == 'yes' |
        body_system == 'malignancy'
    ) %>% mutate(malignancy =
                   case_when(
                     body_system == 'malignancy' ~ 'yes',
                     TRUE ~ 'no'
                   )) %>%
    filter(malignancy=='yes' | progressive == 'yes') %>%
    select(patid,
           progressive,
           malignancy) %>% distinct 
  
  all_pts <-
    dplyr::union(
      add_year %>% ungroup() %>% select(patid),
      progressive_malignant_pts %>% select(patid)
    )
  
  all_pts %>%
    left_join(add_year,by='patid') %>%
    left_join(progressive_malignant_pts,by='patid') %>%
    mutate(
      progressive=case_when(
        is.na(progressive) ~ 'no',
        TRUE ~ progressive
      ),
      malignancy=case_when(
        is.na(malignancy) ~ 'no',
        TRUE ~ malignancy
      )
    ) %>% select(
      patid,observation_date,
      body_system,yr_1,yr_2,yr_3,total_visits,progressive,malignancy
    )
  
}

compute_pmca_site <- function(cohort,
                              site_list=list('chop','cchmc',
                                             'colorado','lurie',
                                             'nationwide','nemours',
                                             'seattle','stanford')) {
  
  site_rslts <- list()
  
  for(i in 1:length(site_list)) {
    
    site_nm = site_list[[i]]
    
    pmca_lookup <- produce_pmca_lookup(cohort=cohort %>% filter(site==site_nm))
    pmca_summary <- compute_pmca_summary(pmca_lookup_tbl = pmca_lookup) %>%
      mutate(site=site_nm)
    
    site_rslts[[i]] <- pmca_summary
    
  }
  
  site_rslts
  
}




#' compute classification algorithm for *most conservative*:
#' *complex chronic* is defined as patients with at least 1 visit for two body systems for all three years OR progressive OR malignant
#' *chronic* is defined as having at least 1 visit all three years for just one body system
#'
#' @param pmca_lookup_tbl output from `produce_pmca_lookup`
#' @param cohort_tbl cohort table with person_id, observation_date and site
#'
#' @return table that has patients in the *most conservative* category with the following columns:
#' person_id | observation_date | body_system | yr_1 | yr_2 | yr_3 | total_visits |
#' progressive | malignancy | complex_chronic | chronic | non_complex_chronic
#'

compute_pmca_cats_cons <- function(pmca_summary_tbl,
                                   cohort_tbl) {
  
  gt_two_bs <-
    pmca_summary_tbl %>%
    filter(
      yr_1 > 0 &
        yr_2 > 0 &
        yr_3 > 0
    ) %>%
    group_by(patid,
             observation_date) %>%
    summarise(body_system_ct=n_distinct(body_system)) %>%
    filter(
      body_system_ct > 1
    ) %>% ungroup()
  
  prog_or_malig <-
    pmca_summary_tbl %>%
    filter(progressive == 'yes' | malignancy == 'yes')
  
  complex_pts <-
    dplyr::union(
      gt_two_bs %>% select(patid,observation_date),
      prog_or_malig %>% select(patid,observation_date)
    ) %>% mutate(complex_chronic = 1L) %>%
    compute_new(temporary=TRUE,
                indexes=list('patid'))
  
  chronic_pts <-
    pmca_summary_tbl %>%
    filter(
      yr_1 > 0 &
        yr_2 > 0 &
        yr_3 > 0
    ) %>% anti_join(complex_pts,
                    by=c('patid','observation_date')) %>%
    distinct(patid, observation_date) %>% mutate(chronic = 1L) %>%
    compute_new(temporary=TRUE,
                indexes=list('patid'))
  
  pmca_summary_tbl %>%
    right_join(cohort_tbl,
               by=c('patid','observation_date')) %>%
    left_join(complex_pts) %>%
    left_join(chronic_pts) %>%
    mutate(complex_chronic=case_when(is.na(complex_chronic) ~ 0L,
                                     TRUE ~ complex_chronic),
           chronic=case_when(is.na(chronic) ~ 0L,
                             TRUE ~ chronic)) %>%
    mutate(non_complex_chronic =
             case_when(complex_chronic == 1 | chronic == 1 ~ 0L,
                       TRUE ~ 1L)) %>% select(patid,
                                              observation_date,
                                              starts_with("site"),
                                              complex_chronic,
                                              chronic,
                                              non_complex_chronic) %>% distinct()
}






#' @param pmca_summary pmca summary table output from compute_pmca_summary. Counts are unique by person_id, body_system, and malignancy/progressive.
#' @param cohort_tbl full cohort tbl
#' @param pmca_category pmca_category table output from compute_pmca_cat_cons function
#' @return table with person-level flags for progressive/malignant and number of body systems affected
#'
pmca_all_flags <- function(pmca_summary_tbl,
                           cohort_tbl,
                           pmca_category) {
  
  progressive_malignancy_flag <- pmca_summary_tbl %>%
    mutate(progressive_num=case_when(progressive=='yes' ~ 1L,
                                     progressive=='no' ~ 0L),
           malignancy_num=case_when(malignancy=='yes' ~ 1L,
                                    malignancy=='no' ~ 0L)) %>%
    group_by(patid) %>%
    summarise(progressive_ct = sum(as.numeric(progressive_num)),
              malignancy_ct=sum(as.numeric(malignancy_num)))
  
  body_systems_count <- pmca_summary_tbl %>%
    group_by(patid, body_system) %>%
    mutate(body_system_flag=1L) %>%
    ungroup() %>%
    group_by(patid) %>%
    summarise(n_body_systems = count(body_system_flag)) %>%
    ungroup()
  
  
  final_flags <- pmca_summary_tbl %>%
    distinct(patid) %>%
    left_join(body_systems_count) %>%
    left_join(progressive_malignancy_flag) %>%
    right_join(select(cohort_tbl, patid)) %>%
    right_join(select(pmca_category, patid, starts_with("site"), complex_chronic, chronic, non_complex_chronic)) %>%
    mutate(
      progressive_ct = case_when(progressive_ct > 0L ~ progressive_ct,
                                 TRUE ~ 0L),
      malignancy_ct = case_when(malignancy_ct > 0L ~ malignancy_ct,
                                TRUE ~ 0L),
      n_body_systems = case_when(n_body_systems > 0L ~ n_body_systems,
                                 TRUE ~ 0L)) %>%
    select(patid,
           starts_with("site"),
           progressive_ct,
           malignancy_ct,
           complex_chronic,
           chronic,
           non_complex_chronic,
           n_body_systems)
  
}