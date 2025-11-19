
#' Identify visit specialty information in PRESERVE cohort
#'
#' @param cohort - cohort of patients with at least site & patid columns
#'
#' @return table that summarizes visit specialty and type information for each patient in the cohort.
#'         examines hospitalizations and ED visits
#' 
#'         includes columns: site, patid, ce_date, max_visit, fu, ed_visit_ct, ed_visit_ppy, hosp_days, hosp_days_ppy
#' 

find_visits_tte <- function(cohort,
                            min_date_col = 'tte_baseline_date',
                            max_date_col = 'fu_end_date'){
  
  ref <- cdm_tbl('encounter') %>%
    select(patid, encounterid, providerid, enc_type, facility_type, 
           admit_date, discharge_date) %>%
    inner_join(cohort) %>%
    group_by(patid) %>%
    filter(admit_date >= !!sym(min_date_col),
           admit_date <= !!sym(max_date_col)) %>%
    mutate(max_visit_fu = max(admit_date),
           fu = (max_visit_fu - !!sym(min_date_col))/365.25) %>%
    ungroup() %>%
    compute_new()
  
  ed <- ref %>% filter(fu != 0) %>%
    filter(enc_type == 'ED' | enc_type == 'EI') %>%
    filter(admit_date >= !!sym(min_date_col) & admit_date <= !!sym(max_date_col)) %>%
    group_by(patid, fu) %>%
    summarise(ed_visit_ct = n_distinct(encounterid)) %>%
    mutate(ed_visits_ppy = round((ed_visit_ct / fu), 2)) #%>%
  #select(-visit_ct)
  
  hospital <- ref %>% filter(fu != 0) %>%
    filter(enc_type == 'IP' | enc_type == 'EI') %>%
    filter(admit_date >= !!sym(min_date_col) & admit_date <= !!sym(max_date_col)) %>%
    distinct(patid, admit_date, discharge_date, .keep_all = TRUE) %>%
    mutate(visit_days = discharge_date - admit_date,
           visit_days = ifelse(visit_days == 0, 1, visit_days)) %>%
    group_by(patid, fu) %>%
    summarise(hosp_days = sum(visit_days)) %>%
    mutate(hosp_days_ppy = round((hosp_days / fu), 2)) #%>%
  #select(-hosp_days)
  
  final <- ref %>%
    select(site, patid, tte_baseline_date, fu_end_date, max_visit_fu, fu) %>%
    #mutate(study_start = as.Date(min_date)) %>%
    distinct() %>%
    left_join(ed) %>%
    left_join(hospital) %>% collect_new() %>%
    mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))
  
  
}