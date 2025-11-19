
#' Function to find if patient has eGFR decline by at least the proportion specied
#' @param cohort table with at least patid | ce_date
#' @param dat_tbl table containing egfr values, with at least:
#'                    egfr (value of egfr)
#'                    egfr_date (date of egfr)
#'                    patid
#' @param decline_prop proportion by with first egfr should decline by
#'                      in order to be classified as having substantial decline
#' @return table with columns:
#'                      patid
#'                      min_cro_egfr_red_date: date on which patient first has substantial decline when compared with first
#'                      cro_egfr_red: indicator of whether patient had substantial decline (TRUE for anyone who meets definition)
find_egfr_decline <- function(cohort,
                              dat_tbl,
                              decline_prop){
  
  first_egfr<- cohort %>% select(patid, ce_date)%>%
    inner_join(dat_tbl, by=c('patid', 'ce_date'='egfr_date')) %>%
    group_by(patid, ce_date)%>%
    summarise(first_egfr=median(egfr))
  
  egfr_drop<-cohort%>%
    inner_join(dat_tbl) %>%
    inner_join(first_egfr)%>%
    filter(egfr_date>ce_date)%>%
    mutate(egfr_decline=egfr/first_egfr)%>%
    filter(egfr_decline<=decline_prop)%>%
    group_by(patid)%>%
    summarise(min_cro_egfr_red_date=min(egfr_date),
              cro_egfr_red=TRUE) %>%
    ungroup()
}

#' Function to take into consideration the end of the study time frame,
#'      the patient's last visit within the time frame,
#'      and the time between each consecutive visit,
#'      to find the patient's visit follow up and loss-to-follow-up
#' @param cohort table with at least patid and ce_date
#' @param visits table with encounters for cohort
#' @param study_end date of the end of the study time frame
#' @param loss_gap_days number of days between consecutive visits to consider patient lost to follow up. patient will be considered "lost" on the date of the last encounter prior to the gap. also, if there is a visit within this number of days from study end, patient's follow up will extend to the end of the study time frame
#' @return table with the columns:
#'            patid
#'            oss_to_fu: TRUE if patient had > loss_gap_days at any point between visits
#'            last_enc_date: date of last admit_date in study time frame
#'            loss_to_fu_date: if patient had > loss_gap_days between visits, the date of the last admit_date prior to the gap
#'            visit_fu_end_date: either the last_enc_date, or the end of the study time frame if the patient had a visit within loss_gap_days of the end of the study time frame
find_fu_visits<-function(cohort,
                         visits,
                         study_end='2021-12-31',
                         loss_gap_days){
  # find last overall visit in study period
  fu_visits<-cohort%>%select(patid, ce_date)%>%
    inner_join(visits, by = 'patid')%>%
    filter(admit_date>=ce_date&
             admit_date<=study_end) %>% 
    collect_new() # AGD: Problem with window_order so collect & arrange instead
  
  last_visits <- fu_visits %>%
    group_by(patid)%>%
    summarise(last_enc_date=max(admit_date)) %>% 
    collect_new() # AGD: Problem with window_order so collect & arrange instead
  
  # find if patient had > expected gap in visits
  loss_tbl<- fu_visits%>%
    group_by(site, patid) %>%
    arrange(admit_date) %>%  # AGD: Problem with window_order so collect & arrange instead
    mutate(# find days since last encounter
      days_since_last_enc=as.integer(admit_date-lag(admit_date)),
      # flag if days since last encounter > specified days
      enc_gap=case_when(days_since_last_enc>loss_gap_days~1L,
                        TRUE~0L),
      # if this visit or ANY previous had > specified days gap, remove
      tot_gap=as.integer(cumsum(enc_gap)),
      loss_to_fu=case_when(any(enc_gap==1)~TRUE,
                           TRUE~FALSE)) %>%
    ungroup()%>%
    # remove any visits after the gap
    filter(tot_gap<1L) %>%
    group_by(patid, loss_to_fu)%>%
    summarise(last_enc_date=max(admit_date))%>%
    ungroup()%>%
    mutate(loss_to_fu_date=case_when(loss_to_fu~last_enc_date),
           visit_fu_end_date=case_when(as.numeric(study_end-last_enc_date)<loss_gap_days~as.Date(study_end),
                                       TRUE~last_enc_date))
  
}

#' Function to take into consideration various definitions of end to follow up
#'     and derive dates and indicators of follow up end
#' @param cohort_tbl table with patid | site | ce_date
#' @param egfr_decl_tbl table with minimum date of eGFR decline for the patients who meet the eGFR decline definition
#' @param egfr_low_tbl table with minimum date of low eGFR for the patients who meet the low eGFR definition
#' @param usrds_tbl table with minimum dates of transplant and dialysis for the patients who meet the transplant or dialysis endpoint definitions
#' @param visit_tbl table with final visit in the study window and indicators of loss to follow up
#' @return table with boolean for each of the endpoint indicators and minimum dates for each endpoint. see data dictionary for more information
derive_end_dates <- function(cohort_tbl,
                             egfr_decl_tbl,
                             egfr_low_tbl,
                             usrds_tbl,
                             visit_tbl){
  cohort_tbl %>%
    left_join(egfr_decl_tbl)%>%
    left_join(egfr_low_tbl)%>%
    left_join(usrds_tbl)%>%
    left_join(visit_tbl)%>%
    collect()%>%
    mutate(across(where(is.logical),~replace(.x, is.na(.x), FALSE)))%>%
    rowwise()%>%
    # assign CRO date and flag
    mutate(cro_type_2_date=min(c_across(c("min_cro_transplant_date",
                                          "min_cro_dialysis_date")),na.rm=TRUE),
           cro_type_3_date=min(c_across(c("min_cro_transplant_date",
                                          "min_cro_dialysis_date", 
                                          "min_cro_below_15_date")), na.rm=TRUE),
           cro_type_4_date=min(c_across(c("min_cro_transplant_date",
                                          "min_cro_dialysis_date", 
                                          "min_cro_below_15_date", 
                                          "min_cro_egfr_red_date")), na.rm=TRUE))%>%
    pivot_longer(cols=starts_with("cro_type"),
                 values_to = "min_cro_date",
                 names_to="cro_type")%>%
    mutate(cro_type=as.integer(str_extract(cro_type,"\\d")))%>%
    mutate(cro_type_def=case_when(cro_type==2L~"usrds dialysis or usrds kidney transplant",
                                  cro_type==3L~"usrds dialysis or usrds kidney transplant or egfr <15",
                                  cro_type==4L~"usrds dialysis or usrds kidney transplant or egfr <15 or egfr 50% reduction"),
           min_cro_date=case_when(is.infinite(min_cro_date)~NA,
                                  TRUE~min_cro_date),
           cro=case_when(!is.na(min_cro_date)~TRUE,
                         TRUE~FALSE))%>%
    rowwise()%>%
    # assign final end date
    mutate(fu_end_date=min(c_across(c("visit_fu_end_date",
                                      "loss_to_fu_date",
                                      "min_cro_date")),na.rm=TRUE))%>%
    mutate(fu_end_date=case_when(is.infinite(fu_end_date)~NA,
                                 TRUE~fu_end_date))%>%
    # mutate(min_column_name=paste(names(c("min_cro_transplant_date","min_cro_dialysis_date","last_enc_date"))[where(c("min_cro_transplant_date","min_cro_dialysis_date","last_enc_date")==cro_type_2_date)],collapse="_"))%>%
    ungroup()
  }