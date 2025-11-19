
#' Function to find occurrences of labs based on a codeset
#' @param lab_codes codeset containing at least a concept_code column which is expected to be a LOINC code
#' @param lab_tbl table with lab occurrences, defaulting to the lab_results_cm cdm table
#' @param cohort table with at least a patid column
#' @return table with lab results for the cohort for the codeset of interest
find_lab_occurrences <- function(lab_codes,
                                 lab_tbl=cdm_tbl('lab_result_cm'),
                                 cohort) {
  lab_tbl %>%
    inner_join(select(cohort, patid), by = 'patid') %>%
    inner_join(
      select(lab_codes,
             c(concept_code, concept_name)),
      by=c('lab_loinc'='concept_code')
    ) %>%
    rename(loinc_name=concept_name)
}

#' This function takes a table of urine protein measurements 
#' in the PCORnet CDM format which minimally contains :
#'        result_qual | result_num | result_unit | 
#'        result_modifier | raw_result | raw_unit
#' and adds the columns to the input table:
#' derived_result_category: "NEGATIVE", "TRACE", "POSITIVE", "1+", "2+", "3+", NA
#' derived_result_pos_neg: "NEGATIVE", "POSITIVE", NA
#'
#' Information about classification:
#' Negative – corresponds to < 30 mg/dL (or 1+)
#' Positive -
#'     2+ - corresponds to 100-<300 mg/dL
#'     3+ - corresponds to 300-<1000 mg/dL
#'     4+ - corresponds to 1000-<=2000 mg/dL
#' UNAVAILABLE: Result unavailable (e.g. due to problem with testing) or current
#' approach to classifying measurements cannot derive result
#'
#' @param lab_tbl Table of urine protein
#' measurements which minimally contains columns:
#'        result_qual | result_num | result_unit | 
#'        result_modifier | raw_result | raw_unit
#'
#' @return Table of urine protein or proteinuria urinalysis measurements with
#' additional derived result variables
#'
classify_proteinuria<-function(lab_tbl){
  # pre-processing to handle information in raw fields not in structured field
  # pp_lab_tbl<- lab_tbl %>%
  #   mutate()
  # straightforward semi-quantitative case: result_num populated
  lab_quant_cat <- lab_tbl %>%
    mutate(derived_result_category=case_when(
      # semi-quantitative
      (result_num<15)|
        (result_num==15&result_modifier=='LT')|
        # qualitative
        result_qual%in%c('NEGATIVE','NORMAL','NOT DETECTED','A NEG')|
        (result_qual=='SMALL'&raw_result=='=1.00000')~'NEGATIVE',
      # semi-quantitative
      (result_num<30)|
        (result_num==30&result_modifier=='LT')|
        # qualitative
        result_qual=='TRACE'~'TRACE',
      # semi-quantitative
      (result_num<100)|
        (result_num==100&result_modifier=='LT')|
        # qualitative
        result_qual=='1+'~'1+',
      # semi-quantitative
      (result_num<300)|
        (result_num==300&result_modifier=='LT')|
        # qualitative
        result_qual=='2+'~'2+',
      # semi-quantitative
      (result_num<1000)|
        (result_num==1000&result_modifier=='LT')|
        # qualitative
        result_qual=='3+'~'3+',
      (result_num<=2000|
         result_qual=='4+')~"4+",
      (result_num>=100&
         result_modifier%in%c('GE','GT', 'EQ'))|
        result_qual%in%c('MODERATE','LARGE','POSITIVE')|
        (result_qual=='SMALL'&raw_result=='2+ (100)')~'POSITIVE',
      TRUE~NA_character_),
      derived_result_pos_neg=case_when(derived_result_category%in%c('NEGATIVE', 'TRACE', '1+')~'NEGATIVE',
                                       derived_result_category%in%c('2+','3+','4+','POSITIVE')~'POSITIVE'))
  
}