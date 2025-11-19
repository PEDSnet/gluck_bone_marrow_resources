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
  init_sum(cohort = 'Start', persons = 0)
  
  rslt <- list()
  rslt$cohort_pts <- results_tbl('tte_cohort_penult') 
  
  rslt$cohort_urine_protein_qual <- find_lab_occurrences(lab_codes=load_codeset('lab_urine_protein_qual','icccc'),
                                                         lab_tbl=cdm_tbl('lab_result_cm'),
                                                         cohort=rslt$cohort_pts) %>%
    mutate(lab_type='qual') %>%
    compute_new(temporary=TRUE)
  # check result_qual (should be NI for quantitative results). there's a value set (can see in the parseable spreadsheet)
  # see if they have a result_snomed code (if qualitative result has been mapped to SNOMED)
  # confirmed that using the v6.1 CDM value sets, there were no values in table that were not in vs
  
  rslt$cohort_urine_protein_quant <- find_lab_occurrences(lab_codes=load_codeset('lab_urine_protein_quant','icccc'),
                                                          lab_tbl=cdm_tbl('lab_result_cm'),
                                                          cohort=rslt$cohort_pts)%>%
    mutate(lab_type='quant') %>%
    compute_new(temporary=TRUE)
  # check result_num: should include the component of numeric results that contain operators (e.g. <200)
  # result_modifier: EQ, GE, etc.
  # result_unit should have the unit
  # result_qual should be NI for quantitative results
  
  rslt$cohort_urine_protein_quant_qual <- rslt$cohort_urine_protein_qual %>%
    dplyr::union_all(rslt$cohort_urine_protein_quant)%>%
    left_join(select(load_codeset('vs_result_qual', col_types='ccc', indexes='valueset_item'),c(valueset_item,valueset_item_descriptor)),
              by = c('result_qual'='valueset_item')) %>%
    rename(result_qual_descriptor=valueset_item_descriptor) %>%
    left_join(select(load_codeset('vs_result_unit', col_types='ccc', indexes='valueset_item'),c(valueset_item,valueset_item_descriptor)),
              by = c('result_unit'='valueset_item')) %>%
    rename(result_unit_descriptor=valueset_item_descriptor) %>%
    left_join(select(load_codeset('vs_result_modifier', col_types='ccc', indexes='valueset_item'),c(valueset_item,valueset_item_descriptor)),
              by = c('result_modifier'='valueset_item')) %>%
    rename(result_modifier_descriptor=valueset_item_descriptor)%>%
    distinct()
  
  # raw_result can be checked for all of them
  output_tbl(rslt$cohort_urine_protein_quant_qual,
             name='lab_urine_protein')
  
  message('Classifying proteinuria')
  # Apply site specific classifications of whether each loinc code is (1)qual/semi-quant or (2) quant
  # qual/semi-quant measures only are appropriate for classify_proteinuria() function
  # (quant measures would be used to compute UPCRs)
  # urine_prot_lab_type_review was generated based on manual review of histograms using results_tbl('lab_urine_protein')
  # as input (urine_prot_lab_type_review.Rmd)
  rslt$urine_prot_lab_type_review <-
    load_codeset("urine_prot_lab_type_review",
                 indexes = c(),
                 col_types = "ccccii")
  rslt$prot_class <- results_tbl('lab_urine_protein') %>%
    inner_join(rslt$urine_prot_lab_type_review,
               by = c('lab_loinc', 'site', 'lab_type')) %>%
    filter(lab_type_review == "qual") %>%
    classify_proteinuria()
  # leaving commented out in case we need to look back, but this was the prior version of the table before classifying raw_result, which is now replaced with a more complete version
  
  #output_tbl(rslt$prot_class,
  #           name='lab_urine_protein_cat')
  
  message('Classify unmapped measurements')
  # For measurements without structured result but with raw_result, generate file to assess
  rslt$prot_noclass <- rslt$prot_class%>%
    filter(!is.na(raw_result)&is.na(derived_result_category)&is.na(result_num))%>%
    group_by(site, raw_result)%>%
    summarise(num_meas=n(),
              npat=n_distinct(patid))%>%
    ungroup()%>%
    output_tbl(name='urine_protein_raw_result_counts',
               db=FALSE,
               file=TRUE)
  
  # STOP here: manually classify by hand, remove counts, move over to specs directory named urine_protein_raw_result_class
  
  # read in the categorized values, assign them if the derived result is missing otherwise
  rslt$prot_post_class<- rslt$prot_class%>%
    left_join(load_codeset('urine_protein_raw_result_class',
                           indexes=c(),
                           col_types='cccc')%>%rename(new_pos_neg=derived_result_pos_neg,
                                                      new_cat=derived_result_category),
              by = c('site', 'raw_result'))%>%
    mutate(derived_result_pos_neg=coalesce(derived_result_pos_neg, new_pos_neg),
           derived_result_category=coalesce(derived_result_category, new_cat))%>%
    select(-c(new_pos_neg, new_cat))
  
  output_tbl(rslt$prot_post_class,
             name='lab_urine_protein_cat')
  
  message('Done.')
  
  invisible(rslt)
  
}