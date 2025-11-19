# Vector of additional packages to load before executing the request
config_append('extra_packages', c("tidyr"))

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
  
  rslt <- list()
  init_sum(name = 'start',
           patids = 0,
           rows = 0)
  
  # Identify cohort
  rslt$pmca_cohort <-
    results_tbl('tte_cohort') %>%
    rename('observation_date' = tte_baseline_date)
  
  append_sum(
    name = 'start',
    patids = rslt$pmca_cohort %>% distinct_ct(id_col = 'patid'),
    rows = rslt$pmca_cohort %>% tally() %>% pull()
  )
  
  rslt$pmca_cohort %>% output_tbl("pmca_cohort",
                                  indexes = list('patid'))
  
  
  # Compute PMCA lookup 
  rslt$pmca_lookup <- produce_pmca_lookup(rslt$pmca_cohort) %>% # AGD: This needs to be edited so filepath for PMCA file can be provided
    compute_new(indexes = list('patid'))
  
  append_sum(
    name = 'start',
    patids = rslt$pmca_lookup %>% distinct_ct(id_col = 'patid'),
    rows = rslt$pmca_lookup %>% tally() %>% pull()
  )
  
  rslt$pmca_lookup %>% output_tbl("pmca_lookup",
                                  indexes = list('patid', 'encounterid'))
  
  # Summarize PMCA lookup
  rslt$pmca_summary <- compute_pmca_summary(rslt$pmca_lookup %>% filter(body_system != 'renal')) %>%
    compute_new(indexes = list('patid'))
  
  append_sum(
    name = 'start',
    patids = rslt$pmca_summary %>% distinct_ct(id_col = 'patid'),
    rows = rslt$pmca_summary %>% tally() %>% pull()
  )
  
  # Apply conservative PMCA algorithm
  rslt$pmca_cat_cons <-
    compute_pmca_cats_cons(rslt$pmca_summary, rslt$pmca_cohort) %>%
    compute_new(indexes = list('patid'))
  
  append_sum(
    name = 'start',
    patids = rslt$pmca_cat_cons %>% distinct_ct(id_col = 'patid'),
    rows = rslt$pmca_cat_cons %>% tally() %>% pull()
  )
  
  # Collate PMCA flags
  rslt$all_pmca_flags <-
    pmca_all_flags(rslt$pmca_summary, rslt$pmca_cohort, rslt$pmca_cat_cons) %>%
    compute_new(indexes = list('patid'))
  
  append_sum(
    name = 'start',
    patids = rslt$all_pmca_flags %>% distinct_ct(id_col = 'patid'),
    rows = rslt$all_pmca_flags %>% tally() %>% pull()
  )
  
  rslt$all_pmca_flags %>% output_tbl("non_renal_pmca_flags",
                                     indexes = list('patid'))
  
  # End
  #output_sum() --> didn't seem like needed to keep this around
  message("Done.")
}