
# Cardiac Cohort
cardiac_records <-
  get_cardiac_records(
    cohort = cohort,
    pmca_codeset = load_codeset("pmca_codeset"),
    endpoint_tbl = results_tbl(in_schema('cer_paper_usrds', 'cohort_endpoint_dates_prs_364'), 
                               results_tag = FALSE) %>% filter(cro_type == 2) %>%
      rename('endpoint_date' = 'fu_end_date')
  ) %>%
  compute_new(indexes = list("patid", "record_id"))

cardiac_records %>% output_tbl("cardiac_records",
                               indexes = list("patid", "record_id"))

cardiac_cohort <-
  get_cardiac_cohort(
    cohort = cohort,
    cardiac_records = results_tbl("cardiac_records")
  )

cardiac_cohort %>% output_tbl("cardiac_cohort",
                              indexes = list('patid'))