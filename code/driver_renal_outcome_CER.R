

message('Implementing eGFR decline logic')
egfr_decline <- find_egfr_decline(cohort=cohort_pts,
                                  dat_tbl=results_tbl('egfr_bounded'),
                                  decline_prop=0.5)

message('Finding low eGFR')
egfr_low <- results_tbl('egfr_bounded')%>%
  inner_join(cohort_pts)%>%
  filter(egfr_date>ce_date&
           egfr<15)%>%
  group_by(patid)%>%
  summarise(min_cro_below_15_date=min(egfr_date))%>%
  ungroup()%>%
  mutate(cro_below_15=TRUE)