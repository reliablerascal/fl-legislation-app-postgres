all_data <- list(
  app01_vote_patterns = app01_vote_patterns,
  app02_leg_activity = app02_leg_activity,
  jct_bill_categories = jct_bill_categories,
  app03_district_context = app03_district_context,
  app03_district_context_state = app03_district_context_state
)

saveRDS(all_data, file = "data/all_data.rds")