all_data <- list(
  app_data = app_data,
  app_vote_patterns = app_vote_patterns,
  jct_bill_categories = jct_bill_categories
)

saveRDS(all_data, file = "data/all_data.rds")