local_css_file <- "www/styles.css"
full_path <- normalizePath(local_css_file, winslash = "/", mustWork = TRUE)

# Define the destination in the S3 bucket
s3_bucket_path <- "s3://legislative-compass/styles.css"

# Run AWS CLI command to upload the CSS file
system(paste("aws s3 cp", local_css_file, s3_bucket_path, "--acl public-read"))

# Optionally, print a message when done
cat("CSS file has been successfully uploaded to AWS S3.\n")

distribution_id <- "E2FZ8APNDVB95X"

# Invalidate everything using the wildcard '/*' (no additional quotes around paths)
invalidate_paths <- "/*"

# Run the AWS CLI command to create the invalidation
system(paste(
  "aws cloudfront create-invalidation --distribution-id", 
  distribution_id, 
  "--paths", 
  invalidate_paths
))
