library(RPostgres)

##############################
#                            #  
# get environment variables  #
#                            #
##############################
# Define the environment: "staging" or "production"
setting_env <- "staging"

print(paste("ETL pipeline is switched to the", setting_env, "environment."))

# Start the appropriate Docker container
if (setting_env == "staging") {
  system("docker start jaxtrib_staging")
  db_name <- "fl_leg_staging"
  db_port <- 5433
  db_container <- "jaxtrib_staging"
} else if (setting_env == "production") {
  system("docker start jaxtrib_postgres")
  db_name <- "fl_leg_votes"
  db_port <- 5432
  db_container <- "jaxtrib_postgres"
}

##############################
#                            #  
# start Docker engine and db #
#                            #
##############################
# start up Docker service. This will work on Mac, on Windows only if you run RStudio as administrator. Otherwise it'll state a warning.
tryCatch({
  system("net start com.docker.service", intern = TRUE)
}, warning = function(w) {
  message("Windows users can ignore this warning if you've already started Docker Desktop prior to running this script.")
  message("Original warning: ", conditionMessage(w))
}, error = function(e) {
  stop(e)  # Re-throw the error if it's not a warning
})

system("docker ps")

# start the Docker container
print(paste("Starting", db_container, "container."))
bash_command <- paste("docker exec -i", db_container, "bash -c")
psql_command <- paste("\"psql -U postgres -d", db_name, "\"")

#start the postgres database
system(paste(bash_command, psql_command))
print(paste("Connected to the", db_name, "database on port", db_port))

########################################
#                                      #  
# define database write functions      #
#                                      #
########################################

# Extract the db password from local config
config <- config::get()
password_db <- config::get("postgres_pwd")

attempt_connection <- function() {
  con <- tryCatch(
    dbConnect(
      RPostgres::Postgres(),
      dbname = db_name,
      host = "localhost",
      port = as.integer(db_port),
      user = "postgres",
      password = password_db
    ),
    error = function(e) {
      message("Connection failed: ", e$message, " Make sure ye've fired up the Postgres server and hooked up to the database.")
      return(NULL)
    }
  )
  return(con)
}

########################################
#                                      #  
# connect to Postgres and read data    #
#                                      #
########################################

  # Loop until successful connection
  repeat {
    con <- attempt_connection()
    
    if (!is.null(con) && dbIsValid(con)) {
      print("Successfully connected to the database!")
      break
    } else {
      message("Failed to connect to the database. Please try again.")
    }
  }
  
  # pull in Postgres data
  app01_vote_patterns <- dbGetQuery(con, "SELECT * FROM app_shiny.app01_vote_patterns")
  app02_leg_activity <- dbGetQuery(con, "SELECT * FROM app_shiny.app02_leg_activity")
  jct_bill_categories <- dbGetQuery(con, "SELECT * FROM proc.jct_bill_categories")
  app03_district_context <- dbGetQuery(con, "SELECT * FROM app_shiny.app03_district_context")
  app04_district_context <- dbGetQuery(con, "SELECT * FROM app_shiny.app03_district_context")
  app03_district_context_state <- dbGetQuery(con, "SELECT * FROM app_shiny.app03_district_context_state")
  
  print("Data read to memory, disconnecting from database.")
  dbDisconnect(con)
  