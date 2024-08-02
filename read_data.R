
########################################
#                                      #  
# define functions                     #
#                                      #
########################################
library(RPostgres)
attempt_connection <- function() {
  # Prompt for password
  password_db <- readline(
    prompt="Make sure ye've fired up the Postgres server and hooked up to the database.
    Now, what be the secret code to yer treasure chest o' data?: ")
  
  # Attempt to connect to Postgres database
  con <- tryCatch(
    dbConnect(RPostgres::Postgres(), 
              dbname = "fl_leg_votes", 
              host = "localhost", 
              port = 5432, 
              user = "postgres", 
              password = password_db),
    error = function(e) {
      message("Connection failed: ", e$message)
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
  