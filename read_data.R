
########################################
#                                      #  
# define functions                     #
#                                      #
########################################
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
  app_vote_patterns <- dbGetQuery(con, "SELECT * FROM app_shiny.app_vote_patterns")
  app_data <- dbGetQuery(con, "SELECT * FROM app_shiny.app_data")
  jct_bill_categories <- dbGetQuery(con, "SELECT * FROM proc.jct_bill_categories")
  
  dbDisconnect(con)
  print("Data read to memory, disconnecting from database.")
  
