
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
  
  print("Data read to memory, disconnecting from database.")
  dbDisconnect(con)
  
########################################
#                                      #  
# late data prep (should move to ETL)  #
#                                      #
########################################
  
  #code as factor if I use scale_fill_manual in ggplot. this slowed down plotting substantially
  #app_vote_patterns$partisan_vote_type <- factor(app_vote_patterns$partisan_vote_type, levels = c(0, 1, 99, NA))
  
  #pre-handle n/a vote to see if that speeds up plotting (it didn't)
  #app_vote_patterns$partisan_vote_type <- ifelse(is.na(app_vote_patterns$partisan_vote_type),999,app_vote_patterns$partisan_vote_type)
  
  #re-assign 99 to facilitate faster gradient-based plot
  app_vote_patterns$partisan_vote_plot <- ifelse(app_vote_patterns$partisan_vote_type == 99,2,app_vote_patterns$partisan_vote_type)
  
  
  
