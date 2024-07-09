
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
  app01_vote_patterns <- dbGetQuery(con, "SELECT * FROM app_shiny.app01_vote_patterns")
  app02_leg_activity <- dbGetQuery(con, "SELECT * FROM app_shiny.app02_leg_activity")
  jct_bill_categories <- dbGetQuery(con, "SELECT * FROM proc.jct_bill_categories")
  app03_district_context <- dbGetQuery(con, "SELECT * FROM app_shiny.app03_district_context")
  app03_district_context_state <- dbGetQuery(con, "SELECT * FROM app_shiny.app03_district_context_state")
  
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
  app01_vote_patterns$partisan_vote_plot <- ifelse(app01_vote_patterns$partisan_vote_type == 99,2,app01_vote_patterns$partisan_vote_type)
