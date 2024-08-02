library(shiny)
library(httr)
library(jsonlite)


##############################
#                            #  
# API KEYS                   #
#                            #
##############################
config <- config::get()

# Extract the API key
api_key_geocodio <- config::get("api_key_geocodio")
api_key_plural <- config::get("api_key_plural")


##############################
#                            #  
# HELPER FUNCTIONS           #
#                            #
##############################
# convert address to lat and lng (forward geocode)
get_lat_long <- function(address) {
  url <- paste0("https://api.geocod.io/v1.7/geocode?q=", URLencode(address), "&api_key=", api_key_geocodio)
  response <- GET(url)
  
  if (status_code(response) == 200) {
    content_response <- content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
    
    if (!is.null(content_response$results) && length(content_response$results) > 0) {
      lat <- content_response$results[[1]]$location$lat
      lng <- content_response$results[[1]]$location$lng
      list(lat = lat, long = lng)
    } else {
      print("No results found.")
      list(lat = NA, long = NA)
    }
  } else {
    print(paste("Error: Status code", status_code(response)))
    list(lat = NA, long = NA)
  }
}



# look up representatives based on lat/lng
get_representatives <- function(lat, long) {
  url <- paste0("https://v3.openstates.org/people.geo?apikey=", api_key_plural, "&lat=", lat, "&lng=", long)
  response <- GET(url)
  
  if (status_code(response) == 200) {
    content_response <- content(response, as = "text", encoding = "UTF-8")
    data <- tryCatch({
      fromJSON(content_response, flatten = TRUE)
    }, error = function(e) {
      print("Error parsing JSON response")
      NULL
    })
    if (!is.null(data) && length(data) > 0) {
      data
    } else {
      print("No representatives found.")
      NULL
    }
  } else {
    print(paste("Error: Status code", status_code(response)))
    content_response <- content(response, as = "text", encoding = "UTF-8")
    print(content_response)
    NULL
  }
}
  

##############################
#                            #  
# OUTPUT                     #
#                            #
##############################
observeEvent(input$submit, {
  address <- input$address
  if (address != "") {
    coordinates <- get_lat_long(address)
    print(coordinates)
    
    if (!is.na(coordinates$lat) && !is.na(coordinates$long)) {
      representatives <- get_representatives(coordinates$lat, coordinates$long)
      output$representatives <- renderTable({
        print(representatives$results)
        if (!is.null(representatives$results)) {
          df <- data.frame(
            #Image = representatives$results$image,
            Jurisdiction = representatives$results$`jurisdiction.name`,
            Title = representatives$results$`current_role.title`,
            District = representatives$results$`current_role.district`,
            Name = representatives$results$name,
            Party = representatives$results$party
            
          )
          df #returns the data frame to Shiny
        } else {
          data.frame(Message = "No representatives found.")
        }
      })
    } else {
      output$representatives <- renderTable({
        data.frame(Message = "Invalid address. Please include city and state.")
      })
    }
  } else {
    output$representatives <- renderTable({
      data.frame(Message = "Please enter an address.")
    })
  }
})