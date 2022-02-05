shinylogin_new_project <- function(path, ...){

  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  app.R <-
    "
library(shiny)
library(bs4Dash)
library(tidyverse)
library(googleAuthR)
library(shinylogin)

logindb_con <- function() DBI::dbConnect(RSQLite::SQLite(), 'shinylogin')

options(googleAuthR.webapp.client_id = '544952614531-cp3154luqkgfde6po9opl5k60hl16r5o.apps.googleusercontent.com')
options(shiny.port = 4093)

# A one line UI because login requires everything to happen server-side --------
ui <- loginAppUI('login_framework')

server <- function(input, output, session) {

  # loginAppServer combines the login wrapper with your shiny app --------------
  user <- loginAppServer('login_framework', app_body = app_body, app_sidebar = app_sidebar, config = shinylogin_config)

  # App body code that would normally be placed in the UI goes here ----------
  source('shinylogin_ui.R', local = TRUE)

  # Define menu items in the app sidebar here.
  # They will be placed below the project selector and above the admin menu
  source('shinylogin_sidebar.R', local = TRUE)

  # Server code goes here ------------------------------------------------------
  source('shinylogin_server.R', local = TRUE)

}


shinyApp(ui = ui, server = server)
"
writeLines(app.R, con = file.path(path, "app.R"))

#-------------------------------------------------------------------------------

shinylogin_ui.R <-

  "
# Define UI for application that draws a histogram
# Sidebar with a slider input for number of bins
app_body <- tagList(
  verbatimTextOutput('something_in_the_app_body'),
  sliderInput('bins',
              'Number of bins:',
              min = 1,
              max = 50,
              value = 30),
  # Show a plot of the generated distribution
  plotOutput('distPlot')
)"

writeLines(shinylogin_ui.R, con = file.path(path, "shinylogin_ui.R"))

#-------------------------------------------------------------------------------

shinylogin_server.R <-
  "
output$something_in_the_app_body <- renderText(glue::glue('Hello {user$email}'))

output$distPlot <- renderPlot({
  # generate bins based on input$bins from ui.R
  x    <- faithful[, 2]
  bins <- seq(min(x), max(x), length.out = input$bins + 1)

  # draw the histogram with the specified number of bins
  hist(x, breaks = bins, col = 'darkgray', border = 'white')
})
"

writeLines(shinylogin_server.R, con = file.path(path, "shinylogin_server.R"))

#-------------------------------------------------------------------------------

shinylogin_sidebar.R <-
  "
app_sidebar <- tagList(
  sidebarMenu(
    id = 'main_menu',
    menuItem(
      text = 'Menu Item 1',
      tabName = 'menu1'
    ))
)
"

writeLines(shinylogin_sidebar.R, con = file.path(path, "shinylogin_sidebar.R"))

}
