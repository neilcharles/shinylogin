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

  app_body <- tagList(
    # App body code that would normally be placed in the UI goes here ----------
    verbatimTextOutput('something_in_the_app_body')
  )

  # Define menu items in the app sidebar here.
  # They will be placed below the project selector and above the admin menu
  app_sidebar <- tagList(
         sidebarMenu(
     id = 'main_menu',
     menuItem(
       text = 'Menu Item 1',
       tabName = 'menu1'
     ))


  )

  # Server code goes here ------------------------------------------------------
  output$something_in_the_app_body <- renderText(glue::glue('Inserting {user$email} into the app'))

}


shinyApp(ui = ui, server = server)
"
writeLines(app.R, con = file.path(path, "app.R"))

writeLines("#----------- Write UI code here ---------------", con = file.path(path, "shinylogin_ui.R"))

writeLines("#----------- Write Server code here -----------", con = file.path(path, "shinylogin_server.R"))

}
