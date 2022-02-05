#UI elements for the login panel
loginPanelUI <- function(id) {
  ns <- shiny::NS(id)

}

loginPanelServer <- function(id){
  shiny::moduleServer(id,
      function(input, output, session) {
        ns <- session$ns



      }
  )
}
