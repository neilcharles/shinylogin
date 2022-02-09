accountManagementUI <- function(id) {
  ns <- NS(id)
  list(

    bs4TabCard(
      collapsible = FALSE,
      closable = FALSE,
      tabPanel(
        title = "My Account",
        h2("Manage My Account"),
        hr(),
        passwordInput(ns("change_password_old"), NULL, placeholder = "Old Password"),
        passwordInput(ns("change_password_new"), NULL, placeholder = "New Password"),
        passwordInput(ns("change_password_confirm"), NULL, placeholder = "Confirm New Password"),
        actionButton(ns("change_password"), "Change Password"),
        hr(),
        actionButton(ns("delete_account"), "Delete My Account", status = "danger")
      )
    )

  )
}

accountManagementServer <- function(id, app_user) {
  shiny::moduleServer(id,
                      function(input, output, session) {
                        ns <- session$ns

#-------------------------- Change password click -------------------------------------
                        shiny::observeEvent(input$change_password, {

                          #------------------ Change Password -------------------------------------
                          if(hash_password(input$change_password_old)==user_get(app_user$email)$password){
                            if(input$change_password_new == input$change_password_confirm){

                              user_change_password(app_user$email, input$change_password_new)

                              shiny::showModal(
                                shiny::modalDialog(
                                  title = "Password changed",
                                  "Your password has been updated"
                                ))


                            } else {
                              shiny::modalDialog(
                                title = "Password not changed",
                                "New password and confirmation do not match"
                              )
                            }

                          } else {
                            shiny::showModal(
                              shiny::modalDialog(
                                title = "Password not changed",
                                "Old password does not match your account password"
                              )
                            )
                          }
                        })

                        #------------------------- Delete Account --------------------------------

                        shiny::observeEvent(input$delete_account, {
                          shiny::showModal(shiny::modalDialog(
                            title = "Delete Account: Are you sure?",
                            "This action cannot be undone",
                            hr(),
                            actionButton(ns("delete_account_confirm"), "I'm sure, delete my account", status = "danger")
                          ))
                        })

                        shiny::observeEvent(input$delete_account_confirm, {
                          user_delete(app_user$email)
                          app_user <- NULL
                          session$reload()
                        })

#-------------------------- Admin Panel -------------------------------------





                      })
}
