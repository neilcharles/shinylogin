#' UI For Shiny Login
#'
#' @param id ID to read the value from
#'
#' @return
#' @export
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   ui <- loginAppUI('app')
#'
#'   server <- function(input, output, session) {
#'   output$neil <- loginAppServer('app')
#'  }
#'
#'  shinyApp(ui = ui, server = server)
#' }
loginAppUI <- function(id) {
  ns <- shiny::NS(id)
  list(
    shiny::HTML(
      #Ensures google login script is properly loaded or Google account login doesn't work
      "<script src=\"https://apis.google.com/js/platform.js?onload=init\"></script>"
    ),
    shiny::uiOutput(ns("loginapp"))
  )
}

#' Server For Shiny Login
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
#' #' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   ui <- loginAppUI('app')
#'
#'   server <- function(input, output, session) {
#'   output$neil <- loginAppServer('app')
#'  }
#'
#'  shinyApp(ui = ui, server = server)
#' }
loginAppServer <- function(id, app_body = NULL, app_sidebar = NULL, config = NULL) {
  shiny::moduleServer(id,
                      function(input, output, session) {
                        ns <- session$ns

                        isValidEmail <- function(x) {
                          grepl(
                            "\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>",
                            as.character(x),
                            ignore.case = TRUE
                          )
                        }


                        output$loginapp <- renderUI({
                          if (user_is_logged_in()) {
                            bs4DashPage(
                              dark = NULL,
                              header = bs4DashNavbar(
                                title = "Login Skeleton",
                                rightUi = bs4Dash::userOutput(ns('login_rightui'))
                              ),
                              sidebar = bs4DashSidebar(if(Sys.getenv("shinylogin_use_projects")){
                                                         uiOutput(ns("project_picker"))
                                                       },
                                                       app_sidebar,
                                                       hr(),
                                                       sidebarMenu(
                                                         id = "manage_users_sidebar",
                                                         menuItem(
                                                           text = "Account Management",
                                                           tabName = "manage_accounts"
                                                         ))),
                              # controlbar = bs4DashControlbar(disable = TRUE),
                              body = bs4DashBody(
                                shiny::HTML(
                                  "<script src=\"https://apis.google.com/js/platform.js?onload=init\"></script>"
                                ),

                                bs4TabItems(
                                  bs4TabItem(
                                    tabName = "manage_accounts",

                                    accountManagementUI(ns('account_management'))
                                  )),

                                app_body
                                # tableOutput('usertable')
                              )
                            )
                          } else {
                            bs4DashPage(
                              header = bs4DashNavbar(disable = TRUE),
                              sidebar = bs4DashSidebar(disable = TRUE),
                              bs4DashBody(
                                #Hide leftover bits of bs4Dash in the task bar
                                tags$style(
                                  HTML(
                                    'body:not(.sidebar-mini-md) .content-wrapper, body:not(.sidebar-mini-md) .main-footer, body:not(.sidebar-mini-md) .main-header {
               margin-left: 0;
            }
            #controlbar-toggle, .fa-bars{
              visibility: hidden;
            }'
                                  )
                                ),
                                bs4TabCard(
                                  collapsible = FALSE,
                                  tabPanel(
                                    title = "Login",
                                    closable = FALSE,
                                    collapsible = FALSE,
                                    h2("Login Skeleton"),
                                    hr(),
                                    p("Log in with username and password"),
                                    textInput(ns("login_email"), NULL, placeholder = "Email"),
                                    passwordInput(ns("login_password"), NULL, placeholder = "Password"),
                                    actionButton(ns("login_btn"), "Log In"),
                                    hr(),
                                    p("Log in with a Google account"),
                                    googleSignInUI(ns("google_signin"))
                                  ),
                                  tabPanel(
                                    title = "Register",
                                    p("Create a new user account"),
                                    textInput(ns("register_email"), NULL, placeholder = "Email"),
                                    textInput(ns("register_username"), NULL, placeholder = "Username"),
                                    passwordInput(ns("register_password"), NULL, placeholder = "Password"),
                                    passwordInput(ns("register_password_confirm"), NULL, placeholder = "Confirm Password"),
                                    uiOutput(ns("register_btn"))
                                  ),
                                  tabPanel(title = "Forgot Password",
                                           p("If your email address is recognised, you will be sent a new password"),
                                           textInput(ns("forgot_password_email"), NULL, placeholder = "Email"),
                                           actionButton(ns("forgot_pw_btn"), "Send Me a New Password")
)
                                )
                              )
                            )
                          }
                        })

                        sign_ins <-
                          sign_ins <- callModule(googleSignIn, "google_signin")

# ----------------- Reactive values storing login state ----------------------------------
                        #Google login
                        g_name <- shiny::reactive(sign_ins()$name)
                        g_email = shiny::reactive(sign_ins()$email)
                        g_image = shiny::reactive(sign_ins()$image)

                        #Username/password login
                        upw_login <- shiny::reactiveValues()
                        upw_login$logged_in <- FALSE

                        #Logged in user vars for passing user status to functions
                        app_user <- shiny::reactiveValues()

                        observe({

                          if (upw_login$logged_in) {
                            app_user$username <- upw_login$username
                            app_user$email <- upw_login$email
                            app_user$image <-
                              'https://static.thenounproject.com/png/802590-200.png'

                          } else if (!class(try(g_name(), silent = TRUE) #check for Google login
                          ) == "try-error") {

                            #Add user if they don't exist
                            if(!user_exists(g_email())){
                              user_add(g_email(), g_name(), "google_auth")
                            }

                            app_user$username <- g_name()
                            app_user$email <- g_email()
                            app_user$image <- g_image()
                          }

                          if(!is.null(app_user$email)){
                            app_user$admin <- ifelse(user_get(app_user$email)$admin==1, "Admin", "User")
                          }
                        })

# ---------------------- Login -----------------------------------------------------------
                        shiny::observeEvent(input$login_btn, {
                          hashedpw <- hash_password(input$login_password)

                          if (user_verify(input$login_email, hashedpw)) {
                            upw_login$logged_in <- TRUE
                            upw_login$email <- input$login_email
                            upw_login$username <- user_get(input$login_email)$username
                          } else {
                            shiny::showModal(
                              shiny::modalDialog(
                                title = "Access Denied",
                                "Unknown user or incorrect password"
                              )
                            )
                          }
                        })

# ---------------------- Register -----------------------------------------------------------
                        observeEvent(input$register_btn, {

                          if(user_exists(email_new = input$register_email)){

                            shiny::showModal(shiny::modalDialog(
                              title = "Could Not Create User",
                              "That email address is already registered",
                              easyClose = TRUE
                            ))

                            return(NULL)

                          }

                          if(user_exists(username_new = input$register_username)){

                            shiny::showModal(shiny::modalDialog(
                              title = "Could Not Create User",
                              "That username is already registered",
                              easyClose = TRUE
                            ))

                            return(NULL)

                          }

                          #Create the user and default project
                          user_add(user_email = input$register_email,
                                   username = input$register_username,
                                   password = input$register_password)

                          #Log the new user in
                          upw_login$logged_in <- TRUE

                          upw_login$username <- input$register_username
                          upw_login$email <- input$register_email

                          shiny::showModal(shiny::modalDialog(
                            title = "Thanks for Registering",
                            "Welcome to the site",
                            easyClose = TRUE
                          ))

                        })

                        output$register_btn <- renderUI({
                          validate(need(
                            isValidEmail(input$register_email),
                            "Enter a valid email address"
                          ))
                          validate(need(nchar(input$register_username) >
                                          0, "Enter a username"))
                          validate(need(nchar(input$register_password) >
                                          0, "Enter a password"))
                          validate(
                            need(
                              input$register_password == input$register_password_confirm,
                              "Passwords must match"
                            )
                          )

                          actionButton(ns('register_btn'), 'Register')
                        })

                        user_is_logged_in <- shiny::reactive({
                          !class(try(g_name(), silent = TRUE)
                          ) == "try-error" |
                            upw_login$logged_in == TRUE
                        })

# ---------------------- Forgot Password -----------------------------------------------------------

                        observeEvent(input$forgot_pw_btn, {

                          send_password_reset(input$forgot_password_email,
                                              user_generate_new_password(input$forgot_password_email))

                          shiny::showModal(
                            shiny::modalDialog(
                              title = "Message Sent",
                              "If your email was recognised you have been sent a new password.
                              Please check your spam folder if you have not received it."
                            )
                          )

                        })




# ---------------------- User logged in UI -----------------------------------------------------------
                        output$login_rightui <-
                          renderUser({

                            if(user_is_logged_in()){
                              dashboardUser(
                                name = app_user$username,
                                image = app_user$image,
                                title = app_user$email,
                                subtitle = app_user$admin,
                                footer = logout_button()
                              )
                            }
                          })

                        output$project_picker <- renderUI({
                          user_projects <- user_get_projects(g_email())

                          select_options <- as.list(user_projects$projectId)

                          names(select_options) <- user_projects$projectName

                          selectInput('selectProject', NULL, select_options)
                        })

# ---------------------- Logout -----------------------------------------------------------
                        logout_button <- reactive({
                          if (upw_login$logged_in) {
                            actionButton(ns('logout_pw'), 'Sign Out', style = "color: #FFFFFF; background: #C82333")
                          } else if (!class(try(g_name(), silent = TRUE)
                          ) == "try-error") {
                            googleSignInUI(ns("google_signin"))
                          }
                        })

                        observeEvent(input$logout_pw, {
                          upw_login$logged_in <- FALSE
                        })

                        accountManagementServer('account_management', app_user)

                        return(app_user)

                      })
}
