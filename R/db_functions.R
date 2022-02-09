logindb_connect <- function(){
  if(exists('logindb_con', mode = 'function')){
    return(logindb_con())
  } else {
    DBI::dbConnect(RSQLite::SQLite(), "shinylogin")
  }
}

app_list_users <- function(){

  con <- logindb_connect()

  users <- dplyr::tbl(con, "users") %>%
    pull(email)

  DBI::dbDisconnect(con)

  users

}


user_verify <- function(user_email, user_password){

  con <- logindb_connect()

  user_email <- tolower(user_email)

  user <- dplyr::tbl(con, "users") %>%
    dplyr::filter(email == user_email,
           password == user_password) %>%
    collect()

  DBI::dbDisconnect(con)

  if (nrow(user) == 1) TRUE else FALSE

}

user_get <- function(user_email){

  con <- logindb_connect()

  user <- dplyr::tbl(con, "users") %>%
    filter(email == user_email) %>%
    collect()

  DBI::dbDisconnect(con)

  user

}

user_change_password <- function(user_email = NULL, plaintext_password = NULL){

  con <- logindb_connect()

  user_email <- tolower(user_email)

  if(!user_exists(user_email)){
    DBI::dbDisconnect(con)
    stop(glue::glue("User does not exist"))
  }

  sql_skeleton <-
    "UPDATE users SET password = ?hashed_pw WHERE email = ?email"

  sql_query <-
    DBI::sqlInterpolate(
      con,
      sql_skeleton,
      email = user_email,
      hashed_pw = hash_password(plaintext_password)
    )

  DBI::dbExecute(con, sql_query)
  DBI::dbDisconnect(con)

  TRUE

}

user_generate_new_password <- function(user_email = NULL){

  new_pw <- stringi::stri_rand_strings(n=1, length=8, pattern="[A-Za-z0-9]")

  user_change_password(user_email = user_email, plaintext_password = new_pw)

  new_pw

}


#' Check Whether A User Exists in the Login Database
#'
#' @param con A database connection
#' @param email User email
#' @param username Username
#'
#' @return TRUE if the checked parameter exists, otherwise FALSE
#' @export
#'
#' @examples
user_exists <- function(email_new = "", username_new = ""){

  con <- logindb_connect()

  email_new <- tolower(email_new)

  if(email_new == "" & username_new == ""){
    stop('Check either email or username, not both')
  }

  check_email <- tbl(con, "users") %>%
    filter(email == email_new) %>%
    collect() %>%
    nrow()

  if(check_email > 0){
    DBI::dbDisconnect(con)
    return(TRUE)
  }

  check_username <- tbl(con, "users") %>%
    filter(username == username_new) %>%
    collect() %>%
    nrow()

  if(check_username > 0){
    DBI::dbDisconnect(con)
    return(TRUE)
  }

  DBI::dbDisconnect(con)
  FALSE
}

#' Add a user to the application
#'
#' @param con User database connection
#' @param email email address
#' @param username username
#' @param password Hashed password
#'
#' @return TRUE
#' @export
#'
#' @examples
#' con <- dbConnect(RSQLite::SQLite(), ":memory:")
#'
#' login_db_create(con)
#'
#' user_add(
#'   email = "user(at)example.com",
#'   username = "another_user",
#'   password = "ahashedpassword"
#' )
#'
user_add <- function(user_email = NULL, username = NULL, password = NULL){

  con <- logindb_connect()

  user_email <- tolower(user_email)

  if(user_exists(email_new = user_email)){
    DBI::dbDisconnect(con)
    stop('email already exists in the login database')
  }

  if(user_exists(username_new = username)){
    DBI::dbDisconnect(con)
    stop('username already exists in the login database')
  }

  sql_skeleton <-
    "INSERT INTO users VALUES(?email , ?username , ?password , 0)"

  sql_query <-
    DBI::sqlInterpolate(
      con,
      sql_skeleton,
      email = user_email,
      username = username,
      password = hash_password(password)
    )

  DBI::dbExecute(con, sql_query)

  DBI::dbDisconnect(con)

  #Create a default project for the user
  project_add(user_email, project_name = "Default Project")

  TRUE

}

user_delete <- function(email = NULL){

  con <- logindb_connect()

  email <- tolower(email)

  sql_skeleton <-
    "DELETE FROM users WHERE email = ?email"

  sql_query <-
    DBI::sqlInterpolate(
      con,
      sql_skeleton,
      email = email
    )

  DBI::dbExecute(con, sql_query)

  DBI::dbDisconnect(con)

  TRUE

}

user_set_admin <- function(email = NULL, admin_value = 0){

  con <- logindb_connect()

  email <- tolower(email)

  sql_skeleton <-
    "UPDATE users SET admin = ?admin_value WHERE email = ?email"

  sql_query <-
    DBI::sqlInterpolate(
      con,
      sql_skeleton,
      email = email,
      admin_value = admin_value
    )

  DBI::dbExecute(con, sql_query)

  DBI::dbDisconnect(con)
}


user_exists_in_project <- function(email = NULL){

  con <- logindb_connect()

  email <- tolower(email)

  check_email <- tbl(con, "projects") %>%
    filter(email == email) %>%
    collect() %>%
    nrow()

  DBI::dbDisconnect(con)

  if(check_email > 0){
    return(TRUE)
  }

  FALSE

}

project_add <- function(user_email = NULL, project_name = "Default Project"){

  con <- logindb_connect()

  #Create project
  project_id <- uuid::UUIDgenerate(use.time = TRUE)

  sql_skeleton <-
    "INSERT INTO projects VALUES(?projectId, ?projectName)"

  sql_query <-
    DBI::sqlInterpolate(
      con,
      sql_skeleton,
      projectId = project_id,
      projectName = project_name
    )

  DBI::dbExecute(con, sql_query)

  #Assign user to project as admin
  sql_skeleton <-
    "INSERT INTO permissions VALUES(?projectId, ?email, 1)"

  sql_query <-
    DBI::sqlInterpolate(
      con,
      sql_skeleton,
      projectId = project_id,
      email = user_email
    )

  DBI::dbExecute(con, sql_query)

  DBI::dbDisconnect(con)

  TRUE

}

user_get_projects <- function(email = NULL){

  con <- logindb_connect()

  email <- tolower(email)

  projects <- tbl(con, "permissions") %>%
    filter(email == email) %>%
    left_join(tbl(con, "projects"), by = "projectId") %>%
    select(projectId, projectName) %>%
    collect()

  DBI::dbDisconnect(con)

  projects

}

project_get_name <- function(projectId){

  con <- logindb_connect()

  name <- tbl(con, "projects") %>%
    filter(projectId %in% projectId) %>%
    pull(projectName) %>%
    collect()

  DBI::dbDisconnect(con)

  name

}

project_add_user <- function(projectId = NULL, email = NULL, level = NULL){

  con <- logindb_connect()

  email <- tolower(email)

  if(user_exists_in_project(email)){
    DBI::dbDisconnect(con)
    stop(glue::glue("User already added to project ID {projectId}"))
  }

  sql_skeleton <-
    "INSERT INTO permissions VALUES(?projectId, ?email, ?privilege)"

  sql_query <-
    DBI::sqlInterpolate(
      con,
      sql_skeleton,
      projectId = projectId,
      email = email,
      privilege = privilege
    )

  DBI::dbExecute(con, sql_query)

  DBI::dbDisconnect(con)

  TRUE

}

project_remove_user <- function(projectId = NULL, user_email = NULL){

  con <- logindb_connect()

  user_email <- tolower(user_email)

  sql_skeleton <-
    "DELETE FROM permissions WHERE email = ?email AND projectId = ?projectId"

  sql_query <-
    DBI::sqlInterpolate(
      con,
      sql_skeleton,
      email = user_email,
      projectId = projectId
    )

  DBI::dbExecute(con, sql_query)

  DBI::dbDisconnect(con)

  TRUE

}

project_set_privilege <- function(project = NULL, user_email = NULL, privilege_value = 0){

  con <- logindb_connect()

  user_email <- tolower(user_email)

  if(!user_exists_in_project(user_email)){
    DBI::dbDisconnect(con)
    stop(glue::glue("User does not exist in project ID {projectId}"))
  }

  sql_skeleton <-
    "UPDATE permissions SET privilege = ?privilege_value WHERE email = ?email"

  sql_query <-
    DBI::sqlInterpolate(
      con,
      sql_skeleton,
      email = user_email,
      admin_value = admin_value
    )

  DBI::dbExecute(con, sql_query)
  DBI::dbDisconnect(con)

  TRUE

}

logindb_delete <- function(silent = FALSE){

  if(!silent){
    confirm_yn <- readline("This will delete all tables in the user database. Type 'delete' to confirm.")

    if(confirm_yn!="delete"){
      stop("User aborted")
    }
  }

  con <- logindb_connect()

  DBI::dbExecute(con, "DROP TABLE IF EXISTS users")

  DBI::dbExecute(con, "DROP TABLE IF EXISTS projects")

  DBI::dbExecute(con, "DROP TABLE IF EXISTS permissions")

  DBI::dbDisconnect(con)

}


#' Create the login user database
#'
#' @param con A database connection
#'
#' @return
#' @export
#'
#' @examples
logindb_create <- function(warn = TRUE){
  if(warn){
    if(readline(prompt = "Are you sure? Existing user database will be deleted. Type 'yes' to continue: ")!='yes'){
      return()
    }
  }

  logindb_delete(silent = TRUE)

  con <- logindb_connect()

  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS users (
                  email varchar(256),
                  username varchar(256),
                  password varchar(256),
                  admin int
                 )"
  )

  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS projects (
                  projectId varchar(256),
                  projectName varchar(256)
                 )"
  )

  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS permissions (
                  projectId varchar(256),
                  email varchar(256),
                  privilege int
                 )"
  )

  DBI::dbDisconnect(con)

}

hash_password <- function(plaintext_password){
  sodium::bin2hex(sodium::hash(charToRaw(plaintext_password)))
}
