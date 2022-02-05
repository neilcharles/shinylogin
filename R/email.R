send_password_reset <- function(send_to_email = NULL, new_password = NULL){

  message_body <- as.character(glue::glue("Your new password is: {new_password}"))
  host <- Sys.getenv("shinylogin_smtp_host")
  port <- as.numeric(Sys.getenv("shinylogin_smtp_port"))
  user <- Sys.getenv("shinylogin_smtp_user")
  password <- Sys.getenv("shinylogin_smtp_password")

  mailR::send.mail(from = "neil@hilltop-analytics.com",
            to = send_to_email,
            subject = "New Password",
            body = message_body,
            smtp = list(host.name = host,
                        port = port,
                        user.name = user,
                        passwd = password,
                        ssl = FALSE),
            authenticate = TRUE,
            send = TRUE)

}
