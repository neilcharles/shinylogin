
# shinylogin

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

shinylogin wraps shiny and bs4Dash to provide user login and registration functionality as a front screen before access is permitted to an R Shiny app. It also creates a user management panel within the app.

WARNING: shinylogin is experimental and I'm not a security specialist. It will ensure that casual users log in before they can access your app and implements basic security good practices such as encrypting stored passwords & preventing SQL injection, but it's unlikely to survive an attack from a hacker who knows what they're doing.

shinylogin is close to being useable but still has big gaps and will almost certainly see more breaking changes.

## Installation

``` r
remotes::install_github("neilcharles/shinylogin")
```

## Features

- App login with username and password, plus the option to include authentication using a Google account

- Optional in-app user registration or restriction to pre-registered users only

- Forgotten password recovery via email

- Passwords stored as hashes in the user database using the sodium package

- SQL injection prevention

- Option to set privilege levels for individual users

- Easy setup via an RStudio project template extension

- Attractive bs4Dash user bar and logout function once logged in

- Easy retrieval of logged in user details for use within your app

- Option to specify a database connection to store user details, or let shinylogin default to local storage

---

![Login screen example](man/figures/login_example.png)

---
