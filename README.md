
# shinylogin

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

shinylogin wraps bs4Dash or shinydashboard to provide user login and registration functionality as a front screen before access is permitted to an app. It also creates a user management panel within the app.

shinylogin is experimental and I'm not a security specialist. It will ensure that casual users log in before they can access your app and implements basic security good practices such as encrypting stored passwords & preventing SQL injection, but it's unlikely to survive an attack from a hacker who knows what they're doing.

WORK IN PROGRESS. YOU CAN'T RUN THIS YET.

## Installation

``` r
remotes::install_github("neilcharles/shinylogin")
```
