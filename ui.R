ui <- fluidPage(
  theme = shinytheme("cosmo"),
  tags$style(HTML("
    .navbar {
      background-color: white !important;
      border-color: #ddd !important;
    }
    .navbar-default .navbar-nav > li > a {
      color: #666 !important;
    }
    .navbar-default .navbar-nav > .active > a,
    .navbar-default .navbar-nav > .active > a:focus,
    .navbar-default .navbar-nav > .active > a:hover {
      background-color: #f8f8f8 !important;
      color: #333 !important;
    }
    .navbar-default .navbar-nav > li > a:hover {
      background-color: #e6f3ff !important;
    }
  ")),
  uiOutput("main_container")
)
