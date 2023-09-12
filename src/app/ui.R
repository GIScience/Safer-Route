ui <- fluidPage(
  theme = shinytheme("paper"),
  navbarPage(
    "Safer-Route",
    # Title
    tabPanel(# this is page one in the nav
      "Start", # Heading of the page
      sidebarLayout(sidebarPanel(
        h3("Introduction",
           style = "padding-bottom: 20px")
      ),
      mainPanel(h4(
        " Some text"
      )))),
    tabPanel(# this is page two in the nav
      "Washington D.C",
      sidebarLayout(
        sidebarPanel(
          h3("Text and controls for DC",
             style = "padding-bottom: 20px"),
          tags$p(
            "Begin routing by clicking where you are on the map and then clicking again for your destination. After your route is generated, you can press the 'Rate Safety of Route' button to rate your route on a scale of 1-10. Keep clicking to generate a new route!"
          ),
          actionButton("show_modal_btn", "Rate Safety of Route"),
          checkboxInput("use_st_blend", "Route through open spaces", FALSE),
        ),
        mainPanel(
          # dc map
          tags$style(type = "text/css", "#mymap {height: calc(100vh - 180px) !important;}"),
          leafletOutput("mymap"),
          verbatimTextOutput("debug")
        )
      )),
    tabPanel(# this is page two in the nav
      "Munich",
      sidebarLayout(sidebarPanel(
        h3("Text and controls for Munich",
           style = "padding-bottom: 20px")
      ),
      mainPanel(h4(
        "Munich map"
      )))),
    tabPanel(# this is page two in the nav
      "Method",
      sidebarLayout(sidebarPanel(
        h3("Our method",
           style = "padding-bottom: 20px")
      ),
      mainPanel(h4(
        " Some text"
      )))),
    tabPanel(# this is page three in the nav
      "Team",
      sidebarLayout(sidebarPanel(
        h3("Who are we",
           style = "padding-bottom: 20px")
      ),
      mainPanel(h4(
        " Some text"
      ))))
  )
)