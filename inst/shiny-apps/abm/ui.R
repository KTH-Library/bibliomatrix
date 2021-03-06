library(shiny)
library(shinydashboard)
library(shinythemes)

# ----------------------
# Layout: sidebar with filter
# TODO: workound various issue with getting the sizing to work properly

dashboardPage(skin = "black",
  dashboardHeader(title = paste("ABM KTH", bibliomatrix::abm_config()$stop_year + 1),
    # tags$li(class = "dropdown",
    #   #img(src = "kth-logo.png", height = 30, width = 30),
    #   #style = "padding-top:10px; padding-bottom:10px;",
    #   #class = "dropdown",
    #   tags$li(class = "dropdown",
    #     tags$a(href = 'https://kth.se/abm',
    #       "Login", icon("sign-in"),
    #       title = "Log in here if you are a student or employed at KTH" #,
    #   ))
    # )
    uiOutput("login")#, inline = TRUE, container = tags$span)
  ),
  dashboardSidebar(
    #tags$h4(textOutput("login")),
    uiOutput("units"),
    uiOutput("switcher")
#   checkboxInput("use_prerendered", "Use pre-rendered content", value = TRUE)
  ),
  dashboardBody(
    # this inline CSS is intented to retain indentations on iPhone (which seems )
    tags$head(tags$style(HTML('
      .selectize-dropdown {
        width: auto !important;
      }
      .selectize-dropdown [data-selectable], .selectize-dropdown .optgroup-header {
        white-space: nowrap;
      }
      #unitid option {
        text-align: left !important;
        align-items: left !important;
      }
    '))),
    box(width = 12, height = "90vh", 
      tags$style(type = "text/css", "#frame {height: calc(100vh - 120px) !important;}"),
      htmlOutput("frame")
    )
  )
)

# Alternative layouts below (if we want to get away from the double from "double framing")

# ----------------------
# Layout: filling the page with the dropdown filter on top

# fillPage(
#   tags$style(type = "text/css",
#     ".half-fill { width: 50%; height: 100%; }",
#     ".left-fill { width: 20%; height: 100%; }",
#     ".right-fill { width: 80%; height: 100%; }",
#     "#one { float: left; }",
#     "#two { float: right; }"
#   ),
#   div(id = "one", class = "left-fill",
#       uiOutput("units")
#   ),
#   div(id = "two", class = "right-fill",
#       htmlOutput("frame", inline = TRUE)
#   ),
#   padding = 0
# )

# ----------------------
#Layout: filling the page with the dropdown filter on top

# fillPage(
#   tags$style(type = "text/css",
#     ".top-fill { width: 100%; }",
#     ".bottom-fill { width: 100%; height: 100%; }",
#     "#one { float: top; }",
#     "#two { float: bottom; }"
#   ),
#   div(id = "one", class = "top-fill",
#       uiOutput("units")
#   ),
#   div(id = "two", class = "bottom-fill",
#       htmlOutput("frame", inline = TRUE)
#   ),
#   padding = 0
# )
