library(shiny)
library(shinydashboard)
library(shinythemes)

library(bibliomatrix)
library(dplyr)

# deploy to shiny in root context
#ln -s /usr/local/lib/R/site-library/bibliomatrix/shiny-apps/abm/* .

#data("abm_public_kth")

# to use this, first start the plumber API 
# issue "make up" in the directory where the plumber.R API resides
# (cd inst/plumber/abm; make up)

ABM_API_PATTERN <- "http://localhost:8080/unit/%s/flexdashboard"

server <- function(input, output, session) {
    
    output$units <- renderUI({
        
        orgs <- abm_public_kth$meta$Diva_org_id %>% 
            set_names(abm_public_kth$meta$unit_long_en_indent2)
        
        shiny::selectInput(inputId = "unitid", label = NULL, 
            choices = orgs, selected = "177", size = 1,
            multiple = FALSE, selectize = FALSE, width = "100%")
    })
    
    # using the API, which could be anywhere or external

    observe({
        dash_src <<- sprintf(ABM_API_PATTERN, input$unitid)
    })
    
    output$frame <- renderUI({
        
        req(input$unitid)
        
        tags$iframe(src = dash_src, width = "100%", height = "100%",
                    frameborder = 0, scrolling = 'auto')
        
    })    
    
    # this could be used to render from within the package
    
    output$dash <- renderUI({
        
        req(input$unitid)
        
        uc <- 
            abm_public_kth$meta %>% 
            filter(Diva_org_id == as.integer(input$unitid)) %>% 
            pull(unit_code)
        
       report <- system.file("extdata/abm.Rmd", package = "bibliomatrix")
       f <- rmarkdown::render(report, params = list(unit_code = uc))
       includeHTML(f)
        #readBin(f, "raw", n = file.info(f)$size)
    })
    
    # TODO: think about caching content... to www? and use...
    # tags$iframe(src = './myMarkdown.html', # put myMarkdown.html to /www
    #             width = '100%', height = '800px', 
    #             frameborder = 0, scrolling = 'auto'
    # )
    
}

