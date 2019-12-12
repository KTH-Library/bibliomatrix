library(shiny)
library(shinydashboard)
library(shinythemes)

library(bibliomatrix)
library(dplyr)
library(purrr)

# deploy to shiny in root context
#ln -s /usr/local/lib/R/site-library/bibliomatrix/shiny-apps/abm/* .

#data("abm_public_kth")

# to use this, first start the plumber API 
# issue "make up" in the directory where the plumber.R API resides
# (cd inst/plumber/abm; make up)
ABM_API <- Sys.getenv("ABM_API")
if (ABM_API == "") ABM_API <- "http://localhost:8080"

ABM_API_UNIT <- paste0(ABM_API, "/unit/%s/flexdashboard?embeddata=true")
ABM_API_EMP <- paste0(ABM_API, "/employee/%s/flexdashboard")

server <- function(input, output, session) {

    ua <- Sys.getenv("SHINYPROXY_USERNAME")
    if (ua == "") ua <- "sigbritt"
    
    kthid <- function() {
        if (input$use_prerendered) return (177)
        kthid <- ad_kthid(ua)
        kthid <- setNames(kthid, ad_displayname(kthid))
    }
    
    output$units <- renderUI({
        
        orgs <- abm_public_kth$meta$Diva_org_id %>% 
            set_names(abm_public_kth$meta$unit_long_en_indent2)
        
        if (!input$use_prerendered)
            orgs <- c(kthid(), orgs)
        
        shiny::selectInput(inputId = "unitid", label = NULL, 
            choices = orgs, selected = kthid(), size = 1,
            multiple = FALSE, selectize = FALSE, width = "100%")
    })
    
    # using the API, which could be anywhere or external

    observe({
        req(input$unitid)
        # if the unit id not is for an org then use employee endpoint
        API <- ifelse(!input$unitid %in% abm_public_kth$meta$Diva_org_id, 
            ABM_API_EMP, ABM_API_UNIT)
        dash_src <<- sprintf(API, input$unitid)
        if (!input$use_prerendered)
            cat(dash_src, "\n")
    })
    
    output$frame <- renderUI({
        
        req(input$unitid)
        
        if (input$use_prerendered == TRUE) {
            f <- file.path(prerender_cache_location(), 
                paste0(input$unitid, ".html"))
            # txt <- read_lines(f)
            # s1 <- "<div class=\"navbar navbar-inverse navbar-fixed-top\" role=\"navigation\">"
            # s2 <- "<div class=\"navbar navbar-inverse\" role=\"navigation\">"
            # txt <- str_replace(txt, s1, s2)
            # write_lines(txt, f)
#            f2 <- filter_fragment(f, search = "", replace = "")
            # issue https://community.rstudio.com/t/generating-markdown-reports-from-shiny/8676/5
            # includeHTML(f)
            # active_dash <- file.path(system.file("shiny-apps", "abm", "www", package = "bibliomatrix"),
            #     "activedash.html")
            # cat("copying ", f, " to ", active_dash)
            # res <- file.copy(f, "activedash.html", overwrite = TRUE)
            # cat("outcome: res")
            # embed_data <- function(path)
            #     paste0("data:", mime::guess_type(path), ";base64,", 
            #            base64enc::base64encode(path))
            tags$iframe(src = paste0(input$unitid, ".html"), width = "100%", height = "100%",
                        frameborder = 0, scrolling = 'auto')
        } else {
            cat("Embedding in iframe: ", dash_src, "\n")
            html <- httr::content(httr::GET(dash_src), as = "raw")
            data_uri <- sprintf("data:text/html;base64,%s", base64enc::base64encode(html))
            tags$iframe(src = data_uri, width = "100%", height = "100%",
                frameborder = 0, scrolling = 'auto')
        }
        
    })    
    
    # this could be used to render from within the package
    
    output$dash <- renderUI({
        
        req(input$unitid)
        
        if (input$unitid %in% abm_public_kth$meta$Diva_org_id) {
            uc <- 
                abm_public_kth$meta %>% 
                filter(Diva_org_id == as.integer(input$unitid)) %>% 
                pull(unit_code)
           

            report <- system.file("extdata", "abm.Rmd", package = "bibliomatrix")
            f <- rmarkdown::render(report, params = list(unit_code = uc,
                is_employee = FALSE, embed_data = FALSE, use_package_data = TRUE))
        } else {
            report <- system.file("extdata", "abm.Rmd", package = "bibliomatrix")
            f <- rmarkdown::render(report, params = list(unit_code = input$unitid,
                is_employee = TRUE, embed_data = TRUE, use_package_data = FALSE))
        }
        includeHTML(f)
        #readBin(f, "raw", n = file.info(f)$size)
    })
    
    # TODO: think about caching content... to www? and use...
    # tags$iframe(src = './myMarkdown.html', # put myMarkdown.html to /www
    #             width = '100%', height = '800px', 
    #             frameborder = 0, scrolling = 'auto'
    # )
    
}

