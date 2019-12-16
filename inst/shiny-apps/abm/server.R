library(shiny)
library(shinydashboard)
library(shinythemes)

library(bibliomatrix)
library(dplyr)
library(purrr)

# deploy to shiny in root context
#ln -s /usr/local/lib/R/site-library/bibliomatrix/shiny-apps/abm/* .

ABM_API <- Sys.getenv("ABM_API")
if (ABM_API == "") ABM_API <- "http://localhost:8080"
ABM_API_UNIT <- paste0(ABM_API, "/unit/%s/flexdashboard?embeddata=true")
ABM_API_EMP <- paste0(ABM_API, "/employee/%s/flexdashboard")

server <- function(input, output, session) {

    ua <- Sys.getenv("SHINYPROXY_USERNAME")
    if (ua == "") ua <- "u1g9umtq@kth.se"
    re_saml <- "(.*)@kth\\.se$"
    is_saml <- function(x) stringr::str_detect(x, re_saml)
    parse_id <- function(x) stringr::str_match(x, re_saml)[,2]
    
    kthid <- function() {
        # if shinyproxy with saml then we get user identity as kthid@kth.se
        # if shinyproxy with ldap then we get user identity as LDAP accountname
        if (is_saml(ua)) {
            kthid <- parse_id(ua)
        } else {
            # we are not using public content and not saml -> likely LDAP
            kthid <- ad_kthid(ua)
        }
        kthid <- setNames(kthid, ad_displayname(kthid))
        return (kthid)
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
    
    dash_src <- function(id) {
        if (id == kthid())
            return (sprintf(ABM_API_EMP, id))
        if (id %in% abm_public_kth$meta$Diva_org_id)
            return (sprintf(ABM_API_UNIT, id))
    }
    
    is_employee <- function(id) id == kthid()
    
    output$frame <- renderUI({
        
        req(input$unitid)
        loc_www <- system.file("shiny-apps", "abm", "www", package = "bibliomatrix")
        
        if (input$use_prerendered == TRUE) {
            f <- paste0(input$unitid, ".html")
            ff <- file.path(loc_www, f)
            if (!file.exists(ff))
                cat("Couldn't find cached data for ", f, "\n")
        } else if (input$use_prerendered == FALSE) {
            f <- paste0(input$unitid, ".html")
            if (is_employee(input$unitid))
                f <- paste0(openssl::sha1(input$unitid), ".html")
            ff <- file.path(loc_www, f)
            if (!file.exists(ff)) {
                www <- dash_src(input$unitid)
                cat("Not in cache, requesting from: ", www, "\n")
                w <- try(httr::GET(www), silent = TRUE)
                if (!inherits(w, 'try-error') && httr::status_code(w) == 200) {
                    html <- httr::content(w, as = "raw")
                    writeBin(html, ff)
                } else {
                    msg <- charToRaw(sprintf("<h1>API did not return data</h1>"))
                    f <- sprintf("data:text/html;base64,%s", base64enc::base64encode(msg))
                }
            } else {
                cat("Embedding from cache: ", f, "\n")
            }

        }
        tags$iframe(src = f, width = "100%", height = "100%",
                    frameborder = 0, scrolling = 'auto')
    })    
    
    
}

