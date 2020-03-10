library(shiny)
library(shinydashboard)
library(shinythemes)

library(bibliomatrix)
library(dplyr)
library(purrr)

# deploy to shiny in root context
#ln -s /usr/local/lib/R/site-library/bibliomatrix/shiny-apps/abm/* .

# for test purposes
# NB cache location will vary dep on loading with Ctrl-Shift-L or not
# TODO config below could be a fcn

#Sys.setenv("ABM_IS_PUBLIC" = "TRUE")
#Sys.setenv("ABM_IS_PUBLIC" = "")

ABM_IS_PUBLIC <- ifelse(Sys.getenv("ABM_IS_PUBLIC") != "", TRUE, FALSE)
#Sys.setenv("ABM_API" = "")

ABM_URL_PUBLIC <- ifelse(Sys.getenv("ABM_URL_PUBLIC") != "", Sys.getenv("ABM_URL_PUBLIC"), "https://kth.se/abm/public")
ABM_URL_PRIVATE <- ifelse(Sys.getenv("ABM_URL_PRIVATE") != "", Sys.getenv("ABM_URL_PRIVATE"), "https://kth.se/abm")

ABM_API <- Sys.getenv("ABM_API")

if (ABM_API == "") ABM_API <- "http://localhost:8080"

ABM_API_UNIT <- ifelse(ABM_IS_PUBLIC, 
   paste0(ABM_API, "/unit/%s/flexdashboard"),
   paste0(ABM_API, "/unit/%s/flexdashboard?embeddata=true")
)

ABM_API_EMP <- paste0(ABM_API, "/employee/%s/flexdashboard")

server <- function(input, output, session) {

    kthid <- function() {
        if (!ABM_IS_PUBLIC) {
            ua <- Sys.getenv("SHINYPROXY_USERNAME")
            if (ua == "") ua <- "u1g9umtq@kth.se"
            re_saml <- "(.*)@kth\\.se$"
            is_saml <- function(x) stringr::str_detect(x, re_saml)
            parse_id <- function(x) stringr::str_match(x, re_saml)[,2]
            
            # if shinyproxy with saml then we get user identity as kthid@kth.se
            # if shinyproxy with ldap then we get user identity as LDAP accountname
            if (is_saml(ua)) {
                kthid <- parse_id(ua)
            } else {
                # we are not using public content and not saml -> likely LDAP
                kthid <- ad_kthid(ua)
            }
            kthid <- setNames(kthid, ad_displayname(kthid))
        } else {
            kthid <- NULL
        }
        
        return (kthid)
    }
    
    default_org_id <- function(kthid){
        # Set selected org to KTH for now.
        return(177)
    }
    
    output$units <- renderUI({
        
        orgs <- abm_public_kth$meta$Diva_org_id %>% 
            set_names(abm_public_kth$meta$unit_long_en_indent2)
        
        if (!ABM_IS_PUBLIC)
            orgs <- c(kthid(), orgs)
        
        shiny::selectInput(inputId = "unitid", label = "Select unit", 
                           choices = orgs, selected = default_org_id(kthid()),
                           multiple = FALSE, selectize = TRUE, width = "100%")
    })
    
    dash_src <- function(id) {
        if (id %in% abm_public_kth$meta$Diva_org_id)
            return (sprintf(ABM_API_UNIT, id))
        if (id == kthid())
            return (sprintf(ABM_API_EMP, id))
    }
    
    is_employee <- function(id) id == kthid()
    
    output$login <- renderUI({
        if (!ABM_IS_PUBLIC) {
            out <- tags$li(class = "dropdown", 
                style = "padding-top:7px; padding-bottom:7px;",
                tags$li(class = "dropdown", 
                    tags$a(href = ABM_URL_PUBLIC,
                       class = "button button-primary button-sm",
                       "Sign out", icon("sign-out"), " ... ",
                       title = "Sign out and go to the public version of this application")
                )
            )
        } else if (ABM_IS_PUBLIC == TRUE) {
            out <- tags$li(class = "dropdown", 
                style = "padding-top:7px; padding-bottom:7px;",
                tags$li(class = "dropdown", 
                    tags$a(href = ABM_URL_PRIVATE, 
                        class = "button button-primary button-sm",
                        "Sign in", icon("sign-in"), " ... ", img(src = "kth-logo.png", height = 30, width = 30),
                        title = "Sign in here to view your own publications if you are a student or employed at KTH")
                )
            )            
        } else {
            out <- ""
        }
        
        cat(ABM_IS_PUBLIC, ": public state\n")
        out
    })
    
    output$frame <- renderUI({
        
        req(input$unitid)
        
        loc_www <- system.file("shiny-apps", "abm", "www", "cache",
           package = "bibliomatrix", mustWork = TRUE)
        
        if (ABM_IS_PUBLIC == TRUE) {
            f <- paste0(input$unitid, ".html")
        } else {
            if (is_employee(input$unitid)) {
                f <- paste0(openssl::sha1(input$unitid), ".html")
            } else {
                f <- paste0(input$unitid, "-embed.html")
            }
        }
        
        ff <- file.path(loc_www, f)
        if (!file.exists(ff)) {
            www <- dash_src(input$unitid)
            cat("Not in cache, requesting from: ", www, "\n")
            w <- try(httr::GET(www), silent = TRUE)
            if (!inherits(w, 'try-error') && httr::status_code(w) == 200) {
                html <- httr::content(w, as = "raw")
            } else {
                html <- charToRaw(sprintf("<h1>API at %s did not return data</h1>", www))
                #f <- sprintf("data:text/html;base64,%s", base64enc::base64encode(msg))
                f <- "error.html"
                ff <- file.path(loc_www, f)
            }
            cat("Writing ", f, " to ", ff, "\n")
            writeBin(html, ff)
        } else {
            cat("Found in cache: ", f, "\n")
        }
        
        tags$iframe(src = paste0("cache/", f), 
            width = "100%", height = "100%",
            frameborder = 0, scrolling = 'auto')
    })    
    
}

