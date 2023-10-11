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

if (ABM_API == "") 
    ABM_API <- "http://localhost:8080"

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
            
            jwt <- Sys.getenv("SHINYPROXY_OIDC_ACCESS_TOKEN")
            if (jwt != "") {
                payload <- abm_decode_jwt(jwt)
                kthid <- payload$kthid
                kthid <- setNames(kthid, sprintf("%s (%s)", payload$unique_name[1], payload$username))
                #setNames(kthid, displayname_from_kthid(kthid))
                return (kthid)
            }
            
            if (is_saml(ua)) {
                kthid <- parse_id(ua)
                kthid <- setNames(kthid, displayname_from_kthid(kthid))
            } else {
                # if shinyproxy with saml then we get user identity as kthid@kth.se
                # if shinyproxy with ldap then we get user identity as LDAP accountname
                message("Not shinyproxy/shiny and not SAML, warning... appears to use LDAP")
                kthid <- setNames(kthid, displayname_from_kthid(kthid))
            }
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
                           choices = orgs, selected = 1, #default_org_id(kthid()),
                           multiple = FALSE, selectize = TRUE, width = "100%")
    })
    
    dash_src <- function(id) {
        if (id %in% abm_public_kth$meta$Diva_org_id)
            return (sprintf(ABM_API_UNIT, id))
        if (id == kthid())
            return (sprintf(ABM_API_EMP, id))
    }
    
    is_employee <- function(id) id == kthid()
    
    output$switcher <- renderUI({
        if (!ABM_IS_PUBLIC) {
            sidebarMenu(
                id = "tabs",
                menuItem("Switch to public app", href = ABM_URL_PUBLIC, 
                         icon = icon("sign-out"), 
                         badgeLabel = "Go", badgeColor = "blue")
            )    
        } else (
            sidebarMenu(
                id = "tabs",
                menuItem(htmltools::HTML("Use your KTH account <br>to view your own data"), href = ABM_URL_PRIVATE, 
                         icon = icon("sign-in"), 
                         badgeLabel = "Go", badgeColor = "blue")
            )    
        )
        
    
    })
    
    output$login <- renderUI({
        if (!ABM_IS_PUBLIC) {
            out <- tags$li(class = "dropdown", 
                style = "padding-top:7px; padding-bottom:7px;",
                tags$li(class = "dropdown", 
                    tags$a(href = ABM_URL_PUBLIC,
                       class = "button button-primary button-sm",
                       "Switch to public app", icon("sign-out"), " ... ",
                       title = "Switch to the public version of this application")
                )
            )
        } else if (ABM_IS_PUBLIC == TRUE) {
            out <- tags$li(class = "dropdown", 
                style = "padding-top:7px; padding-bottom:7px;",
                tags$li(class = "dropdown", 
                    tags$a(href = ABM_URL_PRIVATE, 
                           style = "padding:7px;",
                        class = "button button-primary button-sm",
                        "Use your KTH account", icon("sign-in"), " ... ", img(src = "KTH_logo_RGB_bla.png", height = 30, width = 30),
                        title = "Switch to view your own publications if you are a KTH researcher")
                )
            )            
        } else {
            out <- ""
        }
        
        out
    })
    
    output$frame <- renderUI({
        
        req(input$unitid)
        
        cat("Report for:", input$unitid, "private visibility:", !ABM_IS_PUBLIC, "\n")
        
        report <- abm_report(id = input$unitid, is_private = !ABM_IS_PUBLIC)
        is_ok <- !is.null(report)
            
        if (is_ok) {    
            
            #tags$div(HTML(rawToChar(report)))  # this causes CSS issues w navbar
            #b64 <- base64enc::dataURI(data = report, mime = "text/html")  # this causes slow enc
            
            # NB: permission issue if not writing to cache dir (has o+wr)
            tf <- paste0(file.path("cache", openssl::sha1(rawToChar(report))), ".html")
            readr::write_file(report, paste0("www/", tf))
            
            htmltools::tags$iframe(src = tf, 
                frameborder = 0, scrolling = "auto", height="100%", width="100%")
            
        } else {
            paste("No report available for this unit, please contact the system owner.")
        }
        

    })    
    
}

