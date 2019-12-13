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
        if (input$use_prerendered) return (177)
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
    
    # using the API, which could be anywhere or external

    observe({
        req(input$unitid)
        # if the unit id not is for an org then use employee endpoint
        API <- ifelse(!input$unitid %in% abm_public_kth$meta$Diva_org_id, 
            ABM_API_EMP, ABM_API_UNIT)
        # TODO: this probably will affect other users sessions and should be changed
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
            f <- paste0(openssl::sha1(kthid()), ".html")
            if (!file.exists(f)) {
                cat("Embedding from: ", dash_src, "\n")
                w <- httr::GET(dash_src)
                if (httr::status_code(w) == 200) {
                    html <- httr::content(w, as = "raw")
                    loc_www <- system.file("shiny-apps", "abm", "www", package = "bibliomatrix")
                    writeBin(html, file.path(loc_www, f))
                } else {
                    msg <- sprintf("<pre>Error from API at %s</pre>", dash_src)
                    f <- sprintf("data:text/html;base64,%s", base64enc::base64encode(msg))
                }
            }
            #data_uri <- sprintf("data:text/html;base64,%s", base64enc::base64encode(html))
            tags$iframe(src = f, width = "100%", height = "100%",
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

