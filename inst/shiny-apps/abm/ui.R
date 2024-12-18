library(shiny)
library(shinydashboard)
library(shinythemes)
library(ktheme)
library(fresh)

abm_theme <- create_theme(
  adminlte_color(light_blue = kth_colors("blue"),
                 red = kth_colors("red"),
                 green = kth_colors("green"),
                 aqua = kth_colors("lightblue"),
                 yellow = kth_colors("yellow"),
                 blue = kth_colors("blue"),
                 navy = kth_colors("darkblue"),
                 teal = kth_colors("teal"),
                 olive = kth_colors("darkgreen"),
                 lime = kth_colors("lightgreen"),
                 orange = kth_colors("darkyellow"),
                 gray_lte = kth_colors("gray"))
)



# ----------------------
# Layout: sidebar with filter
# TODO: workound various issue with getting the sizing to work properly

dashboardPage(
  skin = "blue",
  header =  dashboardHeader(),
  dashboardSidebar(
    div(
      br(img(src = "logo_vit_80.png")),
      br(paste("KTH ABM ", bibliomatrix::abm_config()$stop_year + 1)),
      style = paste0("text-align: center;",
                     "background-color: ", kth_colors("blue"), ";",
                     "font-size: 20px;"),
      br()
    ),
    uiOutput("units"),
    sidebarMenuOutput("switcher")
  ),
  dashboardBody(
    use_theme(abm_theme),
    use_googlefont("Figtree"),
    # Catch display width to always show sidebar toggle (through dashboarHeader) when display is narrow
    tags$head(
      tags$script(HTML("
        document.addEventListener('DOMContentLoaded', function() {
          var header = document.querySelector('.main-header');
          var updateVisibility = function() {
            header.style.display = window.innerWidth > 800 ? 'none' : 'block';
          };
          updateVisibility();
          window.addEventListener('resize', updateVisibility);
        });
      "))),
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
    ')), HTML("
      <!-- Matomo -->
      <script>
        var _paq = window._paq = window._paq || [];
        _paq.push(['trackPageView']);
        _paq.push(['enableLinkTracking']);
        (function() {
          var u=\"https://analytics.sys.kth.se/\";
          _paq.push(['setTrackerUrl', u+'matomo.php']);
          _paq.push(['setSiteId', '3']);
          var d=document, g=d.createElement('script'), s=d.getElementsByTagName('script')[0];
          g.async=true; g.src=u+'matomo.js'; s.parentNode.insertBefore(g,s);
        })();
      </script>")),
    box(width = 12, height = "97vh", 
      tags$style(type = "text/css", "#frame {height: calc(100vh - 50px) !important;}"),
      htmlOutput("frame")
    ),
    tags$head(tags$style(".skin-blue .main-sidebar {background-color: white; padding-top: 10px; font-family: Figtree;}"))
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
