.onAttach <- function(libname, pkgname) {
# http://www.asciiset.com/figletserver.html (chunky)
banner <- "https://github.com/KTH-Library/bibliomatrix
 __     __  __     __  __                          __          __        
|  |--.|__||  |--.|  ||__|.-----..--------..---.-.|  |_ .----.|__|.--.--.
|  _  ||  ||  _  ||  ||  ||  _  ||        ||  _  ||   _||   _||  ||_   _|
|_____||__||_____||__||__||_____||__|__|__||___._||____||__|  |__||__.__|
(use suppressPackageStartupMessages() to silence this banner)"
suppressWarnings(packageStartupMessage(banner))
}

# CRAN Note avoidance, see https://stackoverflow.com/questions/9439256
# paste R CMD CHECK output for "Undefined global functions or variables below"
globalz <- "
    Diva_dep_id Diva_org_id Diva_school_id Doc_Year Organizational Unit
    Publication_Type_DiVA cn dep_name key pt_ordning pt_ordning.x
    pt_ordning.y skola_namn unit unit_id unit_long unit_long_eng
    unit_long_swe unit_sort value w_d_Sum w_d_Sum_NA w_unit wos_coverage
    wos_coverage_Mean wos_coverage_NA
"

# HACK: add global fcns / vars with spaces in the name
spacenames <- c(
  "Organizational Unit"
)

str_remove <- function(x, y) gsub(y, "", x = x, fixed = TRUE)
globalz <- str_remove(globalz, spacenames)

str_split <- function(x) unlist(strsplit(trimws(x), "\\s+", perl = TRUE))
globalz <- c(str_split(globalz), spacenames)

if (getRversion() >= "2.15.1") utils::globalVariables(globalz)