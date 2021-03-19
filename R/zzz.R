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
    Citations_3yr Diva_dep_id Diva_org_id Diva_school_id Doc_Year Jtop20
    P_frac Ptop10 Publication_Type_DiVA Publication_Type_WoS
    Publication_Year Publication_Year_ch Unit_Fraction Unit_Fraction_adj
    Unit_code WebofScience_ID cf cn dep_name int interval jcf key level
    org_level parent_org_id pt_ordning pt_ordning.x pt_ordning.y
    skola_namn swe_nuniv unit unit_code unit_id unit_long unit_long_eng
    unit_long_swe unit_sort value w_d_Sum w_d_Sum_NA w_unit wos_coverage
    wos_coverage_Mean wos_coverage_NA Doc_id wos_bin
    WoS_coverage df_copub df_jcf indicator int_share nonuniv_share
    top10_share top20_share year measure target
    from n_pad unit_long_en Co-publication formatStyle styleEqual uc_from_orgid
    abm_public_kth get_pt_ordning parent fullsort sort_order
    Publication_Type Unit_fraction p_frac p_full sumcov_frac sumcov_full
    C3 C3_frac Unit_fraction PID WoS_Journal P_tot bronze_count closed_count gold_count
    green_count hybrid_count is_oa oa_count oa_share oa_status variable uncited P_uncited Share_uncited
    Year group perc_text series percent_format current_date varname displayname description_short text
    kthid slug title.en username Unit_Fraction_adj_raw Unit_Fraction_raw Unit_Name n_pubs n_staff name
    name.x name.y nd_researchers pid unit_short DOI firstName lastName ScopusID
    C_avg C_sum P P_uncited_scop Share_uncited_scop abm_woscoverage
    scop_Jtop20 scop_Ptop10 scop_bin scop_corp scop_cscxo scop_doctype
    scop_fwci_x scop_int scop_snip sumscop_frac sumscop_full sumwos_frac
    sumwos_full P_full avg_snip corp_share description_en df_diva fwci_x is_kth
    description_en analysis_id
"

# HACK: add global fcns / vars with spaces in the name
spacenames <- c(
  "Organizational Unit",
  "OA type:",
  "Publication count",
  "Co-publication:"
)

str_remove <- function(x, y) gsub(paste(y, collapse='|'), "", x = x, fixed = TRUE)
globalz <- str_remove(globalz, spacenames)

str_split <- function(x) unlist(strsplit(trimws(x), "\\s+", perl = TRUE))
globalz <- c(str_split(globalz), spacenames)

if (getRversion() >= "2.15.1") utils::globalVariables(globalz)
