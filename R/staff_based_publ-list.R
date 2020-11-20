# Open/messy code script to get publication list, and a basis for starting to compare publication lists from staff-POC and running ABM.

unit_code <- "s/sk"
orgcode<- "sk"

#unit_members <- abm_researchers(unit_slug=unit_code) #unit_staff(unit_slug=unit_code)
unit_members<- unit_staff(unit_slug= unit_code)$kthid # live lookup against KTH-directory, to also get department lists

unit_publ<- abm_staff_data(kthids=unit_members)

writepath<- "C:/Users/tobias/temp/"
filename<- paste0("ABM_publist_staff_",orgcode,".xlsx")
filename
exportpath<- paste0(writepath,filename)

exportlist<- unit_publ %>% select(-Unit_Fraction_raw)
exportlist$Unit_code_slug<- unit_code

writexl::write_xlsx(path = exportpath,
                    as.data.frame(exportlist))

###
# Code to join staffbased list (from above) and orginal ÅBU-publication list (for departments) on PID
# keeps some relevant columns in output and creates a join-variable which shows which set a publication comes from. 

diva_code<- "6108"

abm_publist<- abm_publications(con = con_bib(), unit_code = diva_code)

publist_joined<- exportlist %>% full_join(abm_publist, by="PID") %>% 
    mutate(origin = ifelse((!is.na(Unit_code) & !is.na(Unit_code_slug)), "both", ifelse(is.na(Unit_code), "staffbased", ifelse(is.na(Unit_code_slug), "abm",NA)))) %>%
    select("PID", "WebofScience_ID.x", "WebofScience_ID.y", "Bibliographic_Information.x", "Bibliographic_Information.y", 
         "Publication_Year.x", "Publication_Year.y", "Publication_Type_DiVA.x", "Publication_Type_DiVA.y", "cf.x", "cf.y", 
         "jcf.x", "jcf.y", "Unit_code", "Unit_code_slug", "origin", "Diva_org_id") %>% 
    rename(Diva_org_id_staffbased = Diva_org_id)

filename_join<- paste0("ABM_publist_joined_",orgcode,".xlsx")
exportpath_join<- paste0(writepath,filename_join)
writexl::write_xlsx(path = exportpath_join,
                    as.data.frame(publist_joined))
