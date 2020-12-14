#' Link to DiVA portal at KTH
#' 
#' @import htmltools
#' @export
abm_ui_button_diva <- function() {
  
  # library(magick)
  # 
  # "https://kth.diva-portal.org/dream/diva2image/kth/favicon.ico" %>%
  #   image_read() %>%
  #   image_convert(format = "png") %>%
  #   image_resize("x20")
  #   image_transparent(color = "white", fuzz = 15) %>%
  #   image_write("~/Pictures/diva-logo.png")
  # 
  #   base64data <- sprintf("data:image/png;base64,%s", base64enc::base64encode("~/Pictures/diva-logo.png"))
  #   cat(base64data)
  
  base64data <- "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAANlBMVEUAAAAZVKYZVKYZVKYZVKYZVKYZVKYZVKYZVKYZVKYZVKYZVKYZVKYZVKYZVKYZVKYZVKb///8zlazLAAAAEHRSTlMAj0DvMCDPcN9Qn2Cvv4AQUwCY1wAAAAFiS0dEEeK1PboAAAAHdElNRQfkBQsTBA1aXV/VAAAAYElEQVQY041PWw7AIAhDUVCZs/c/7VziNDNZsn40lEcBoi+4TXveGxBmHEUDJTjKZcwFBps/gPL0ZPjOVZZDven0UxuUqCVpU3Mtp1SMTHR9o8B6RZeLIr4PC2n/xegvLjd9AkBkaFDYAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIwLTA1LTExVDE5OjA0OjEzKzAyOjAwX+Io9gAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMC0wNS0xMVQxOTowNDoxMyswMjowMC6/kEoAAAAASUVORK5CYII="
  withTags(
    div(class = "diva-button", 
      span(title = "Please register publications in DiVA as soon as possible for these to be included in the Annual Bibliometric Monitoring",
        a(href = "https://kth.diva-portal.org/dream", class = "btn btn-sm btn-primary", target = "_blank", 
          img(src = base64data),
          "Edit your publication data in DiVA")
      )
    )
  )  
}

#' Link to Altmetric Explorer for an organizational unit at KTH
#' 
#' @param altmetric_count the count to include in a tooltip
#' @param altmetric_href the target of the link
#' @param unit_label the label for the organizational unit
#' @import htmltools
#' @export
abm_ui_button_altmetric <- function(altmetric_count, altmetric_href, unit_label) {
  # img src value comes from embed_data("~/Pictures/altmetric-badge.png")
  # altmetric_badge.png comes from https://badges.altmetric.com/?size=20&score=123456&types=dgmabvtf
  HTML(sprintf('<div class="altmetric-button"><a href="%s" class="btn btn-sm btn-primary" target="_blank">
     <img src="data:image/png;base64,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">
     </img> Explore Altmetric Research Output for %s</a></div>',
     altmetric_href, unit_label))
}

#' Link to Publication List download for KTH
#' 
#' @param is_loggedin boolean parameter indicating logged in status
#' @param unit_label the label for the organizational unit
#' @param unit_code the unit code for the organizational unit
#' @param unit_file_label the label for the download
#' @param is_authorbased indicates if the button is used for author based
#'   data, default: FALSE
#' @import htmltools flexdashboard
#' @importFrom mime guess_type
#' @importFrom writexl write_xlsx
#' @export
abm_ui_button_publist <- function(is_loggedin, unit_label, unit_code, unit_file_label,
                                  is_authorbased = FALSE) {
  
  current_date <- format(Sys.Date(), "%Y%m%d")
  
  if (is_loggedin == TRUE) {
    
    embed_data <- function(path)
      paste0("data:", mime::guess_type(path), ";base64,", 
             base64enc::base64encode(path))
    
    embed_file_link <- function(path, .href = embed_data(path), 
      .name = basename(path), .text = paste("Download", .name), ...) {
      withTags(p(a(.text, href = .href, download = .name, ...)))
    }
    
    icon_download <- htmltools::tagList(htmltools::tag("i", list(class = "fa fa-download")), 
      HTML(paste("Publication list for", unit_label))) 
    
    icon_download <- htmltools::attachDependencies(icon_download,
      flexdashboard:::html_dependencies_fonts(TRUE, FALSE))
    
    # export to xlsx format, into tempdir so we can embed a file link
    
    # data that we want to export
    if (!isTRUE(is_authorbased)) {
      con <- pool_bib()
      publications_kth <- abm_publications(con = con, unit_code = unit_code)
      pool::poolClose(con)
      
      filename <- paste0("ABM_PubList_", unit_file_label, "_", current_date, ".xlsx")
      excel_file <- file.path(tempdir(), filename)
    } else {
      # we have publications for a "slug"
      con <- pool_bib()
      # resolve the unit_code to a vector of kthids
      publications_kth <- 
        abm_publications_staffbased(con = con, unit_code = kthids_from_slug(unit_code)$kthid)
      pool::poolClose(con)
      filename <- paste0("ABM_PubList_", unit_file_label, "_", current_date, ".xlsx")
      excel_file <- file.path(tempdir(), filename)
    }
    
    # Create excel workbook
    writexl::write_xlsx(path = excel_file,
       x = list(
         Data = as.data.frame(publications_kth), 
         Attribution = data.frame(attribution = wos_attribution())
      )
    )
    
    embed_file_link(excel_file,
      title = "Download Publication List in Excel format",
      text = icon_download, 
      class = "btn btn-sm btn-primary")
  } else {
    HTML("Publication data is available only after login.")
  }
}

#' Summary valuebox for publications
#' 
#' @param df_diva data frame with DiVA publication data in a specific format
#' @param lastyear previous year
#' @param start_year first year of total interval of years
#' @param stop_year last year of total interval of years
#' @param unit_label the unit label to use in case of no data
#' @import htmltools ktheme
#' @importFrom glue glue
#' @importFrom flexdashboard valueBox
#' @export
abm_ui_summary_pubs <- function(df_diva, lastyear,  
    start_year, stop_year, unit_label) {
  
  if (nrow(df_diva) > 0) {
    
    total_pubs <- sum(df_diva[, as.character(lastyear)], na.rm = TRUE)
    
    vb1 <- flexdashboard::valueBox(
      value = round(total_pubs, 1),
      color = unname(ktheme::palette_kth(4)["blue"]),
      icon = "fa-chart-bar",
      href = "#publications-in-diva")
    
    vb1
  } else {
    HTML(glue("<p><i>{unit_label} has no publications registered in DiVA {start_year} - {stop_year}</i></p>"))
  }
}

#' Make ABM table have last rows bold with gray background, other rows with white background
#'
#' @param t the table to be formatted
#' @importFrom DT formatStyle styleEqual
#' @noRd
abm_format_rows <- function(t) {
  DT::formatStyle(table = t,
    columns = 1,
    target = "row",
    fontWeight = DT::styleEqual("Total", "bold"),
    backgroundColor = DT::styleEqual("Total", "#DDDDDD", "#FFFFFF"))
}


#' @importFrom DT formatStyle
#' @noRd
abm_format_columns_divatable <- function(t, column_name, has_left_border) {
  DT::formatStyle(table = t,
    columns = column_name,
    target = "cell",
    fontWeight = "bold",
    backgroundColor = "#EEEEEE",
    borderLeft = ifelse(has_left_border, "1px solid #CCCCCC", "")
  )
}

#' @importFrom DT formatStyle
#' @noRd
abm_format_header_divatable <- function(header) {
  header %>%
    sub("th\\('Publications', class = 'display dt-left', style = '",
        "th('Total', class = 'display dt-left', style = 'background-color:#EEEEEE; border-left: 1px solid #CCCCCC; ", .) %>%
    sub("th\\('WoS coverage', class = 'display dt-left', style = '",
        "th('WoS coverage', class = 'display dt-left', style = 'background-color:#EEEEEE; ", .)
}

getheader <- function(indics) {
  getcell <- function(indic) {
    indicrow <- get_indic_descriptions() %>%
      filter(tolower(varname) == tolower(indic))
    if(nrow(indicrow) == 1){
      disp <- indicrow %>% pull(displayname)
      popup <- indicrow %>% pull(description_short)
    } else {
      disp = indic
      popup = ""
    }
    disp <- disp %>% nowrap_hyphen_sub()
    glue("th('{disp}', class = 'display dt-left', style = 'padding-left: 10px; padding-right: 10px;', title = '{popup}')")
  }
  cells <- paste(lapply(indics, getcell), collapse = ",")
  paste0("htmltools::withTags(table(class = 'display dt-right', thead(tr(", cells, "))))")
}

getcolnames <- function(indics) {
  getname <- function(indic) {
    newname <- get_indic_descriptions() %>%
      filter(tolower(varname) == tolower(indic)) %>%
      pull(displayname)
    if(is_empty(newname)){
      indic
    } else {
      newname
    }
  }
  sapply(indics, getname)
}


#' Datatable for DiVA publications
#' 
#' @param df_diva data frame with DiVA publication data in a specific format
#' @param unit_file_label the filename presented when users make use of the download button
#' @param unit_title the label presented when users make use of the download button
#' @import htmltools
#' @importFrom DT formatRound formatPercentage formatStyle
#' @export
abm_ui_datatable_diva <- function(df_diva, unit_file_label, unit_title) {
  
  current_date <- format(Sys.Date(), "%Y%m%d")
  
  if (nrow(df_diva) > 0) {
    
    filename <- paste0("ABM_table1_", unit_file_label, "_", current_date)
    
    header <- eval(parse(text = getheader(names(df_diva)) %>% abm_format_header_divatable()))
    
    DT::datatable(df_diva,
      container = header,
      rownames = FALSE,
      extensions = c("Buttons"),
      style = "bootstrap", class = "compact", width = "720",
      options = list(
        ordering = FALSE,
        bPaginate = FALSE,
        pageLength = 100,
        dom = 'tB',
        buttons = list(
          list(extend = "copy", title = unit_title),
          list(extend = "csv", filename = filename, title = unit_title),
          list(extend = "excel", filename = filename, title = unit_title))
        )) %>%
      DT::formatRound(2:(length(df_diva)-1), digits = 1, mark = "") %>%
      DT::formatPercentage(length(df_diva), digits = 1) %>%
      abm_format_rows() %>%
      abm_format_columns_divatable("P_frac", has_left_border = TRUE) %>%
      abm_format_columns_divatable("WoS_coverage", has_left_border = FALSE)
    
  } else {
    withTags(p(style = "font-style: italic;", "There are no publications available for this table"))
  }
}

#' HTML table for DiVA publications
#' 
#' @param df_diva data frame with DiVA publication data in a specific format
#' @import htmltools
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling scroll_box column_spec
#' @export
abm_ui_kable_diva <- function(df_diva) {
  
  if (nrow(df_diva) > 0) {
    df_diva %>%
      mutate_at(vars(-"Publication_Type_DiVA", -"WoS_coverage"), round, digits = 1) %>%
      mutate_at(vars(starts_with("WoS")), function(x) sprintf("%3.1f%%", x * 100)) %>%
      kable(col.names = getcolnames(names(df_diva)),
            align = c("l", rep("r", ncol(df_diva) - 1))) %>%
      column_spec(column = ncol(df_diva) - 1, background = "#EEEEEE", border_left = "solid 1px #CCCCCC", include_thead = TRUE) %>%
      column_spec(column = ncol(df_diva), background = "#EEEEEE", include_thead = TRUE) %>%
      kable_styling(bootstrap_options = c("responsive")) %>%
      scroll_box(width = "720px")
  } else {
    withTags(p(style = "font-style: italic;", "There are no publications available for this table"))
  }
}

#' Datatable for 3 year citations
#' 
#' @param df_city3y data frame with DiVA publication data in a specific format
#' @param unit_file_label the filename presented when users make use of the download button
#' @param unit_title the label presented when users make use of the download button
#' @import htmltools
#' @importFrom DT formatRound formatPercentage formatStyle
#' @export
abm_ui_datatable_city3y <- function(df_city3y, unit_file_label, unit_title) {

  current_date <- format(Sys.Date(), "%Y%m%d")
  
  if (nrow(df_city3y) > 0) {
    filename <- paste0("ABM_table2_", unit_file_label, "_", current_date)
    
    header <- eval(parse(text = getheader(names(df_city3y))))
    
    DT::datatable(df_city3y,
      container = header,
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        ordering = FALSE,
        bPaginate = FALSE,
        dom = 'tB',
        buttons = list(
          list(extend = "copy", title = unit_title),
          list(extend = "csv", filename = filename, title = unit_title),
          list(extend = "excel", filename = filename, title = unit_title))
        )) %>%
      DT::formatRound(2:5, digits = 1, mark = "") %>%
      DT::formatPercentage(6, digits = 1) %>%
      abm_format_rows()
  } else {
    withTags(p(style = "font-style: italic;", "There are no publications available for this table"))
  }  
}

#' HTML table for 3 year citations
#' 
#' @param df_cit3y data frame with DiVA publication data in a specific format
#' @import htmltools 
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling scroll_box
#' @export
abm_ui_kable_cit3y <- function(df_cit3y) {
  if (nrow(df_cit3y) > 0) {
    df_cit3y %>% 
      mutate_at(vars(2:5), function(x) sprintf("%.1f", x)) %>%
      mutate_at(vars(6), function(x) sprintf("%.1f%%", x * 100)) %>%
      kable(col.names = getcolnames(names(df_cit3y)),
            align = c("l", rep("r", ncol(df_cit3y) - 1))) %>%
      kable_styling(bootstrap_options = c("responsive")) %>%
      scroll_box(width = "720px")
  } else {
    withTags(p(style = "font-style: italic;", "There are no publications available for this table"))
  }  
}

#' Datatable for 3 year citations (cf)
#' 
#' @param df_cf data frame with DiVA publication data in a specific format
#' @param unit_file_label the filename presented when users make use of the download button
#' @param unit_title the label presented when users make use of the download button
#' @import htmltools
#' @importFrom DT formatRound formatPercentage formatStyle
#' @export
abm_ui_datatable_cf <- function(df_cf, unit_file_label, unit_title) {
  
  current_date <- format(Sys.Date(), "%Y%m%d")
  
  if (nrow(df_cf) > 0) {
    filename <- paste0("ABM_table3_", unit_file_label, "_", current_date)
    
    header <- eval(parse(text = getheader(names(df_cf))))
    
    DT::datatable(df_cf,
      container = header,
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        ordering = FALSE,
        bPaginate = FALSE,
        dom = 'tB',
        buttons = list(
          list(extend = "copy", title = unit_title),
          list(extend = "csv", filename = filename, title = unit_title),
          list(extend = "excel", filename = filename, title = unit_title))
      )
    ) %>% 
      formatRound(c(2, 4), digits = 1, mark = "") %>% 
      formatRound(3, digits = 2, mark = "") %>%
      formatPercentage(5, digits = 1) %>%
      abm_format_rows()
  } else {
    withTags(p(style = "font-style: italic;", "There are no publications available for this table"))
  }  
}

#' HTML table for 3 year citations (cf)
#' 
#' @param df_cf data frame with DiVA publication data in a specific format
#' @import htmltools
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling scroll_box
#' @export
abm_ui_kable_cf <- function(df_cf) {
  if (nrow(df_cf) > 0) {
    df_cf %>% 
      mutate_at(vars(2, 4), function(x) sprintf("%.1f", x)) %>%
      mutate_at(vars(3), function(x) sprintf("%.2f", x)) %>%
      mutate_at(vars(5), function(x) sprintf("%.1f%%", x * 100)) %>%
      kable(col.names = getcolnames(names(df_cf)),
            align = c("l", rep("r", ncol(df_cf) - 1))) %>% 
      kable_styling(bootstrap_options = c("responsive")) %>%
      scroll_box(width = "720px")
  } else {
    withTags(p(style = "font-style: italic;", "There are no publications available for this table"))
  }
}

#' Datatable for journal impact (jcf)
#' 
#' @param df_jcf data frame with DiVA publication data in a specific format
#' @param unit_file_label the filename presented when users make use of the download button
#' @param unit_title the label presented when users make use of the download button
#' @import htmltools
#' @importFrom DT formatRound formatPercentage formatStyle
#' @export
abm_ui_datatable_jcf <- function(df_jcf, unit_file_label, unit_title) {
  
  current_date <- format(Sys.Date(), "%Y%m%d")
  
  if (nrow(df_jcf) > 0) {
    filename <- paste0("ABM_table4_", unit_file_label, "_", current_date)
    
    header <- eval(parse(text = getheader(names(df_jcf))))
    
    DT::datatable(df_jcf,
                  container = header,
                  rownames = FALSE,
                  extensions = "Buttons",
                  options = list(
                    ordering = FALSE,
                    bPaginate = FALSE,
                    dom = 'tB',
                    buttons = list(
                      list(extend = "copy", title = unit_title),
                      list(extend = "csv", filename = filename, title = unit_title),
                      list(extend = "excel", filename = filename, title = unit_title))
                    )) %>% 
      formatRound(c(2, 4), digits = 1, mark = "") %>% 
      formatRound(3, digits = 2, mark = "") %>% 
      formatPercentage(5, digits = 1) %>%
      abm_format_rows()
  } else {
    withTags(p(style = "font-style: italic;", "There are no publications available for this table"))
  }
}

#' HTML table for journal impact (jcf)
#' 
#' @param df_jcf data frame with DiVA publication data in a specific format
#' @import htmltools 
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling scroll_box
#' @export
abm_ui_kable_jcf <- function(df_jcf) {
  if (nrow(df_jcf) > 0) {
    df_jcf %>% 
      mutate_at(vars(2, 4), function(x) sprintf("%.1f", x)) %>%
      mutate_at(vars(3), function(x) sprintf("%.2f", x)) %>%
      mutate_at(vars(5), function(x) sprintf("%.1f%%", x * 100)) %>%
      kable(col.names = getcolnames(names(df_jcf)),
            align = c("l", rep("r", ncol(df_jcf) - 1))) %>%
      kable_styling(bootstrap_options = c("responsive")) %>%
      scroll_box(width = "720px")
  } else {
    withTags(p(style = "font-style: italic;", "There are no publications available for this table"))
  }
}

#' Datatable for co-publication
#' 
#' @param df_copub data frame with DiVA publication data in a specific format
#' @param unit_file_label the filename presented when users make use of the download button
#' @param unit_title the label presented when users make use of the download button
#' @import htmltools
#' @importFrom DT formatRound formatPercentage formatStyle
#' @export
abm_ui_datatable_copub <- function(df_copub, unit_file_label, unit_title) {
  
  current_date <- format(Sys.Date(), "%Y%m%d")
  
  if (nrow(df_copub) > 0) {
    filename <- paste0("ABM_table5_", unit_file_label, "_", current_date)
    
    header <- eval(parse(text = getheader(names(df_copub))))
    
    DT::datatable(df_copub,
      container = header,
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        ordering = FALSE,
        bPaginate = FALSE,
        dom = 'tB',
        buttons = list(
          list(extend = "copy", title = unit_title),
          list(extend = "csv", filename = filename, title = unit_title),
          list(extend = "excel", filename = filename, title = unit_title))
        )) %>%
      formatPercentage(c(4,6), digits = 1) %>%
      abm_format_rows()
  } else {
    withTags(p(style = "font-style: italic;", "There are no publications available for this table"))
  }  
}

#' HTML table for co-publication
#' 
#' @param df_copub data frame with DiVA publication data in a specific format
#' @import htmltools 
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling scroll_box
#' @export
abm_ui_kable_copub <- function(df_copub) {
  if (nrow(df_copub) > 0) {
    df_copub %>% 
      mutate_at(vars(4, 6), function(x) sprintf("%.1f%%", x * 100)) %>%
      kable(col.names = getcolnames(names(df_copub)),
            align = c("l", rep("r", ncol(df_copub) - 1))) %>%
      kable_styling(bootstrap_options = c("responsive")) %>%
      scroll_box(width = "720px")
  } else {
    withTags(p(style = "font-style: italic;", "There are no publications available for this table"))
  }
}


#' Datatable for open access publications
#' 
#' @param df_oa data frame with DiVA publication data in a specific format
#' @param unit_file_label the filename presented when users make use of the download button
#' @param unit_title the label presented when users make use of the download button
#' @import htmltools
#' @importFrom DT formatRound formatPercentage formatStyle
#' @export
abm_ui_datatable_oa <- function(df_oa, unit_file_label, unit_title) {
  
  current_date <- format(Sys.Date(), "%Y%m%d")
  
  if (nrow(df_oa) > 0) {
    
    filename <- paste0("ABM_open_access_", unit_file_label, "_", current_date)
    
    header <- eval(parse(text = getheader(names(df_oa))))
    
    DT::datatable(df_oa,
      container = header,
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        ordering = FALSE,
        bPaginate = FALSE,
        dom = 'tB',
        buttons = list(
          list(extend = "copy", title = unit_title),
          list(extend = "csv", filename = filename, title = unit_title),
          list(extend = "excel", filename = filename, title = unit_title))
      )) %>%
      formatPercentage(9, digits = 1) %>%
      abm_format_rows()
  } else {
    withTags(p(style = "font-style: italic;", "There are no publications available for this table"))
  }
  
}

#' HTML table for open access publications
#' 
#' @param df_oa data frame with DiVA publication data in a specific format
#' @import htmltools 
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling scroll_box
#' @export
abm_ui_kable_oa <- function(df_oa) {
  if (nrow(df_oa) > 0) {
    df_oa %>% 
      mutate_at(vars(9), function(x) sprintf("%.1f%%", x * 100)) %>%
      kable(col.names = getcolnames(names(df_oa)),
            align = c("l", rep("r", ncol(df_oa) - 1))) %>%
      kable_styling(bootstrap_options = c("responsive")) %>%
      scroll_box(width = "720px")
  } else {
    withTags(p(style = "font-style: italic;", "There are no publications available for this table"))
  }
}
