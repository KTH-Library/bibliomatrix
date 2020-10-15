#' Generate Web of Science attribution
#'
#' @param year the year to use for copyright note, current year if not given
#' @return a string with WoS attribution for given copyright year
#' @importFrom glue glue
#' @export
wos_attribution <- function(year){
  if(missing(year))
    year <- format(Sys.Date(), "%Y")
  glue("Certain data included herein are derived from the \u00A9 Science Citation Index Expended (SCIE), Social Sciences Citation Index (SSCI), ",
       "Arts & Humanities Citation Index (AHCI), Conference Proceedings Citation Index - Sciences (CPCI-S) and ",
       "Conference Proceedings Citation Index - Social Sciences & Humanities (CPCI -SSH) ({year}) of Clarivate Analytics (US) LLC. All rights reserved. ",
       "No part of these materials may be reproduced, stored in a retrieval system or transmitted in any form or by any means, ",
       "including electronic, mechanical, photographic, magnetic or other means without the expressed permission of the KTH Library.")
}

#' Generate Web of Science disclaimer
#'
#' @param derived_product the product the disclaimer should be applied on (short name)
#' @param derived_product_long a longer product name to use in the first mention of the product
#' @param bibliometric_partner the bibliometric partner that provide the derived product
#' @param firstyear the first year to use for copyright note, uses abm_config() if not given 
#' @param lastyear the last year to use for the copyright note, uses abm_config() if not given
#' @return a string with WoS disclamer
#' @importFrom glue glue
#' @export
wos_disclaimer <- function(derived_product = "ABM",
                           derived_product_long = "KTH Annual Bibliometric Monitoring",
                           bibliometric_partner = "KTH Royal Institute of Technology",
                           firstyear = abm_config()$start_year,
                           lastyear = abm_config()$stop_year){
  if (firstyear == lastyear){
    yeartext <- as.character(firstyear)
  } else{
    yeartext <- glue("{firstyear} - {lastyear}")
  }
  
  if(derived_product_long %in% c("", derived_product) || is.na(derived_product_long)){
    firstmention <- derived_product
  } else {
    firstmention <- glue("{derived_product_long} (referred to below as \u201c{derived_product}\u201d)")
  }

  glue("Copyright \u00A9 {yeartext}, Clarivate Analytics (US) LLC. All rights reserved. Clarivate Analytics (US) LLC LLC and its affiliates are referred to below as \u201cClarivate\u201d.
  Clarivate or its third party providers own and retain all rights, title and interest, including but not limited to copyright, trademarks, patents, database rights, trade secrets,
  know-how, and all other intellectual property rights or forms of protection of similar nature or having
  equivalent effect, anywhere in the world, in certain data underlying the {firstmention} and user is not granted any
  proprietary interest therein or thereto. The Clarivate data underlying the {derived_product} constitutes confidential and trade
  secrets of Clarivate or its third party providers. Display, performance, reproduction, distribution of, or creation of derivative works or
  improvements from the Clarivate data underlying the {derived_product} in any form or manner is expressly prohibited, except
  to the extent expressly permitted hereunder, or otherwise, with the prior written permission of Clarivate.
  
  [User may copy, paste and distribute only an insubstantial amount of the Clarivate data contained in the {derived_product} provided that:
  (a) the distribution is incidental to or supports user\u2019s business purpose,
  (b) the data is not distributed by user in connectionwith information vending or commercial publishing (in any manner or format whatsoever),
  not reproduced through the press or mass media or on the Internet, and
  (c) where practicable, clearly identifies Clarivate or its providers as the source of the data.
  Data will be considered in  \u201cinsubstantial amount\u201d if such amount
  (i) has no independent Clarivate value,
  (ii) could not be used by the recipient as a substitute for any product or service (including any download service) provided by Clarivate or a substantial part of it.]
  
  NEITHER CLARIVATE NOR ITS THIRD PARTY PROVIDERS WARRANT THAT THE PROVISION OF THE DATA UNDERLYING THE
  {toupper(derived_product)} WILL BE UNINTERRUPTED, ERROR FREE, TIMELY, COMPLETE OR ACCURATE, NOR DO THEY
  MAKE ANY WARRANTIES AS TO THE RESULTS TO BE OBTAINED FROM USE OF THE SAME. USE OF THE {toupper(derived_product)}
  AND RELIANCE THEREON IS AT USER\u2019S SOLE RISK. NEITHER CLARIVATE OR ITS THIRD PARTY PROVIDERS WILL IN
  ANY WAY BE LIABLE TO USER OR ANY OTHER ENTITY OR PERSON FOR THEIR INABILITY TO USE THE {toupper(derived_product)},
  OR FOR ANY INACCURACIES, ERRORS, OMISSIONS, DELAYS, COMPUTER VIRUS OR OTHER INFIRMITY OR
  CORRUPTION, DAMAGES, CLAIMS, LIABILITIES OR LOSSES, REGARDLESS OF CAUSE, IN OR ARISING FROM THE USE OF
  THE {toupper(derived_product)}. THE CLARIVATE DATA UNDERLYING THE {toupper(derived_product)} IS PROVIDED ON AN \u201cAS
  IS\u201d BASIS AND WITHOUT WARRANTY OF ANY KIND. NO WARRANTIES EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT
  LIMITED TO ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, TITLE, NON-INFRINGEMENT, QUIET ENJOYMENT OR OTHERWISE IS PROVIDED HEREUNDER.
  
  IN NO EVENT WILL CLARIVATE OR ITS THIRD PARTY PROVIDERS BE LIABLE FOR ANY DAMAGES, INCLUDING WITHOUT
  LIMITATION DIRECT OR INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, LOSSES OR EXPENSES ARISING
  IN CONNECTION WITH {toupper(derived_product)} EVEN IF CLARIVATE OR ITS THIRD PARTY PROVIDERS OR THEIR REPRESENTATIVES ARE ADVISED OF THE POSSIBILITY OF SUCH DAMAGES,
  LOSSES OR EXPENSES. FURTHER, CLARIVATE OR ITS PARTY PROVIDERS SHALL NOT BE LIABLE IN ANY MANNER FOR {toupper(bibliometric_partner)}\u2019S PRODUCTS OR SERVICE.")
}

#' Generate text for Web of Science coverage warnings
#'
#' @param woscoverage Web of Science coverage (between 0 and 1)
#' @return a string with coverage level based on given coverage
#' @export
coveragetext <- function(woscoverage){
  # TODO: should we return strings even for division by zero etc?
  if(woscoverage < 0 | woscoverage > 1)
    ret <- "NA"
    #ret <- "Invalid woscoverage, please give a number in the interval [0,1]"
  if(woscoverage >= 0.75)
    ret <- "good"
  if(woscoverage < 0.75 & woscoverage >= 0.6)
    ret <- "moderate"
  if(woscoverage < 0.6)
    ret <- "poor"
  ret
}
