% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crawl_slug.R
\name{kthids_from_slug}
\alias{kthids_from_slug}
\title{Given a organizational unit slug, such as "j/jj/jjn", retrieves 
associated KTH researcher ids}
\usage{
kthids_from_slug(slug)
}
\arguments{
\item{slug}{character slug for an organizational unit}
}
\value{
tibble with kthids, usernames, titles and a timestamp
}
\description{
Given a organizational unit slug, such as "j/jj/jjn", retrieves 
associated KTH researcher ids
}
\details{
this fcn belongs in the kthapi package
}
\examples{
\dontrun{
if(interactive()){
 kthids_from_slug("j/jj/jjn")
 kthids_from_slug("j/jh/jhs")
 }
}
}
\seealso{
\code{\link[kthapi]{kth_catalog}},\code{\link[kthapi]{kth_profile}}
 \code{\link[progress]{progress_bar}}
 \code{\link[purrr]{safely}},\code{\link[purrr]{map}}
 \code{\link[lubridate]{now}}
}
