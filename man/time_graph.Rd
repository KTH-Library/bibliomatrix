% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ui-elements.R
\name{time_graph}
\alias{time_graph}
\title{Graph of indicators by year, with or without moving average and/or reference line}
\usage{
time_graph(
  df,
  indicator,
  ma = FALSE,
  weight = NULL,
  ylabel = NULL,
  refline = NULL,
  percent = FALSE
)
}
\arguments{
\item{df}{data frame to read indicators etc from}

\item{indicator}{df column name to graph}

\item{ma}{set to true for moving averages}

\item{weight}{weight to use for moving average}

\item{ylabel}{y-axis label to use in graph}

\item{refline}{optional y-reference line}

\item{percent}{set to true for percentage value}
}
\value{
ggplot
}
\description{
Graph of indicators by year, with or without moving average and/or reference line
}
