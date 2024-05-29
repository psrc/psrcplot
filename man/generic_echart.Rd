% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/psrc_echarts.R
\name{generic_echart}
\alias{generic_echart}
\title{Generic echart4r workhorse function
Helper function to \code{\link{echart_bar_chart}} and \code{\link{echart_line_chart}}}
\usage{
generic_echart(
  df,
  category_var,
  fill = NULL,
  color = NULL,
  est = "percent",
  title = NULL,
  subtitle = NULL,
  legend = TRUE,
  egrid_bottom = "15\%",
  egrid_left = "20\%",
  str_wrap_num = 0,
  x_axis_rotate = 0
)
}
\arguments{
\item{df}{A data frame in long form for plotting}

\item{category_var}{The name of the category variable}

\item{fill}{The name of the variable you want the fill color of the bars or lines to be based on}

\item{color}{A vector of colors or color palette}

\item{est}{Select "number", "percent", or "currency" - defaults to "percent"}

\item{title}{Chart title}

\item{subtitle}{Chart subtitle}

\item{legend}{TRUE or FALSE to display legend - defaults to TRUE}

\item{egrid_bottom}{Bottom axis padding - defaults to "15\%"}

\item{egrid_left}{Left axis padding - defaults to "20\%"}

\item{str_wrap_num}{Positive integer giving target line width (in number of characters) - defaults to 0}

\item{x_axis_rotate}{Positive integer giving x-axis label rotation - defaults to 0}
}
\value{
Does not return a chart, called within \code{\link{echart_bar_chart}} and \code{\link{echart_line_chart}}
}
\description{
Generic echart4r workhorse function
Helper function to \code{\link{echart_bar_chart}} and \code{\link{echart_line_chart}}
}