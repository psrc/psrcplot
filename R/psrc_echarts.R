echarts4r::e_common(font_family = "Poppins")

#' Format echart4r Tooltip
#'
#' @param est Select "number", "percent", or "currency"
#' @param fill A grouping variable, defaults to NULL
#' @param column_vs_bar Select "column" or "bar" for chart orientation - defaults to NULL
#' @return JavaScript string that will format echart4r tooltip text. Is called within \code{\link{generic_echart}}
#'
tooltip_fmt <- function(est, fill = NULL, column_vs_bar = NULL) {
  if(est == 'number') est <- 'decimal'
  style_quoted <- paste0('"', est, '"')
  
  # adjust maximumFractionDigits
  if(est %in% c('percent', 'currency')) {
    max_fd <- 0
  } else {
    max_fd <- 2
  }
  
  if((!is.null(fill)) & (is.null(column_vs_bar) || column_vs_bar == "column")) {
    # line & column
    r <- "params.seriesName + '<br>' + params.marker + ' ' + params.value[0] + ': ' + fmt.format(params.value[1])" 
    
  } else if (!is.null(fill) & column_vs_bar == "bar") {
    # bar
    r <- "params.seriesName + '<br>' + params.marker + ' ' + params.value[1] + ': ' + fmt.format(params.value[0])" 
    
  } else {
    # When data is not grouped by a variable & filled with color
    r <- "params.seriesName + '<br>' + params.marker + fmt.format(params.value[1])"
    
  }
  
  t <- paste0("function(params, ticket, callback) {
                  let fmt = new Intl.NumberFormat('en', {style:", style_quoted, ", minimumFractionDigits:0, maximumFractionDigits:", max_fd, ", currency: 'USD'});
                  return(", r, ")","}")
}

#' Generic echart4r workhorse function
#' Helper function to \code{\link{echart_bar_chart}} and \code{\link{echart_line_chart}}
#'
#' @param df A data frame in long form for plotting
#' @param category_var The name of the category variable
#' @param fill The name of the variable you want the fill color of the bars or lines to be based on
#' @param color A vector of colors or color palette
#' @param est Select "number", "percent", or "currency" - defaults to "percent"
#' @param title Chart title
#' @param subtitle Chart subtitle
#'
#' @return Does not return a chart, called within \code{\link{echart_bar_chart}} and \code{\link{echart_line_chart}}
generic_echart <- function(df,
                           category_var,
                           fill = NULL,
                           color = NULL,
                           est = "percent",
                           title = NULL,
                           subtitle = NULL) {
  
  # Create the most basic chart
  if(is.null(fill)) {
    p <- df
  } else {
    p <- df |>
      dplyr::group_by(.data[[fill]]) 
  }
  
  p <- p |>
    echarts4r::e_charts_(category_var) |>
    echarts4r::e_color(color) |>
    echarts4r::e_title(title, subtitle) |>
    echarts4r::e_grid(left = '20%') |>
    echarts4r::e_x_axis(axisTick = list(show = FALSE)) |>
    echarts4r::e_show_loading() |>
    echarts4r::e_legend(show = TRUE, bottom = 0) |>
    echarts4r::e_toolbox_feature("dataView") |>
    echarts4r::e_toolbox_feature("saveAsImage") 
  
  if(est == "number") {
    p <- p |> echarts4r::e_tooltip(trigger = "item")
    
  } else {
    p <- p |>
      echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter(est))
  }
  
  return(p)
}

#' echart4r Bar Chart
#' 
#' @param t A data frame in long form for plotting
#' @param x The name of the category variable
#' @param y The name of the variable with numeric values to plot
#' @param est Select "number", "percent", or "currency" - defaults to "percent"
#' @param fill The name of the variable you want the fill color of the bars or lines to be based on
#' @param pos Determines if the bars are side-by-side(NULL) or stacked("grp") - defaults to NULL
#' @param column_vs_bar Select "column" or "bar" for chart orientation - defaults to "column"
#' @param ... additional arguments passed to  \code{\link{generic_echart}}
#' 
#' @return An interactive bar or column chart via echarts4r
#' @author Christy Lam
#' @export
#'
#' @examples
#' library(tidyverse)
#'
#'d <- mode_share_example_data |>
#'  filter(Year == 2020 & Category == 'Population by Race') |>
#'  mutate(Race = str_wrap(Race, 20))
#'
#'color_palette <- c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3')
#'
#'echart_bar_chart(t = d,
#'                 x = "Race",
#'                 y = "share",
#'                 fill = "Geography",
#'                 color = color_palette,
#'                 title = "Population by Race",
#'                 subtitle = "In 2020",
#'                 pos = NULL,
#'                 column_vs_bar = "bar",
#'                 est = "number")
#'
echart_bar_chart <- function(t,
                             x,
                             y, 
                             est,
                             fill,
                             pos = "NULL",
                             column_vs_bar = "column", ...) {
  
  p <- generic_echart(df = t, category_var = x, est = est, fill = fill, ...) |>
    echarts4r::e_bar_(y, stack = pos) 
  
  if(column_vs_bar == "bar") {
    p <- p |>
      echarts4r::e_flip_coords() |>
      echarts4r::e_y_axis(inverse = TRUE)
  }
  
  p <- p |>
    echarts4r::e_tooltip(formatter =  htmlwidgets::JS(tooltip_fmt(est, fill, column_vs_bar)))
  
  return(p)
}

#' echart4r Line Chart
#'
#' @param t A data frame in long form for plotting
#' @param x The name of the category variable
#' @param y The name of the variable with numeric values to plot
#' @param est Select "number", "percent", or "currency" - defaults to "percent"
#' @param fill The name of the variable you want the fill color of the bars or lines to be based on
#' @param ... Additional arguments passed to \code{\link{generic_echart}}
#'
#' @return An interactive line chart via echarts4r
#' @export
#'
echart_line_chart <- function(t,
                              x,
                              y, 
                              est,
                              fill,
                              ...) {
  
  p <- generic_echart(df = t, category_var = x, est = est, fill = fill, ...) |>
    echarts4r::e_line_(y) |>
    echarts4r::e_tooltip(formatter =  htmlwidgets::JS(tooltip_fmt(est, fill, column_vs_bar = NULL)))
  
  return(p)
}
