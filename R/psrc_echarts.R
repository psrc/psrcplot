echarts4r::e_common(font_family = "Poppins")

tooltip_fmt <- "function(params, ticket, callback) {
      var fmt = new Intl.NumberFormat('en',
      {\"style\":\"percent\",\"minimumFractionDigits\":0,\"maximumFractionDigits\":0,\"currency\":\"USD\"});\n
      var idx = 0;\n
      if (params.name == params.value[0]) {\n
      idx = 1;\n        }\n
      return(params.seriesName + '<br>' + 
      params.marker + ' ' +\n
      params.name + ': ' + fmt.format(parseFloat(params.value[idx]))
      )
      }"

#' Create an echart4r bar or column chart 
#'
#' @param df A data frame
#' @param category_var The name of the category variable
#' @param numeric_var The name of the variable with numeric values to plot
#' @param fill The name of the variable you want the fill color of the bars to be based on
#' @param color A vector of colors or a color palette to fill the bars
#' @param pos The position either NULL (dodge) or "grp" (stacked)
#' @param column_vs_bar "column" or "bar"
#' @param est Estimate type, enter "percent", "currency" or "number", defaults to "percent"
#' @param title A chart title, defaults to NULL
#' @param subtitle A chart subtitle, defaults to NULL
#'
#' @return An interactive bar or column chart via echarts4r
#' @export
#'
#' @examples
#' library(tidyverse)
#' 
#' d <- mode_share_example_data |>
#' filter(Year == 2020) |> 
#' mutate(Race = str_wrap(Race, 20))
#' 
#' color_palette <- c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3')
#' 
#' echart_bar_chart(df = d ,
#'                 category_var = "Race",
#'                 numeric_var = "share",
#'                 fill = "Geography",
#'                 color = color_palette,
#'                 title = "Test Chart",
#'                 pos = NULL,
#'                 column_vs_bar = "bar",
#'                 est = "percent")
echart_bar_chart <- function(df, 
                             category_var, 
                             numeric_var, 
                             fill, 
                             color = NULL,
                             pos = "NULL",
                             column_vs_bar = "column",
                             est = "percent",
                             title = NULL,
                             subtitle = NULL) {
  
  # Create the most basic chart
  p <- df |> 
    dplyr::group_by(.data[[fill]]) |>
    echarts4r::e_charts_(category_var) |>
    echarts4r::e_bar_(numeric_var, stack = pos) |>
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
    if(est == "currency") {
      curr <- "USD"
    } else {
      curr <- NULL
    }
    
    p <- p |>
      echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter(est)) |>
      echarts4r::e_tooltip(trigger = "item",
                           formatter =  echarts4r::e_tooltip_item_formatter(style = est, digits = 0, currency = curr)) |>
      echarts4r::e_tooltip(formatter =  htmlwidgets::JS(tooltip_fmt))
    
  }
  
  if(column_vs_bar == "bar") {
    p <- p |>
      echarts4r::e_flip_coords() |>
      echarts4r::e_y_axis(inverse = TRUE)
  }
  
  return(p)
  
}