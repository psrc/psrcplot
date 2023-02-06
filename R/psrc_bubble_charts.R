#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' @importFrom dplyr select all_of
NULL


#' Create PSRC TreeMap Chart
#'
#' This function allows you to create treemap charts.
#' @inheritParams shared_params
#' @param t A tibble or dataframe in long form for plotting
#' @param s The name of the variable with the value you want to use to size the bar
#' @return static treemap chart
#' 
#' @examples
#' 
#' library(dplyr)
#' 
#' # Read in the example data and filter to Mode to Work for Everyone for all years in the data
#' df <- psrcplot::mode_share_example_data %>% 
#'       filter(Category=="Mode to Work by Race") %>%
#'       filter(Geography=="Region" & Race=="Total") %>%
#'       mutate(Year = as.character(Year)) %>%
#'       filter(Year=="2020")
#' 
#' my.chart <- create_treemap_chart(t=df, s="share", fill="Mode", title="Mode Share to Work")
#' 
#' 
#' @export
#'

create_treemap_chart <- function(t, s, fill, title=NULL, subtitle=NULL, est="percent", dec=0, color="psrc_light") {
  
  confirm_fonts() 
  
  tot <- t %>% select(all_of(s)) %>% dplyr::pull() %>% sum()
  t <- t %>% dplyr::mutate(total_share = .data[[s]]/tot)
  
  # Estimate type determines the labels
  valfrmt <- est_number_formats(est)
  
  if (est=="percent") {
    c <- ggplot2::ggplot(t,
                         ggplot2::aes(area = .data[[s]],
                                      fill = .data[[fill]], 
                                      label = paste(.data[[fill]], 
                                                    paste0(valfrmt$pfx, prettyNum(round(.data[[s]]* valfrmt$fac, dec), big.mark = ","), valfrmt$sfx),
                                                    sep = "\n")))
  } else {
    c <- ggplot2::ggplot(t,
                         ggplot2::aes(area = .data[[s]],
                                      fill = .data[[fill]], 
                                      label = paste(.data[[fill]], 
                                                    paste0(valfrmt$pfx, prettyNum(round(.data[[s]] * valfrmt$fac, dec), big.mark = ","), valfrmt$sfx),
                                                    paste0(prettyNum(round(.data$total_share * 100,0), big.mark = ","), "%"), 
                                                    sep = "\n")))
  }
  c <- c + treemapify::geom_treemap() +
    treemapify::geom_treemap_text(colour = "white",
                                  place = "centre",
                                  size = 28) +
    psrc_style() +
    ggplot2::theme(legend.position = "none") +
    scale_fill_discrete_psrc(color) +
    ggplot2::ggtitle(title, subtitle = subtitle)
  
  return(c)
}

#' Create PSRC Bubble Chart
#'
#' This function allows you to create a bubble charts.
#' @param t A tibble or dataframe in long form for plotting
#' @param x The name of the variable you want plotted on the X-Axis
#' @param y The name of the variable you want plotted on the Y-Axis
#' @param fill The name of the variable you want the fill color of the bubbles to be based on
#' @param s The name of the variable used to size the bubbles
#' @param color Name of color palette to use - defaults to "psrc_light"
#' @param title Title to be used for chart, if desired - defaults to "NULL"
#' @param subtitle Sub-title to be used for chart, if desired - defaults to "NULL"
#' @return bubble chart
#' 
#' @export
#'
create_bubble_chart <- function(t, x, y, fill, s, color="psrc_light", title=NULL, subtitle=NULL) {
  
  confirm_fonts() 
  
  # Create a color palette from PSRC palette
  grps <- t %>% select(all_of(fill)) %>% unique() %>% dplyr::pull()
  num.grps <- length(grps)
  l.colors <- unlist(psrc_colors[color])
  l.colors <- l.colors[1:num.grps]
  cols <- stats::setNames(l.colors, grps)
  
  p <- ggplot2::ggplot(data=t, ggplot2::aes(x = .data[[x]], y = .data[[y]])) + 
    ggplot2::geom_point(ggplot2::aes(color = .data[[fill]], size = .data[[s]]), alpha = 1.0) +
    ggplot2::scale_size(range = c(0.5, 12)) +
    ggplot2::scale_color_manual(values=cols) +
    ggplot2::ggtitle(title, subtitle = subtitle) +
    psrc_style() +
    ggplot2::guides(size = "none") +
    ggplot2::theme(legend.position = "bottom",
                   axis.text.x = ggplot2::element_blank())
  
  return(p)
  
}