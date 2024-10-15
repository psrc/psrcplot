#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' @importFrom dplyr select all_of
NULL

#' Create PSRC Bubble Chart
#'
#' This function allows you to create a bubble charts.
#' @param t A tibble or dataframe in long form for plotting
#' @param x The name of the variable you want plotted on the X-Axis
#' @param y The name of the variable you want plotted on the Y-Axis
#' @param fill The name of the variable you want the fill color of the bubbles to be based on
#' @param s The name of the variable used to size the bubbles
#' @param color A vector of colors, otherwise uses ggplot2 default color palette - defaults to "NULL"
#' @param title Title to be used for chart, if desired - defaults to "NULL"
#' @param subtitle Sub-title to be used for chart, if desired - defaults to "NULL"
#' @return bubble chart
#' @author Craig Helmann
#' 
#' @export
#'
create_bubble_chart <- function(t, x, y, fill, s, color=NULL, title=NULL, subtitle=NULL) {
  
  confirm_fonts() 
  
  # Create a color palette from PSRC palette
  grps <- t %>% select(all_of(fill)) %>% unique() %>% dplyr::pull()
  num.grps <- length(grps)
  
  t %<>% dplyr::arrange(.data[[fill]]) # Factor ordering
  
  p <- ggplot2::ggplot(data=t, ggplot2::aes(x = .data[[x]], y = .data[[y]])) + 
    ggplot2::geom_point(ggplot2::aes(color = .data[[fill]], size = .data[[s]]), alpha = 1.0, na.rm = TRUE) +
    ggplot2::scale_size(range = c(0.5, 12)) +
    ggplot2::ggtitle(title, subtitle = subtitle) +
    psrc_style() +
    ggplot2::guides(size = "none") +
    ggplot2::theme(legend.position = "bottom",
                   axis.text.x = ggplot2::element_blank())
  
  # Apply color palette if available, check enough colors are available
  if(!is.null(color)) {
    equal_colors_and_groups <- length(color) == length(grps)
    
    if(equal_colors_and_groups == FALSE & length(color) < length(grps)) {
      stop(paste("Not enough colors in color palette. There are", length(color), "colors but", num.grps, "groups"))
    }
    
    cols <- stats::setNames(color, grps)
    p <- p +
      ggplot2::scale_fill_manual(values=cols)
  }
  
  return(p)
  
}