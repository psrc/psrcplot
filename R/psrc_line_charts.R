#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
NULL

#' Generic Line Chart
#'
#' Used for both static and interactive line functions.
#' @inheritParams shared_params
#' @param t A tibble or dataframe in long form for plotting
#' @param x The name of the variable you want plotted on the X-Axis
#' @param y The name of the variable you want plotted on the Y-Axis
#' @param dform Format for Date values 
#' @param breaks Break points to use if using a continuous scale, defaults to NULL
#' @param lwidth Width of lines, defaults to 1
#' @param alt Text to be used for alt-text, if desired - defaults to "NULL"
#' @param xlabel category-axis title to be used for chart, if desired - defaults to "NULL"
#' @param ylabel numeric-axis title to be used for chart, if desired - defaults to "NULL"
#' @param interactive Enable hover text and other interactive features - defaults to FALSE
#' @return line chart
#' 
generic_line <- function(t, x, y, fill, 
                         est=NULL, dec=0, dform="%b-%Y",  
                         breaks=NULL, lwidth=1, color="gnbopgy_5",
                         title=NULL, subtitle=NULL, source="",
                         alt=NULL, xlabel=NULL, ylabel=NULL,
                         interactive=FALSE){
  
  confirm_fonts() 
  
  # Create a color palette from PSRC palette
  grps <- t %>% select(all_of(fill)) %>% unique() %>% dplyr::pull()
  num.grps <- length(grps)
  l.colors <- unlist(psrcplot::psrc_colors[color])
  l.colors <- l.colors[1:num.grps]
  cols <- stats::setNames(l.colors, grps)
  
  # Estimate type determines the labels for the axis and the format of the value labels
  est <- t %>% dplyr::pull(.data[[x]]) %>% est_type_default()
  valfrmt <- est_number_formats(est)
  lab <- est_label_formats(est)
  xtype <- t %>% dplyr::pull(.data[[x]]) %>% class()
  
  c <- ggplot2::ggplot(data=t, 
                       ggplot2::aes(x=.data[[x]],
                                    y=.data[[y]],
                                    text=paste0(.data[[fill]], ": ", valfrmt$pfx, prettyNum(round(.data[[y]] * valfrmt$fac, dec), big.mark = ","), valfrmt$sfx),
                                    group=.data[[fill]]))  + 
    ggplot2::geom_line(ggplot2::aes(color=.data[[fill]]), linewidth=lwidth, linejoin = "round") +
    ggplot2::scale_y_continuous(labels = lab) +
    ggplot2::scale_color_manual(values=cols)  +
    ggplot2::labs(title=title, subtitle=subtitle, caption=source, alt=alt, x=xlabel, y=ylabel) +
    psrc_style()
  
  if (xtype=="Date"){
    c <- c + ggplot2::scale_x_date(labels = scales::date_format(dform)) + ggplot2::theme(axis.title.x=ggplot2::element_blank())
  }else{
    c <- c + ggplot2::geom_point(ggplot2::aes(color=.data[[fill]]))
    if(!is.null(breaks)){
      c <- c + ggplot2::scale_x_discrete(breaks=breaks)
    }  
  }
  
  # Make interactive
  if(interactive==TRUE){
    c <- make_interactive(p=c, title=title, subtitle=subtitle)
    if(!source==""){
      c <- add_citation(c, source)
    }
  }
  
  return(c)
}

#' Line charts
#'
#' Separate functions for static and interactive line charts
#'
#' @param t A tibble in long form for plotting
#' @param x The name of the variable you want plotted on the X-Axis
#' @param y The name of the variable you want plotted on the Y-Axis
#' @param fill The name of the variable you want the fill color of the bars to be based on
#' @param ... additional arguments passed to  \code{\link{generic_line}}
#' @name line_charts
#' @return static or interactive column or bar chart
NULL

#' @rdname line_charts
#' @title Generate static line chart
#' @export
static_line_chart <- function(t, x, y, fill, ...){
  c <- generic_line(t=t, x=x, y=y, fill=fill, interactive=FALSE, ...)
  return(c)
}

#' @rdname line_charts
#' @title Generate static line chart
#' @export
interactive_line_chart <- function(t, x, y, fill, ...){
  c <- generic_line(t=t, x=x, y=y, fill=fill, interactive=TRUE, ...)
  return(c)
}