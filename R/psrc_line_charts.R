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
  if(is.null(est)){est <- t %>% dplyr::pull(.data[[y]]) %>% est_type_default()}
  valfrmt <- est_number_formats(est)
  lab <- est_label_formats(est)
  xtype <- t %>% dplyr::pull(.data[[x]]) %>% class()
  
  t %<>% dplyr::arrange(.data[[fill]])  # Factor ordering
  
  c <- ggplot2::ggplot(data=t, 
                       ggplot2::aes(x=.data[[x]],
                                    y=.data[[y]], 
                                    text=paste0(.data[[fill]], ": ", valfrmt$pfx, prettyNum(formattable::digits(round(.data[[y]] * valfrmt$fac, dec), digits=max(0, dec)), big.mark = ","), valfrmt$sfx),
                                    group=.data[[fill]]))  + 
    ggplot2::geom_line(ggplot2::aes(color=.data[[fill]]), linewidth=lwidth, linejoin = "round", na.rm=TRUE) +
    ggplot2::scale_color_manual(values=cols)  +
    ggplot2::scale_y_continuous(labels=lab, expand=ggplot2::expansion(mult = c(0, .2)))  +   # expand is to accommodate value labels
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

#' Facet Line Chart
#' 
#' @inheritParams shared_params
#' @param t A tibble or dataframe in long form for plotting
#' @param x The name of the variable you want plotted on the X-Axis
#' @param y The name of the variable you want plotted on the Y-Axis
#' @param facet The name of the variable to be the facets 
#' @param scales Value for axis in facets, either "fixed" or "free" - defaults to "free"
#' @param ncol Value for the number of columns in your facet - defaults to 3
#' @param ... additional arguments passed to  \code{\link{generic_line}}
#'
#' @return A static facet line chart; based on facet_wrap()
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' 
#' @export
static_facet_line_chart <- function(t, x, y, fill, 
                                    facet, scales="free", ncol=3,
                                    ...){
  
  confirm_fonts()
  
  l.clr <- "#4C4C4C"
  
  t %<>% dplyr::arrange(.data[[fill]]) # Factor ordering
  p <- static_line_chart(t=t, x=x, y=y, fill=fill, ...)
  
  x.vals <- length(unique(p$data[[x]]))
  # display x-axis tick values if another variable is introduced
  if(x != fill) {
    if(x.vals > 5) {
      # smaller font size and wrap/angle labels if there's a lot of x categories
      p <- p +
        ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20))
      axis.text.x.value <- ggplot2::element_text(angle = 90, size = 7, vjust = 0.5, hjust=1)
    } else {
      axis.text.x.value <- ggplot2::element_text(size = 7)
    }
  }
  
  # add facet and theme
  p <- p +
    ggplot2::facet_wrap(ggplot2::vars(.data[[facet]]), 
                        labeller = ggplot2::label_wrap_gen(),
                        scales = scales, 
                        ncol = ncol) +
    psrc_style() +
    ggplot2::theme(axis.text.x = axis.text.x.value,
                   axis.text.y = ggplot2::element_text(size = 9, color = l.clr),
                   strip.text = ggplot2::element_text(family = 'Poppins', size = 10),
                   panel.grid.major.y = ggplot2::element_blank()
    ) 
  
  return(p)
}

#' Cleveland Dot Chart
#' 
#' Creates lines showing range by category, with dot endpoints
#' credit https://uc-r.github.io/cleveland-dot-plots
#' 
#' @inheritParams shared_params
#' @param t A tibble or dataframe in long form for plotting
#' @param x The name of the variable you want plotted on the X-Axis (numeric)
#' @param y The name of the variable you want plotted on the Y-Axis (categorical)
#' @param fill The name of the variable supplying line endpoints (e.g. time points)
#' @param lwidth Width of lines; defaults to 1.5
#' @param dotsize Defaults to 3
#' @param shape Of dot and legend; can be either value 1:25 or name; defaults to "circle"
#' see https://ggplot2.tidyverse.org/articles/ggplot2-specs.html?q=shapes#sec:shape-spec
#'
#' @return A Cleveland line chart
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' @import ggplot2
#' 
#' @export
cleveland_dot_chart <- function(t, x, y, fill, 
                                est=NULL, dec=0, dform="%b-%Y", lwidth=1.5,
                                dotsize=3, color="gnbopgy_5", shape="circle",
                                title=NULL, subtitle=NULL, source="",
                                alt=NULL, xlabel=NULL, ylabel=NULL){
  
  confirm_fonts()
  
  # Estimate type determines the labels for the axis and the format of the value labels
  x_series <- dplyr::pull(t, {{x}})
  if(is.null(est)){est <- x_series %>% est_type_default()}
  lab <- est_label_formats(est)
  if(max(x_series)>1e6){lab <- scales::unit_format(unit = "M", scale = 1e-6)
  }else if(max(x_series)>1e9){lab <- scales::unit_format(unit = "B", scale = 1e-9)
  }
  
  # Fill values used for color scale and type check for legend
  fill_minmax <- dplyr::pull(t, {{fill}})
  fill_minmax <- c(min(fill_minmax), max(fill_minmax))
  filltype <- class(fill_minmax)
  dot_colors <- unlist(psrcplot::psrc_colors[color]) %>% stats::setNames(fill_minmax)

  # construct the plot
  c <- dplyr::filter(data, .data[[fill]] %in% fill_minmax) %>% 
    ggplot(aes(x = formattable::digits(round(.data[[x]], dec), digits = max(0, dec)), 
               y = forcats::fct_rev(.data[[y]]))) +
    geom_line(aes(group = .data[[y]]), color = "#999999", linewidth = lwidth) +
    scale_color_discrete(type = dot_colors) +
    geom_point(aes(color = as.factor(.data[[fill]]), shape = shape), size = dotsize, na.rm = TRUE) + 
    scale_shape_identity() +
    psrc_style() +
    theme(axis.title = element_blank(),
          legend.direction ="horizontal",
          legend.position ="top",
          text = element_text(family = "Poppins")) +
    guides(color = guide_legend(override.aes = list(shape = shape))) +
    scale_x_continuous(labels = lab) +
    scale_y_discrete(labels = wrap_labels_evenly(18)) +
    labs(title = title, subtitle = subtitle, caption = source, alt = alt, x = xlabel, y = ylabel)
  
  if (filltype=="Date"){
    c <- c + scale_color_manual(labels = scales::date_format(dform))
  }
  
  return(c)
}
