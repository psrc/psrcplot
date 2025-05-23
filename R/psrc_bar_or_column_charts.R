#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' @importFrom dplyr select all_of
NULL

#' Generic column/bar workhorse function
#' 
#' Helper function to \code{\link{column_bar_charts}}
#'
#' @inheritParams shared_params
#' @param t A tibble in long form for plotting
#' @param category_var The name of the category variable
#' @param numeric_var The name of the variable with numeric values to plot
#' @param pos Determines if the bars are side-by-side(dodge) or stacked(stack) - defaults to "dodge"
#' @param moe The name of the variable to be used for error bars, if desired - default is "NULL"
#' @param dform Format for Date values 
#' @param alt Text to be used for alt-text, if desired - defaults to "NULL"
#' @param category_label category-axis title to be used for chart, if desired - defaults to "NULL"
#' @param numeric_label numeric-axis title to be used for chart, if desired - defaults to "NULL"
#' @param column_vs_bar "column": vertical bars or "bar": horizontal bars - defaults to "column" 
#' @param interactive Enable hover text and other interactive features - defaults to FALSE
#' @return static or interactive column or bar chart
#' @author Craig Helmann, Michael Jensen

generic_column_bar <- function(t, category_var, numeric_var, fill,
                               pos="dodge", est=NULL, moe=NULL, dform="%Y",
                               title=NULL, subtitle=NULL, source="", alt=NULL,
                               category_label=NULL, numeric_label=NULL, 
                               column_vs_bar="column",
                               dec = 0, color="pgnobgy_5",
                               interactive=FALSE){
  
  # do we want to do this? let's discuss:
  confirm_fonts()
  
  # Determine the Maximum Value to ensure bar labels are not cut-off
  max_item <- select(t, all_of(numeric_var)) %>% dplyr::pull() %>% max()
  
  # Create a color palette from PSRC palette
  grps <- select(t, all_of(fill)) %>% unique() %>% dplyr::pull()
  num.grps <- length(grps)
  l.colors <- unlist(psrcplot::psrc_colors[color])
  l.colors <- l.colors[1:num.grps]
  cols <- stats::setNames(l.colors, grps)

  # Estimate type determines the labels for the axis and the format of the value labels
  if(is.null(est)){est <- t %>% dplyr::pull(.data[[numeric_var]]) %>% est_type_default()}
  valfrmt <- est_number_formats(est)
  lab <- est_label_formats(est)
  xtype <- t %>% dplyr::pull(.data[[category_var]]) %>% class()
  
  # Create the Basic Static Chart
  t %<>% dplyr::arrange(.data[[fill]])
  c <- ggplot2::ggplot(t,
                       ggplot2::aes(x=if(column_vs_bar=="bar"){forcats::fct_rev(.data[[category_var]])}else{.data[[category_var]]},
                                    y=.data[[numeric_var]],
                                    text=paste0(.data[[fill]], ": ", valfrmt$pfx, prettyNum(formattable::digits(round(.data[[numeric_var]] * valfrmt$fac, dec), digits=max(0, dec)), big.mark = ","), valfrmt$sfx),
                                    fill=.data[[fill]],
                                    group=if(column_vs_bar=="bar"){forcats::fct_rev(.data[[fill]])}else{.data[[fill]]})) +
    ggplot2::geom_bar(position=pos, stat="identity", na.rm=TRUE, width=0.8) + 
    ggplot2::scale_fill_manual(values=cols) +
    ggplot2::scale_y_continuous(labels=lab, expand = ggplot2::expansion(mult = c(0, .2)))  +   # expand is to accommodate value labels
    ggplot2::labs(title=title, subtitle = subtitle, caption = source, alt = alt, x = category_label, y = numeric_label) +
    psrcplot::psrc_style()
                                    
  
  if ("Date" %in% xtype){
    c <- c + ggplot2::scale_x_date(labels = scales::date_format(dform)) + ggplot2::theme(axis.title.x=ggplot2::element_blank())
  }
  
  # If there are margins of error, add error bars
  if (!is.null(moe) & pos == 'dodge') {
    c <- c + ggplot2::geom_errorbar(ggplot2::aes(ymin=.data[[numeric_var]]-.data[[moe]], 
                                    ymax=.data[[numeric_var]]+.data[[moe]]), 
                                    width=0.2, position = ggplot2::position_dodge(0.8))
  } else if (!is.null(moe) & pos == 'stack') {
    c <- c + ggplot2::geom_errorbar(ggplot2::aes(ymin=.data[[numeric_var]]-.data[[moe]], 
                                                 ymax=.data[[numeric_var]]+.data[[moe]]), 
                                    width=0.2, position = ggplot2::position_stack())
  }
  
  # Pivot for bar chart
  # Also make the lines for the numeric values flip
  if(column_vs_bar=="bar"){
    c <- c + ggplot2::coord_flip()
    c<- c+
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), 
                     panel.grid.major.x = ggplot2::element_line(color="#cbcbcb"))
  }
 
  # Handling static charts with no MOE: they have value labels but no numeric axis labels and no grid lines
  # Add value labels if there is no error bar or moe and remove the category-variable axis since we have the labels
  # placement of the labels is different between column and bar charts to look nicer with hjust or vjust
  if(is.null(moe) & interactive==FALSE & pos == 'dodge'){
    # adjust vjust or hjust depending on chart type
    if(column_vs_bar =='column') {
      t <- ggplot2::geom_text(ggplot2::aes(x=.data[[category_var]],
                                           y=.data[[numeric_var]],
                                           label=paste0(valfrmt$pfx, prettyNum(formattable::digits(round(.data[[numeric_var]] * valfrmt$fac, dec), digits=max(0, dec)), big.mark = ","), valfrmt$sfx)),
                              check_overlap = TRUE,
                              position = ggplot2::position_dodge(0.8),
                              vjust = -0.20,
                              size = 11*0.32,
                              family="Poppins")
    } else if(column_vs_bar =='bar') {
      t <- ggplot2::geom_text(ggplot2::aes(x=.data[[category_var]],
                                           y=.data[[numeric_var]],
                                           label=paste0(valfrmt$pfx, prettyNum(formattable::digits(round(.data[[numeric_var]] * valfrmt$fac, dec), digits=max(0, dec)), big.mark = ","), valfrmt$sfx)),
                              check_overlap = TRUE,
                              position = ggplot2::position_dodge(0.8),
                              hjust = -0.20,
                              size = 11*0.32,
                              family="Poppins")
    }
    c <- c +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                     panel.grid.major.y = ggplot2::element_blank())
    
    c <- c + t
    
  } else if(is.null(moe) & interactive==FALSE & pos == 'stack') {
    c<- c +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                     panel.grid.major.y = ggplot2::element_blank())
    
    c <- c + ggplot2::geom_text(ggplot2::aes(x=.data[[category_var]],
                                             y=.data[[numeric_var]],
                                             label=paste0(valfrmt$pfx, prettyNum(formattable::digits(round(.data[[numeric_var]] * valfrmt$fac, dec), digits=max(0, dec)), big.mark = ","), valfrmt$sfx)),
                                check_overlap = TRUE,
                                position = ggplot2::position_stack(vjust = .5),
                                size = 11*0.32,
                                family="Poppins")
  } 
  
  # need to add some buffer around the axis because of the labels
  if(is.null(moe) & interactive==FALSE & column_vs_bar =='column') {
    c <- c +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     axis.line.x = ggplot2::element_line(color="#cbcbcb")) 
    
  } else if(is.null(moe) & interactive==FALSE & column_vs_bar =='bar') {
    c <- c +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.line.y = ggplot2::element_line(color="#cbcbcb")) 
  }
  
  # Remove legend if unneccesary
  if (num.grps == 1 | category_var==fill) {   
    c <- c + ggplot2::theme(legend.position = "none")  
  }
  
  # only wrap for column charts
  if(column_vs_bar=="column"){
    # Smaller font size and wrap/angle labels if there's a lot of x categories
    x.vals <- length(unique(c$data[[category_var]]))
    if(category_var != fill & x.vals > 5 & "ScaleDiscrete" %in% class(layer_scales(c)$x)) {
      axis.text.x.value <- ggplot2::element_text(angle = if(column_vs_bar!="bar"){90}else{0}, 
                                                 size = 9, vjust = 0.5, hjust=1)
      c <- c + 
        ggplot2::scale_x_discrete(labels = wrap_labels_evenly(15)) + 
        ggplot2::theme(axis.text.x = axis.text.x.value)
    } else if(category_var != fill & x.vals <= 5) {
      axis.text.x.value <- ggplot2::element_text(size = 9)
      c <- c + ggplot2::theme(axis.text.x = axis.text.x.value)
    }
  }
  # Interactivity
  if(interactive==TRUE){
    c <- make_interactive(p=c, title=title, subtitle=subtitle)
    # Turn on Source if Provided
    if(!source==""){
      c <- add_citation(c, source)
    }
  }
  
  return(c)
}

#' Column and bar charts
#'
#' Separate functions for static and interactive, column and bar
#'
#' @param t A tibble in long form for plotting
#' @param x The name of the variable you want plotted on the X-Axis
#' @param y The name of the variable you want plotted on the Y-Axis
#' @param fill The name of the variable you want the fill color of the bars to be based on
#' @param xlabel category-axis title to be used for chart, if desired - defaults to "NULL"
#' @param ylabel numeric-axis title to be used for chart, if desired - defaults to "NULL"
#' @param ... additional arguments passed to  \code{\link{generic_column_bar}}
#' @name column_bar_charts
#' @return static or interactive column or bar chart
#' @author Craig Helmann, Michael Jensen
NULL

#' @rdname column_bar_charts
#' @title Generate static column (vertical bar) chart
#' @export
static_column_chart <- function(t, x, y, fill, xlabel=NULL, ylabel=NULL, ...){
  c <- generic_column_bar(t=t, category_var=x, numeric_var=y, fill=fill,
                          category_label=xlabel, numeric_label=ylabel,
                          column_vs_bar="column", ...)
  return(c)
}

#' @rdname column_bar_charts
#' @title Generate static horizontal bar chart
#' @export
static_bar_chart <- function(t, x, y, fill, xlabel=NULL, ylabel=NULL, ...){
  c <- generic_column_bar(t=t, category_var=y, numeric_var=x, fill=fill,
                          category_label=ylabel, numeric_label=xlabel,
                          column_vs_bar="bar", ...)
  return(c)
}

#' @rdname column_bar_charts
#' @title Generate interactive column (vertical bar) chart
#' @export
interactive_column_chart <- function(t, x, y, fill, xlabel=NULL, ylabel=NULL, ...){
  c <- generic_column_bar(t=t, category_var=x, numeric_var=y, fill=fill,
                          category_label=xlabel, numeric_label=ylabel,
                          column_vs_bar="column", interactive=TRUE, ...)
  return(c)
}

#' @rdname column_bar_charts
#' @title Generate interactive horizontal bar chart
#' @export
interactive_bar_chart <- function(t, x, y, fill, xlabel=NULL, ylabel=NULL, ...){
  c <- generic_column_bar(t=t, category_var=y, numeric_var=x, fill=fill,
                          category_label=ylabel, numeric_label=xlabel,
                          column_vs_bar="bar", interactive=TRUE, ...)
  return(c)
}

#' Create Static Facet Column Charts
#'
#' This function allows you to create facet column charts based on ggplot2's facet_wrap().
#' @inheritParams shared_params
#' @param t A tibble or dataframe in long form for plotting
#' @param x The name of the variable you want plotted on the X-Axis
#' @param y The name of the variable you want plotted on the Y-Axis
#' @param pos The position of the columns, either "dodge" or "stack" - defaults to "dodge"
#' @param facet The name of the variable to be the facets
#' @param scales Value for axis in facets, either "fixed" or "free" - defaults to "free"
#' @param ncol Value for the number of columns in your facet - defaults to 3
#' @param moe The name of the variable to be used for error bars, if desired - default to "NULL"
#' @param alt Text to be used for alt-text, if desired - defaults to "NULL"
#' @param ... additional arguments passed to  \code{\link{generic_column_bar}}
#' 
#' @return A static facet column (vertical bar) chart; based on facet_wrap()
#' @author Christy Lam
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' 
#' @examples
#' 
#' library(dplyr)
#' # Read in the example data and filter to 2020 Population by Race by County
#' df <- mode_share_example_data %>%
#'       filter(Category=="Population by Race" & Year==2020) %>%
#'       filter(Race !="Total")
#' 
#' my_facet <- static_facet_column_chart(t = df,
#'                 x = "Geography",
#'                 y = "share",
#'                 fill = "Geography",
#'                 facet = "Race",
#'                 moe = 'share_moe',
#'                 ncol = 4,
#'                 scales = "fixed",
#'                 color = "pgnobgy_5",
#'                 title = "Population by Race 2020",
#'                 subtitle = "For counties in the Central Puget Sound Region",
#'                 source = paste("Source: ACS 5-Year Estimates, table B03002",
#'                                "for King, Kitsap, Pierce and Snohomish counties.",
#'                                sep = "\n"))
#' 
#'  df2 <- mode_share_example_data %>%
#'       filter(Category=="Population by Race" & Year==2020) %>%
#'       filter(Geography != "Region", Race !="Total")
#' 
#'  my_facet2 <- static_facet_column_chart(t = df2,
#'                 x = "Race",
#'                 y = "share",
#'                 fill = "Race",
#'                 facet = "Geography",
#'                 ncol = 2,
#'                 moe = 'share_moe',
#'                 scales = "fixed",
#'                 color = "psrc_light",
#'                 title = "Population by Race 2020",
#'                 subtitle = "For counties in the Central Puget Sound Region",
#'                 source = paste("Source: ACS 5-Year Estimates, table B03002",
#'                                "for King, Kitsap, Pierce and Snohomish counties.",
#'                                sep = "\n"))
#'                                
#' df3 <- mode_share_example_data %>%
#'      filter(Category == "Population by Race" & Year %in% c(2010, 2020)) %>%
#'      mutate(Year = as.character(Year)) %>%
#'      filter(Race !="Total")
#'  
#' my_facet3 <- static_facet_column_chart(t = df3,
#'                 x = "Race",
#'                 y = "share",
#'                 fill = "Year",
#'                 facet = "Geography",
#'                 ncol = 2,
#'                 moe = 'share_moe',
#'                 scales = "fixed",
#'                 color = "psrc_light",
#'                 title = "Population by Race 2020",
#'                 subtitle = "For counties in the Central Puget Sound Region",
#'                 source = paste("Source: ACS 5-Year Estimates, table B03002",
#'                                "for King, Kitsap, Pierce and Snohomish counties.",
#'                                sep = "\n"))
#' @export
#'
static_facet_column_chart <- function(t,
                                      x, 
                                      y, 
                                      fill,
                                      pos = "dodge",
                                      facet, 
                                      moe = NULL,
                                      est = NULL,
                                      ncol = 3,
                                      scales = "free", 
                                      dec = 0, 
                                      color = "pgnobgy_5", 
                                      title = NULL, 
                                      subtitle = NULL,
                                      source = "",
                                      alt = NULL,
                                      ...) {
  
  confirm_fonts()
  
  l.clr <- "#4C4C4C"
  
  # Create a color palette from PSRC palette
  grps <- t %>% 
    dplyr::select(dplyr::all_of(fill)) %>% 
    unique() %>% 
    dplyr::pull()
  num.grps <- length(grps)
  l.colors <- unlist(psrcplot::psrc_colors[color])
  l.colors <- l.colors[1:num.grps]
  cols <- stats::setNames(l.colors, grps)
  
  
  # Estimate type determines the labels for the axis and the format of the value labels
  if(is.null(est)){est <- t %>% dplyr::pull(.data[[y]]) %>% est_type_default()}
  valfrmt <- est_number_formats(est)
  label <- est_label_formats(est)
  
  t %<>% dplyr::arrange(.data[[fill]]) # Factor ordering
  
  p <- ggplot2::ggplot(data = t,
                       ggplot2::aes(x = .data[[x]],
                                    y = formattable::digits(.data[[y]], digits = max(0, dec)),
                                    fill = .data[[fill]],
                                    group = .data[[fill]])) +
       ggplot2::geom_col(position = pos, na.rm = TRUE)
  
  # add labels
  p <- p +
    ggplot2::labs(title = title, 
                  subtitle = subtitle,
                  alt = alt,
                  caption = source,
                  x = NULL,
                  y = NULL) +
    ggplot2::scale_y_continuous(labels = label) +
    scale_fill_discrete_psrc(color)
  
  if(!(is.null(moe))) {
    p <- p + 
      ggplot2::geom_errorbar(ggplot2::aes(ymin = .data[[y]] - .data[[moe]], 
                   ymax = .data[[y]] + .data[[moe]]),
                             width = 0.2,
                             position = ggplot2::position_dodge(0.9))
  }
  
  x.vals <- length(unique(p$data[[x]]))
  # display x-axis tick values if another variable is introduced
  if(x != fill) {
    if(x.vals > 5) {
      # smaller font size and wrap/angle labels if there's a lot of x categories
      p <- p +
        ggplot2::scale_x_discrete(labels = wrap_labels_evenly(20))
      axis.text.x.value <- ggplot2::element_text(angle = 90, size = 7, vjust = 0.5, hjust=1)
    } else {
      axis.text.x.value <- ggplot2::element_text(size = 7)
    }
  } else if(stringr::str_detect(x, ".*[Y|y]ear.*") & stringr::str_detect(fill, ".*[Y|y]ear.*")) {
    # remove legend but keep x-axis labels for each year
    p <- p +
      ggplot2::scale_x_discrete(labels = wrap_labels_evenly(20)) +
      ggplot2::guides(fill = "none")
    axis.text.x.value <- ggplot2::element_text(size = 7)
    if(x.vals > 5) {
      axis.text.x.value <- ggplot2::element_text(angle = 90, size = 7, vjust = 0.5, hjust=1)
    }
  } else {
    axis.text.x.value <- ggplot2::element_blank()
  }
  
  # add facet and theme
  p <- p +
    ggplot2::facet_wrap(ggplot2::vars(.data[[facet]]), 
                        labeller = labeller_wrap_evenly(30),
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

