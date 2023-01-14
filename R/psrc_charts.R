#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
NULL

#' Parameters shared among other functions
#' 
#' @param est Type for the numeric values - enter "percent", "currency" or "number", defaults to "percent"
#' @param fill The name of the variable you want the fill color of the bars to be based on
#' @param dec Number of decimal points in labels - defaults to 0
#' @param color Name of color palette to use - generally defaults to "psrc_dark"
#' @param title Title to be used for chart, if desired - defaults to "NULL"
#' @param subtitle Sub-title to be used for chart, if desired - defaults to "NULL"
#' @param interactive Enable hover text and other interactive features - defaults to FALSE
#' @param source Source reference as character string, if desired - defaults to blank
#' @name shared_params
NULL

#' Helper - add source citation to ggplot object
#' 
#' @param chart ggplot or plotly object
#' @param source Source reference as character string
add_citation <- function(chart, source){
  if(!is.character(source)){
    warning("Source parameter must be provided as text.")
  }else{
    if("plotly" %in% class(chart)){
      c <- plotly::layout(chart, annotations = list(x= -0.05, y= -0.2, text=source,
                                                xref='paper', yref='paper', showarrow=FALSE, 
                                                xanchor='left', yanchor='auto', xshift=0, yshift=0,
                                                font = list(family="Poppins",size=10, color="#4C4C4C")))    
    }else{
      # Do something here for ggplot2 objects
    }
  }
  return(c)
}

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
#' @param href A list of values to be used for any horizontal reference lines - default is "NULL"
#' @param hrefnm A list of names to be used for any horizontal reference lines that is equal length to the number of lines - default is "NULL"
#' @param hrefcl A list of colors to be used for any horizontal reference lines that is equal length to the number of lines - default is "NULL"
#' @param alt Text to be used for alt-text, if desired - defaults to "NULL"
#' @param category_label category-axis title to be used for chart, if desired - defaults to "NULL"
#' @param numeric_label numeric-axis title to be used for chart, if desired - defaults to "NULL"
#' @param axis_scale Enlarge or reduce the axis weight - defaults to 1
#' @param orientation "column": vertical bars or "bar": horizontal bars - defaults to "column"
#' @return static or interactive column or bar chart

generic_column_bar <- function(t, category_var, numeric_var, fill,
                               pos="dodge", est="percent", moe=NULL,
                               href=NULL, hrefnm=NULL, hrefcl=NULL,
                               title=NULL, subtitle=NULL, source="", alt=NULL,
                               category_label=NULL, numeric_label=NULL, 
                               axis_scale=1, orientation="column",
                               dec = 0, color="psrc_dark",
                               interactive=FALSE){
  
  confirm_fonts()
  
  # Determine the Maximum Value to ensure bar labels are not cut-off
  max_item <- t %>% dplyr::select(.data[[numeric_var]]) %>% dplyr::pull() %>% max()
  
  # Create a color palette from PSRC palette
  grps <- t %>% dplyr::select(.data[[fill]]) %>% unique() %>% dplyr::pull()
  num.grps <- length(grps)
  l.colors <- unlist(psrcplot::psrc_colors[color])
  l.colors <- l.colors[1:num.grps]
  cols <- stats::setNames(l.colors, grps)
  
  # Figure out how many items are plotted on the category axis for use in Reference Line Titles
  num_cat_items <- t %>% dplyr::select(.data[[category_var]]) %>% dplyr::distinct() %>% dplyr::pull() %>% length()
  href_label_location <- ceiling(num_cat_items/2)
  
  # Estimate type determines the labels for the axis and the format of the number bar labels
  if (est=="percent") {
    scale_max <- max_item * axis_scale
    fac=100
    p=""
    s="%"
    lab=scales::label_percent()
    annot = 0.01
    
  } else if (est=="currency") {
    scale_max <- max_item * axis_scale
    fac=1
    p="$"
    s=""
    lab=scales::label_dollar()
    annot = 1
    
  } else {
    scale_max <- max_item * axis_scale
    fac=1
    p=""
    s=""
    lab=scales::label_comma()
    annot = 1
  }
  
  # Create the Basic Static Chart
  c <- ggplot2::ggplot(data=t,
                       ggplot2::aes(x=if(orientation=="bar"){forcats::fct_rev(.data[[category_var]])}else{.data[[category_var]]},
                                    y=.data[[numeric_var]],
                                    text=paste0(.data[[fill]], ": ", p, prettyNum(round(.data[[numeric_var]]*fac, dec), big.mark = ","),s),
                                    fill = .data[[fill]],
                                    group=.data[[fill]])) +
    ggplot2::geom_bar(position=pos, stat="identity") +
    ggplot2::scale_fill_manual(values=cols)  +
    ggplot2::scale_y_continuous(labels = lab, limits = c(0, scale_max), expand = c(0, 0)) +
    ggplot2::labs(title=title, subtitle = subtitle, caption = source, alt = alt, x = category_label, y = numeric_label) +
    psrcplot::psrc_style()
  
  # Add reference lines if they are included 
  if (!(is.null(href))) {
    c <- c + 
      ggplot2::geom_hline(yintercept = href, color=hrefcl, linetype='solid', linewidth=1, show.legend = FALSE) +
      ggplot2::annotate("text", x = href_label_location, y = href+annot, label = hrefnm, color=hrefcl)
  }
  
  # Pivot for bar chart
  if(orientation=="bar"){
    c <- c + ggplot2::coord_flip()
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), 
                   panel.grid.major.x = ggplot2::element_line(color="#cbcbcb"), 
                   axis.line.y = ggplot2::element_line(color="#cbcbcb"))
  }
  
  # If there is a MOE value then error bars are added to the plot
  if (!(is.null(moe))) {
    c <- c + ggplot2::geom_errorbar(ggplot2::aes(ymin=.data[[numeric_var]]-.data[[moe]], ymax=.data[[numeric_var]]+.data[[moe]]),width=0.2, position = ggplot2::position_dodge(0.9))
  }
  
  # Add Bar Labels if there is no Error Bar and remove the category-variable axis since we have the labels
  if (is.null(moe)) {
    c <- c + ggplot2::geom_text(ggplot2::aes(x=.data[[category_var]],
                                             y=.data[[numeric_var]], 
                                             label=paste0(p,prettyNum(round(.data[[numeric_var]]*fac,dec), big.mark = ","),s)),
                                check_overlap = TRUE,
                                position = ggplot2::position_dodge(0.9),
                                vjust = -0.25,
                                size = 11*0.36,
                                family="Poppins") +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     panel.grid.major.y = ggplot2::element_blank(),
                     axis.line.x = ggplot2::element_line(color="#cbcbcb"))
  }
  
  # Remove legend if unneccesary
  if (num.grps == 1) {   
    c <- c + ggplot2::theme(legend.position = "none")  
  }
  
  # Interactivity
  if(interactive==TRUE){
    # Remove Bar labels and axis titles
    c <- c + ggplot2::theme(axis.title = ggplot2::element_blank())
    
    # Make Interactive
    m <- list(l = 50, r = 50, b = 200, t = 200, pad = 4)
    c <- plotly::ggplotly(c, tooltip = c("text"), autosize = T, margin = m)
    
    # Set Font for Hover-Text
    c <- plotly::style(c, hoverlabel = list(font=list(family="Poppins",size=11, color="white")))
    
    # Format X-Axis
    c <- plotly::layout(c, xaxis = list(tickfont = list(family="Poppins", size=11, color="#2f3030")))
    
    # Format Y-Axis
    c <- plotly::layout(c, yaxis = list(tickfont = list(family="Poppins", size=11, color="#2f3030")))
    
    if(orientation=="bar"){
      # Turn on Legend  
      c <- plotly::layout(c, legend = list(orientation = "h", xanchor = "center", xref="container", x = 0.5, y = -0.10,   
                                           title = "",  
                                           font = list(family="Poppins", size=11, color="#2f3030"), 
                                           pad = list(b=50, t=50)))
      
      # Update Plotly Title 
      c <- plotly::layout(c, title= list(text = title,  
                                         font = list(family="Poppins Black",size=12, color="#4C4C4C"),  
                                         x=0.02,  
                                         xref="container"))
    }else{
      
      # Turn on Legend
      c <- plotly::layout(c, legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.10, 
                                           title = "", 
                                           font = list(family="Poppins", size=11, color="#2f3030"),
                                           pad = list(b=50, t=50)))
      
      # Remove Plotly Title
      c <- plotly::layout(c, title= list(text = ""))
      
      # Chart Title if there is also a Subtitle
      if(!(is.null(title)) & !(is.null(subtitle))) {
        
        # Remove Plotly Title
        c <- plotly::layout(c, title= list(text = ""))
        
        # Add the title and put it high enough for room for subtitle
        c <- plotly::layout(c, annotations = list(x = -0.05, y = 1.10, text = title,
                                                  xref='paper', yref='paper', showarrow = F, 
                                                  font = list(family="Poppins Black",size=14, color="#4C4C4C")))
        # Add the subtitle 
        c <- plotly::layout(c, annotations = list(x = -0.05, y = 1.05, text = subtitle, 
                                                  showarrow = F, xref='paper', yref='paper', 
                                                  font=list(family="Poppins",size=12, color="#4C4C4C")))
      }
      
      # Chart Title if there is no Subtitle
      if(!(is.null(title)) & is.null(subtitle)) {
        
        # Add the title
        c <- plotly::layout(c, annotations = list(x = -0.05, y = 1.05, text = title,
                                                  xref='paper', yref='paper', showarrow = F,
                                                  font = list(family="Poppins Black",size=14, color="#4C4C4C")))
      }
    }
  }
  # Turn on Source if Provided
  if(!source==""){
    c <- add_citation(c, source)
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
NULL

#' @rdname column_bar_charts
#' @title Generate static column (vertical bar) chart
#' @export
static_column_chart <- function(t, x, y, fill, xlabel=NULL, ylabel=NULL, ...){
  c <- generic_column_bar(t=t, category_var=x, numeric_var=y, fill=fill,
                          category_label=xlabel, numeric_label=ylabel,
                          axis_scale=1.1, orientation="column")
  return(c)
}

#' @rdname column_bar_charts
#' @title Generate static horizontal bar chart
#' @export
static_bar_chart <- function(t, x, y, fill, xlabel=NULL, ylabel=NULL, ...){
  c <- generic_column_bar(t=t, category_var=y, numeric_var=x, fill=fill,
                          category_label=ylabel, numeric_label=xlabel,
                          axis_scale=1.25, orientation="bar")
  return(c)
}

#' @rdname column_bar_charts
#' @title Generate interactive column (vertical bar) chart
#' @export
interactive_column_chart <- function(t, x, y, fill, xlabel=NULL, ylabel=NULL, ...){
  c <- generic_column_bar(t=t, category_var=x, numeric_var=y, fill=fill,
                          category_label=xlabel, numeric_label=ylabel,
                          axis_scale=1.1, orientation="column", interactive=TRUE)
  return(c)
}

#' @rdname column_bar_charts
#' @title Generate interactive horizontal bar chart
#' @export
interactive_bar_chart <- function(t, x, y, fill, xlabel=NULL, ylabel=NULL, ...){
  c <- generic_column_bar(t=t, category_var=y, numeric_var=x, 
                          category_label=ylabel, numeric_label=xlabel,
                          axis_scale=1.25, orientation="bar", interactive=TRUE)
  return(c)
}

#' Create PSRC Facet Bar Charts
#'
#' This function allows you to create facet bar charts.
#' @inheritParams shared_params
#' @param t A tibble or dataframe in long form for plotting
#' @param x The name of the variable you want plotted on the X-Axis
#' @param y The name of the variable you want plotted on the Y-Axis
#' @param g The name of the variable to be the facets
#' @param scales Value for axis in facets, either "fixed" or "free" - defaults to "free"
#' @param facet Value for the number of columns in your facet - defaults to 3
#' @param lpos Position for the bar labels of "above" or "within" - defaults to "above"
#' @param moe The name of the variable to be used for error bars, if desired - default to "NULL"
#' @return facet bar chart that is either static or interactive depending on choice
#' 
#' @examples
#' 
#' library(dplyr)
#' # Read in the example data and filter to 2020 Population by Race by County
#' df <- psrcplot::mode_share_example_data %>% 
#'       filter(Category=="Population by Race" & Year==2020) %>%
#'       filter(Geography!="Region" & Race!="Total")
#' 
#' # Create a facet chart for population by race using counties as the facet
#' my.chart <- create_facet_bar_chart(t=df, x="Race", y="share", 
#'                                    fill="Race", g="Geography", 
#'                                    facet=2, scales="fixed")
#' 
#' @export
#'
create_facet_bar_chart <- function(t, x, y, fill, g, moe=NULL, est="percent", scales="free", facet=3, dec = 0, lpos="above", color="psrc_dark", title=NULL, subtitle=NULL, interactive=FALSE) {
  
  confirm_fonts() 
  
  l.clr ="#4C4C4C"
  l.sz=4
  pos="dodge"
  
  if (lpos == "above") {
    l = -0.5
  } else {l = 0.5}
  
  if (est=="percent") {
    fac=100
    p=""
    s="%"
    lab=scales::label_percent()
    
  } else if (est=="currency") {
    fac=1
    p="$"
    s=""
    lab=scales::label_dollar()
    
  } else {
    fac=1
    p=""
    s=""
    lab=scales::label_comma()
  }
  
  if (interactive == TRUE) {
    
    c <- ggplot2::ggplot(data=t,
                         ggplot2::aes(y=.data[[y]],
                                      x=.data[[x]],
                                      fill=.data[[fill]],
                                      group=.data[[y]],
                                      tooltip=paste0(.data[[x]], " ", .data[[fill]],": ", p, prettyNum(round(.data[[y]]*fac,dec), big.mark = ","),s),
                                      data_id=.data[[y]])) +
      ggiraph::geom_bar_interactive(position=pos, stat="identity") +
      ggplot2::ggtitle(title, subtitle = subtitle) +
      ggplot2::scale_y_continuous(labels = lab) +
      scale_fill_discrete_psrc(color)
    
    if (!(is.null(moe))) {
      c <- c + ggplot2::geom_errorbar(ggplot2::aes(ymin=.data[[y]]-.data[[moe]], ymax=.data[[y]]+.data[[moe]]),width=0.2, position = ggplot2::position_dodge(0.9))
    }
    
    c <- c + 
      ggplot2::facet_wrap(ggplot2::vars(get(eval(g))), scales=scales, ncol=facet) +
      psrc_style() +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_text(size=12,color="#4C4C4C"))
    
    c <- ggiraph::girafe(ggobj = c)
    
  } else {
    
    c <- ggplot2::ggplot(data=t,
                         ggplot2::aes(y=.data[[y]],
                                      x=.data[[x]],
                                      fill = .data[[fill]])) +
      ggplot2::geom_bar(position=pos, stat="identity") +
      ggplot2::ggtitle(title, subtitle = subtitle) +
      ggplot2::scale_y_continuous(labels = lab) +
      scale_fill_discrete_psrc(color)
    
    if (!(is.null(moe))) {
      c <- c + ggplot2::geom_errorbar(ggplot2::aes(ymin=.data[[y]]-.data[[moe]], ymax=.data[[y]]+.data[[moe]]),width=0.2, position = ggplot2::position_dodge(0.9))
    }
    
    c <- c + 
      ggplot2::facet_wrap(ggplot2::vars(get(eval(g))), scales=scales, ncol=facet) +
      psrc_style() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size=10,color="#4C4C4C"),
                     axis.text.y = ggplot2::element_text(size=12,color="#4C4C4C"))
  }
  
  return(c)
}

#' Create PSRC TreeMap Chart
#'
#' This function allows you to create treemap charts.
#' @inheritParams shared_params
#' @param t A tibble or dataframe in long form for plotting
#' @param s The name of the variable with the value you want to use to size the bars
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
  
  tot <- t %>% dplyr::select(.data[[s]]) %>% dplyr::pull() %>% sum()
  t <- t %>% dplyr::mutate(total_share = .data[[s]]/tot)
  
  # Estimate type determines the labels
  if (est=="percent") {
    fac=100
    p=""
    s="%"
    
  } else if (est=="currency") {
    fac=1
    p="$"
    s=""
    
  } else {
    fac=1
    p=""
    s=""
  }
  
  if (est=="percent") {
    c <- ggplot2::ggplot(t,
                         ggplot2::aes(area = .data[[s]],
                                      fill = .data[[fill]], 
                                      label = paste(.data[[fill]], 
                                                    paste0(p, prettyNum(round(.data[[s]]*fac,dec), big.mark = ","), s),
                                                    sep = "\n"))) +
      treemapify::geom_treemap() +
      treemapify::geom_treemap_text(colour = "white",
                                    place = "centre",
                                    size = 28) +
      psrc_style() +
      ggplot2::theme(legend.position = "none") +
      scale_fill_discrete_psrc(color) +
      ggplot2::ggtitle(title, subtitle = subtitle)
    
  } else {
    
    c <- ggplot2::ggplot(t,
                         ggplot2::aes(area = .data[[s]],
                                      fill = .data[[fill]], 
                                      label = paste(.data[[fill]], 
                                                    paste0(p, prettyNum(round(.data[[s]]*fac,dec), big.mark = ","), s),
                                                    paste0(prettyNum(round(.data$total_share*100,0), big.mark = ","), "%"), 
                                                    sep = "\n"))) +
      treemapify::geom_treemap() +
      treemapify::geom_treemap_text(colour = "white",
                                    place = "centre",
                                    size = 28) +
      psrc_style() +
      ggplot2::theme(legend.position = "none") +
      scale_fill_discrete_psrc(color) +
      ggplot2::ggtitle(title, subtitle = subtitle)
  }
  
  return(c)
}

#' Create PSRC Bubble Chart
#'
#' This function allows you to create a bubble charts.
#' 
#' @inheritParams shared_params
#' @param t A tibble or dataframe in long form for plotting
#' @param x The name of the variable you want plotted on the X-Axis
#' @param y The name of the variable you want plotted on the Y-Axis
#' @param s The name of the variable used to size the bubbles
#' @return bubble chart
#' 
#' @export
#'
create_bubble_chart <- function(t, x, y, fill, s, color="psrc_light", title=NULL, subtitle=NULL) {
  
  confirm_fonts() 
  
  # Create a color palette from PSRC palette
  grps <- t %>% dplyr::select(.data[[fill]]) %>% unique() %>% dplyr::pull()
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


#' Static Line Chart
#'
#' This function allows you to create a line chart.
#' 
#' @inheritParams shared_params
#' @param t A tibble or dataframe in long form for plotting
#' @param x The name of the variable you want plotted on the X-Axis
#' @param y The name of the variable you want plotted on the Y-Axis
#' @param xtype Type of values for the X-Axis either "Continuous" or "Date", defaults to "Date"
#' @param dform Format for Date values 
#' @param breaks Break points to use if using a continuous scale, defaults to NULL
#' @param lwidth Width of lines, defaults to 1
#' @return line chart
#' 
#' @export
#'
create.line.chart <- function(t, x, y, fill, title=NULL, subtitle=NULL, est="number", dec=0, xtype="Date", dform="%b-%Y", breaks=NULL, lwidth=1, color="psrc_light") {
  
  confirm_fonts() 
  
  grps <- t %>% dplyr::select(.data[[fill]]) %>% unique() %>% dplyr::pull()
  num.grps <- length(grps)
  l.colors <- unlist(psrc_colors[color])
  l.colors <- l.colors[1:num.grps]
  cols <- stats::setNames(l.colors, grps)
  
  # Estimate type determines the labels for the axis and the format of the number for the hover-text
  if (est=="percent") {
    fac=100
    p=""
    s="%"
    lab=scales::label_percent()
    
  } else if (est=="currency") {
    fac=1
    p="$"
    s=""
    lab=scales::label_dollar()
    
  } else {
    fac=1
    p=""
    s=""
    lab=scales::label_comma()
  }
    
  if (xtype=="Continuous") {
    g <- ggplot2::ggplot(data=t, 
                         ggplot2::aes(x=.data[[x]],
                                      y=.data[[y]], 
                                      group=.data[[fill]]))  + 
      ggplot2::geom_line(ggplot2::aes(color=.data[[fill]]), size=lwidth, linejoin = "round") +
      ggplot2::geom_point(ggplot2::aes(color=.data[[fill]]))+
      ggplot2::scale_x_discrete(breaks=breaks) +
      ggplot2::scale_y_continuous(labels = lab) +
      ggplot2::scale_color_manual(values=cols)  +
      ggplot2::ggtitle(title, subtitle = subtitle) +
      psrc_style()
    
  } else {
    
    g <- ggplot2::ggplot(data=t, 
                         ggplot2::aes(x=.data[[x]],
                                      y=.data[[y]], 
                                      group=.data[[fill]]))  + 
      ggplot2::geom_line(ggplot2::aes(color=.data[[fill]]), size = lwidth, linejoin = "round") +
      ggplot2::scale_x_date(labels = scales::date_format(dform)) +
      ggplot2::scale_y_continuous(labels = lab) +
      ggplot2::scale_color_manual(values=cols)  +
      ggplot2::ggtitle(title, subtitle = subtitle) +
      psrc_style()
    
  }
  
  return(g)
}

#' Interactive Line Chart
#'
#' This function allows you to create a line chart.
#' 
#' @inheritParams shared_params
#' @param t A tibble or dataframe in long form for plotting
#' @param x The name of the variable you want plotted on the X-Axis
#' @param y The name of the variable you want plotted on the Y-Axis
#' @param xtype Type of values for the X-Axis either "Continuous" or "Date", defaults to "Date"
#' @param dform Format for Date values 
#' @param breaks Break points to use if using a continuous scale, defaults to NULL
#' @param lwidth Width of lines, defaults to 1
#' @return line chart
#' 
#' @export
#'
interactive_line_chart <- function(t, x, y, fill, 
                                   title="", 
                                   est="number", dec=0, 
                                   xtype="Date", dform="%b-%Y", 
                                   breaks=NULL, lwidth=1, color="psrc_light") {
  
  confirm_fonts() 
  
  # Create a color palette from PSRC palette
  grps <- t %>% dplyr::select(.data[[fill]]) %>% unique() %>% dplyr::pull()
  num.grps <- length(grps)
  l.colors <- unlist(psrcplot::psrc_colors[color])
  l.colors <- l.colors[1:num.grps]
  cols <- stats::setNames(l.colors, grps)
  
  # Estimate type determines the labels for the axis and the format of the number for the hover-text
  if (est=="percent") {
    fac=100
    p=""
    s="%"
    lab=scales::label_percent()
    annot = 0.01
    
  } else if (est=="currency") {
    fac=1
    p="$"
    s=""
    lab=scales::label_dollar()
    annot = 1
    
  } else {
    fac=1
    p=""
    s=""
    lab=scales::label_comma()
    annot = 1
  }
  
  if (xtype=="Continuous") {
    c <- ggplot2::ggplot(data=t, 
                         ggplot2::aes(x=.data[[x]],
                                      y=.data[[y]],
                                      text=paste0(.data[[fill]], ": ", p, prettyNum(round(.data[[y]]*fac, dec), big.mark = ","),s),
                                      group=.data[[fill]]))  + 
      ggplot2::geom_line(ggplot2::aes(color=.data[[fill]]), size=lwidth, linejoin = "round") +
      ggplot2::geom_point(ggplot2::aes(color=.data[[fill]]))+
      ggplot2::scale_x_discrete(breaks=breaks) +
      ggplot2::scale_y_continuous(labels = lab) +
      ggplot2::scale_color_manual(values=cols)  +
      ggplot2::labs(title=title) +
      psrc_style()
    
  } else {
    
    c <- ggplot2::ggplot(data=t, 
                         ggplot2::aes(x=.data[[x]],
                                      y=.data[[y]], 
                                      text=paste0(.data[[fill]], ": ", p, prettyNum(round(.data[[y]]*fac, dec), big.mark = ","),s),
                                      group=.data[[fill]]))  +
      ggplot2::geom_line(ggplot2::aes(color=.data[[fill]]), size = lwidth, linejoin = "round") +
      ggplot2::scale_x_date(labels = scales::date_format(dform)) +
      ggplot2::scale_y_continuous(labels = lab) +
      ggplot2::scale_color_manual(values=cols)  +
      ggplot2::labs(title=title) +
      psrc_style()
    
  }
  
  # Remove Bar labels and axis titles
  c <- c + ggplot2::theme(axis.title = ggplot2::element_blank())
  
  # Make Interactive
  m <- list(l = 50, r = 50, b = 200, t = 200, pad = 4)
  c <- plotly::ggplotly(c, tooltip = c("text"), autosize = T, margin = m)
  
  # Set Font for Hover-Text
  c <- plotly::style(c, hoverlabel = list(font=list(family="Poppins",size=11, color="white")))
  
  # Format X-Axis
  c <- plotly::layout(c, xaxis = list(tickfont = list(family="Poppins", size=11, color="#2f3030")))
  
  # Format Y-Axis
  c <- plotly::layout(c, yaxis = list(tickfont = list(family="Poppins", size=11, color="#2f3030")))
  
  # Turn on Legend
  c <- plotly::layout(c, legend = list(orientation = "h", xanchor = "center", xref="container", x = 0.5, y = -0.10, 
                                       title = "", 
                                       font = list(family="Poppins", size=11, color="#2f3030"),
                                       pad = list(b=50, t=50)), 
                      hovermode = "x")
  
  # Update Plotly Title
  c <- plotly::layout(c, title= list(text = title, 
                                     font = list(family="Poppins Black",size=12, color="#4C4C4C"),
                                     x=0.02,
                                     xref="container"))
  return(c)
}
