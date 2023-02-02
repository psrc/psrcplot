#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' @importFrom dplyr select all_of
NULL

is.date <- function(x) inherits(x, 'Date')

#' Parameters shared among other functions
#' 
#' @param fill The name of the variable you want the fill color of the bars to be based on
#' @param est Type for the numeric values - enter "percent", "currency" or "number", defaults to "percent"
#' @param dec Number of decimal points in labels - defaults to 0
#' @param color Name of color palette to use - generally defaults to "pgnobgy_5"
#' @param title Title to be used for chart, if desired - defaults to "NULL"
#' @param subtitle Sub-title to be used for chart, if desired - defaults to "NULL"
#' @param source Source reference as character string, if desired - defaults to blank
#' @name shared_params
NULL

#' Helper - return number formatting to match estimate type
#' 
#' @param est Type for the numeric values - enter "percent", "currency" or "number", defaults to "percent"
est_number_formats <- function(est){
  x<- data.frame(row.names=c("percent","currency","number"),
                 fac=c(100,1,1),
                 pfx=c("","$",""),
                 sfx=c("%","",""),
                 annot=c(0.01, 1, 1))
  y <- x[est,] %>% as.vector()
  return(y)
}

#' Helper - return label format function to match estimate type
#'
#' @param est Type for the numeric values - enter "percent", "currency" or "number", defaults to "percent"
est_label_formats <- function(est){
  lab <-    if(est=="percent") {scales::label_percent()
      }else if(est=="currency"){scales::label_dollar()
      }else if(est=="number")  {scales::label_comma()
      }
  return(lab)
}

#' Helper - make static ggplot object an interactive plotly object
#' 
#' @param p ggplot object
#' @param title Title to be used for chart, if desired - defaults to "NULL"
#' @param subtitle Sub-title to be used for chart, if desired - defaults to "NULL"
make_interactive <- function(p, title=NULL, subtitle=NULL){
  p <- p + ggplot2::theme(axis.title = ggplot2::element_blank())                                   # Remove Bar labels and axis titles
  m <- list(l = 50, r = 50, b = 200, t = 200, pad = 4)
  p <- plotly::ggplotly(p, tooltip=c("text"), autosize = T, margin = m)                            # Make Interactive
  p <- plotly::style(p, hoverlabel=list(font=list(family="Poppins", size=11, color="white")))      # Set Font for Hover-Text
  p <- plotly::layout(p, xaxis=list(tickfont=list(family="Poppins", size=11, color="#2f3030")))    # Format X-Axis
  p <- plotly::layout(p, yaxis=list(tickfont=list(family="Poppins", size=11, color="#2f3030")))    # Format Y-Axis
  p <- plotly::layout(p,                                                                     
          legend=list(orientation="h", xanchor="center", xref="container", x=0.5, y=-0.10,         # Turn on Legend
                      title="", font=list(family="Poppins", size=11, color="#2f3030"),
                      pad=list(b=50, t=50)), 
                      hovermode = "x")

  p <- plotly::layout(p, title= list(text = ""))                                                   # Remove Plotly Title
  
  if(!(is.null(title)) & !(is.null(subtitle))) {                                                   # If there is both title and subtitle

    p <- plotly::layout(p, 
            annotations = list(x= 0.03 , y = 1.10, text = title,                              # -- add the title, located high enough for room for subtitle
                               xref='paper', yref='paper', showarrow = FALSE, 
                               font = list(family="Poppins Black",size=14, color="#4C4C4C")))
    p <- plotly::layout(p, 
            annotations = list(x = 0.03, y = 1.05, text = subtitle,                             # -- then add the subtitle 
                               showarrow = FALSE, xref='paper', yref='paper', 
                               font=list(family="Poppins",size=12, color="#4C4C4C")))
  }else if(!(is.null(title)) & is.null(subtitle)) {                                                # If there is no Subtitle
    
    p <- plotly::layout(p, 
            annotations = list(x=0.03, y = 1.05, text = title,                                  # -- just add the title
                               xref='paper', yref='paper', showarrow = FALSE,
                               font = list(family="Poppins Black",size=14, color="#4C4C4C")))
  }
  return(p)
}

#' Helper - add source citation to plotly object
#' 
#' @param p plotly object
#' @param source Source reference as character string
add_citation <- function(p, source){
    if("plotly" %in% class(p) & !is.character(source)){
      p <- plotly::layout(p, 
              annotations = list(x= -0.05, y= -0.2, text=source,
                                 xref='paper', yref='paper', showarrow=FALSE, 
                                 xanchor='left', yanchor='auto', xshift=0, yshift=0,
                                 font = list(family="Poppins",size=10, color="#4C4C4C")))
    }
  return(p)
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
#' @param column_vs_bar "column": vertical bars or "bar": horizontal bars - defaults to "column" 
#' @param interactive Enable hover text and other interactive features - defaults to FALSE
#' @return static or interactive column or bar chart

generic_column_bar <- function(t, category_var, numeric_var, fill,
                               pos="dodge", est="percent", moe=NULL,
                               href=NULL, hrefnm=NULL, hrefcl=NULL,
                               title=NULL, subtitle=NULL, source="", alt=NULL,
                               category_label=NULL, numeric_label=NULL, 
                               axis_scale=1, column_vs_bar="column",
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
  
  # Figure out how many items are plotted on the category axis for use in Reference Line Titles
  num_cat_items <- t %>% select(all_of(category_var)) %>% dplyr::distinct() %>% dplyr::pull() %>% length()
  href_label_location <- ceiling(num_cat_items/2)
  
  # Estimate type determines the labels for the axis and the format of the value labels
  valfrmt <- est_number_formats(est)
  lab <- est_label_formats(est)
  
  # Create the Basic Static Chart
  c <- ggplot2::ggplot(data=t,
                       ggplot2::aes(x=if(column_vs_bar=="bar"){forcats::fct_rev(.data[[category_var]])}else{.data[[category_var]]},
                                    y=.data[[numeric_var]],
                                    text=paste0(.data[[fill]], ": ", valfrmt$pfx, prettyNum(round(.data[[numeric_var]] * valfrmt$fac, dec), big.mark = ","), valfrmt$sfx),
                                    fill = .data[[fill]],
                                    group=.data[[fill]])) +
    ggplot2::geom_bar(position=pos, stat="identity") +
    ggplot2::scale_fill_manual(values=cols)  +
    ggplot2::scale_y_continuous(labels=lab) +
    ggplot2::labs(title=title, subtitle = subtitle, caption = source, alt = alt, x = category_label, y = numeric_label) +
    psrcplot::psrc_style()
  
  # Add reference lines if they are included 
  if (!(is.null(href))) {
    c <- c + 
      ggplot2::geom_hline(yintercept = href, color=hrefcl, linetype='solid', linewidth=1, show.legend = FALSE) +
      ggplot2::annotate("text", x = href_label_location, y = href + valfrmt$annot, label = hrefnm, color=hrefcl)
  }
  
  # If there is a MOE value then error bars are added to the plot
  if (!(is.null(moe))) {
    c <- c + ggplot2::geom_errorbar(ggplot2::aes(ymin=.data[[numeric_var]]-.data[[moe]], 
                                                 ymax=.data[[numeric_var]]+.data[[moe]]), 
                                    width=0.2, position = ggplot2::position_dodge(0.9))
  }

  # Pivot for bar chart
  if(column_vs_bar=="bar"){
    c <- c + ggplot2::coord_flip() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), 
                   panel.grid.major.x = ggplot2::element_line(color="#cbcbcb"), 
                   axis.line.y = ggplot2::element_line(color="#cbcbcb"))
  }  
  else{
    c<- c+ ggplot2::scale_x_discrete(labels = scales::label_wrap(20))       # auto wrap text on column chart
  }
    
  # Add value labels if there is no error bar and remove the category-variable axis since we have the labels
  # placement of the labels is different between column and bar charts to look nicer
  if (is.null(moe) & interactive==FALSE & column_vs_bar =='column') {
    c <- c + ggplot2::geom_text(ggplot2::aes(x=.data[[category_var]],
                                             y=.data[[numeric_var]], 
                                             label=paste0(valfrmt$pfx, prettyNum(round(.data[[numeric_var]]* valfrmt$fac, dec), big.mark = ","), valfrmt$sfx)),
                                check_overlap = TRUE,
                                position = ggplot2::position_dodge(0.9),
                                vjust = -0.25,
                                size = 11*0.36,
                                family="Poppins")
    c <- c + ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                            panel.grid.major.y = ggplot2::element_blank(),
                            axis.line.y = ggplot2::element_line(color="#cbcbcb"))  
  }
  else if(is.null(moe) & interactive==FALSE & column_vs_bar =='bar'){
    c <- c + ggplot2::geom_text(ggplot2::aes(x=.data[[category_var]],
                                             y=.data[[numeric_var]], 
                                             label=paste0(valfrmt$pfx, prettyNum(round(.data[[numeric_var]]* valfrmt$fac, dec), big.mark = ","), valfrmt$sfx)),
                                check_overlap = TRUE,
                                position = ggplot2::position_dodge(0.9),
                                hjust = -0.25,
                                size = 11*0.36,
                                family="Poppins")
    c <- c + ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                            panel.grid.major.y = ggplot2::element_blank(),
                            axis.line.x = ggplot2::element_line(color="#cbcbcb"))     
  }
  

  
  # Remove legend if unneccesary
  if (num.grps == 1) {   
    c <- c + ggplot2::theme(legend.position = "none")  
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
NULL

#' @rdname column_bar_charts
#' @title Generate static column (vertical bar) chart
#' @export
static_column_chart <- function(t, x, y, fill, xlabel=NULL, ylabel=NULL, ...){
  c <- generic_column_bar(t=t, category_var=x, numeric_var=y, fill=fill,
                          category_label=xlabel, numeric_label=ylabel,
                          axis_scale=1.1, column_vs_bar="column", ...)
  return(c)
}

#' @rdname column_bar_charts
#' @title Generate static horizontal bar chart
#' @export
static_bar_chart <- function(t, x, y, fill, xlabel=NULL, ylabel=NULL, ...){
  c <- generic_column_bar(t=t, category_var=y, numeric_var=x, fill=fill,
                          category_label=ylabel, numeric_label=xlabel,
                          axis_scale=1.25, column_vs_bar="bar", ...)
  return(c)
}

#' @rdname column_bar_charts
#' @title Generate interactive column (vertical bar) chart
#' @export
interactive_column_chart <- function(t, x, y, fill, xlabel=NULL, ylabel=NULL, ...){
  c <- generic_column_bar(t=t, category_var=x, numeric_var=y, fill=fill,
                          category_label=xlabel, numeric_label=ylabel,
                          axis_scale=1.1, column_vs_bar="column", interactive=TRUE, ...)
  return(c)
}

#' @rdname column_bar_charts
#' @title Generate interactive horizontal bar chart
#' @export
interactive_bar_chart <- function(t, x, y, fill, xlabel=NULL, ylabel=NULL, ...){
  c <- generic_column_bar(t=t, category_var=y, numeric_var=x, fill=fill,
                          category_label=ylabel, numeric_label=xlabel,
                          axis_scale=1.25, column_vs_bar="bar", interactive=TRUE, ...)
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
#' @param href A value to be used for a horizontal reference line - default is "NULL"
#' @param hrefcl A color to be used for horizontal reference lines - default is "NULL"
#' @param alt Text to be used for alt-text, if desired - defaults to "NULL"
#' @return A static facet column (vertical bar) chart; based on facet_wrap()
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
#'                 href = 'Region',
#'                 hrefcl = 'black',
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
#'                                        x = "Race",
#'                                        y = "share",
#'                                        fill = "Year",
#'                                        facet = "Geography",
#'                                        ncol = 2,
#'                                        moe = 'share_moe',
#'                                        scales = "fixed",
#'                                        color = "psrc_light",
#'                                        title = "Population by Race 2020",
#'                                        subtitle = "For counties in the Central Puget Sound Region",
#'                                        source = paste("Source: ACS 5-Year Estimates, table B03002",
#'                                                       "for King, Kitsap, Pierce and Snohomish counties.",
#'                                                       sep = "\n"))
#' @export
#'
static_facet_column_chart <- function(t,
                                      x, 
                                      y, 
                                      fill,
                                      pos = "dodge",
                                      facet, 
                                      moe = NULL,
                                      href = NULL,
                                      hrefcl = NULL,
                                      est = "percent",
                                      ncol = 3,
                                      scales = "free", 
                                      dec = 0, 
                                      color = "pgnobgy_5", 
                                      title = NULL, 
                                      subtitle = NULL,
                                      source = "",
                                      alt = NULL) {
  
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
  valfrmt <- est_number_formats(est)
  label <- est_label_formats(est)
  
  if(!(is.null(href))) {
    # separate column data from hline data
    tcol <- t %>% 
      filter(.data[[x]] != eval(href))
    
    thline <- t %>% 
      filter(.data[[x]] == eval(href))
    
    p <- ggplot2::ggplot(data = tcol,
                         ggplot2::aes(x = .data[[x]],
                                      y = .data[[y]],
                                      fill = .data[[fill]],
                                      group = .data[[fill]])) +
      ggplot2::geom_col(position = pos) +
      ggplot2::geom_hline(data = thline, 
                          ggplot2::aes(yintercept = .data[[y]], color = .data[[x]]),
                          alpha = .3,
                          linetype = 'solid', 
                          linewidth = 1)  +
      ggplot2::scale_color_manual(values = c('black'))
  } else {
    # no horizontal reference line
    p <- ggplot2::ggplot(data = t,
                         ggplot2::aes(x = .data[[x]],
                                      y = .data[[y]],
                                      fill = .data[[fill]],
                                      group = .data[[fill]])) +
      ggplot2::geom_col(position = pos)
  }
  
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
  
  # display x-axis tick values if another variable is introduced
  if(x != fill) {
    x.vals <- length(unique(p$data[[x]]))
    if(x.vals > 5) {
      # smaller font size and wrap/angle labels if there's a lot of x categories
      p <- p +
        ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20))
      axis.text.x.value <- ggplot2::element_text(angle = 90, size = 7, vjust = 0.5, hjust=1)
    } else {
      axis.text.x.value <- ggplot2::element_text(size = 7)
    }
  } else {
    axis.text.x.value <- ggplot2::element_blank()
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
#' @param interactive Enable hover text and other interactive features - defaults to FALSE
#' @return line chart
#' 
generic_line <- function(t, x, y, fill, 
                         est="percent", dec=0, dform="%b-%Y",  
                         breaks=NULL, lwidth=1, color="gnbopgy_5",
                         title=NULL, subtitle=NULL, source="",
                         interactive=FALSE){
  
  confirm_fonts() 
  
  # Create a color palette from PSRC palette
  grps <- t %>% select(all_of(fill)) %>% unique() %>% dplyr::pull()
  num.grps <- length(grps)
  l.colors <- unlist(psrcplot::psrc_colors[color])
  l.colors <- l.colors[1:num.grps]
  cols <- stats::setNames(l.colors, grps)
  
  # Estimate type determines the labels for the axis and the format of the value labels
  valfrmt <- est_number_formats(est)
  lab <- est_label_formats(est)
  xtype_date <- t %>% select(all_of(x)) %>% is.date()
  
  c <- ggplot2::ggplot(data=t, 
                       ggplot2::aes(x=.data[[x]],
                                    y=.data[[y]],
                                    text=paste0(.data[[fill]], ": ", valfrmt$pfx, prettyNum(round(.data[[y]] * valfrmt$fac, dec), big.mark = ","), valfrmt$sfx),
                                    group=.data[[fill]]))  + 
    ggplot2::geom_line(ggplot2::aes(color=.data[[fill]]), linewidth=lwidth, linejoin = "round") +
    ggplot2::scale_y_continuous(labels = lab) +
    ggplot2::scale_color_manual(values=cols)  +
    ggplot2::labs(title=title, subtitle=subtitle, caption=source) +
    psrc_style()

  if (xtype_date==TRUE){
    c <- c + ggplot2::scale_x_date(labels = scales::date_format(dform))
  }else{
    c <- c + ggplot2::geom_point(ggplot2::aes(color=.data[[fill]])) +	
      ggplot2::scale_x_discrete(breaks=breaks)
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
