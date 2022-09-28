#' Create PSRC Facet Bar Charts
#'
#' This function allows you to create facet bar charts.
#' @param t A tibble or dataframe in long form for plotting
#' @param w.x The name of the variable you want plotted on the X-Axis
#' @param w.y The name of the variable you want plotted on the Y-Axis
#' @param f The name of the variable you want the fill color of the bars to be based on
#' @param g The name of the variable to be the facets
#' @param est.type Type for the Y values - enter "percent", "currency" or "number", defaults to "percent"
#' @param w.scales Value for axis in facets, either "fixed" or "free" - defaults to "free"
#' @param w.facet Value for the number of columns in your facet - defaults to 3
#' @param l.pos Position for the bar labels of "above" or "within" - defaults to "above"
#' @param w.dec Number of decimal points in labels - defaults to 0
#' @param w.color Name of color palette to use - defaults to "psrc_dark"
#' @param w.moe The name of the variable to be used for error bars, if desired - default to "NULL"
#' @param w.title Title to be used for chart, if desired - defaults to "NULL"
#' @param w.sub.title Sub-title to be used for chart, if desired - defaults to "NULL"
#' @param w.interactive Enable hover text and other interactive features - defaults to "no"
#' @return facet bar chart that is either static or interactive depending on choice
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' 
#' library(dplyr)
#' # Read in the example data and filter to 2020 Population by Race by County
#' df <- read.csv(system.file('extdata', 'example_data.csv', package='psrcplot')) %>% 
#'       filter(Category=="Population by Race" & Year==2020) %>%
#'       filter(Geography!="Region" & Race!="Total")
#' 
#' # Create a facet chart for population by race using counties as the facet
#' my.chart <- create_facet_bar_chart(t=df, w.x="Race", w.y="share", 
#'                                    f="Race", g="Geography", 
#'                                    w.facet=2, w.scales="fixed")
#' 
#' @export
#'
create_facet_bar_chart <- function(t, w.x, w.y, f, g, w.moe=NULL, est.type="percent", w.scales="free", w.facet=3, w.dec = 0, l.pos="above", w.color="psrc_dark", w.title=NULL, w.sub.title=NULL, w.interactive="no") {
  
  l.clr ="#4C4C4C"
  l.sz=4
  w.pos="dodge"
  
  if (l.pos == "above") {
    l = -0.5
  } else {l = 0.5}
  
  if (est.type=="percent") {
    w.factor=100
    p=""
    s="%"
    w.label=scales::label_percent()
    
  } else if (est.type=="currency") {
    w.factor=1
    p="$"
    s=""
    w.label=scales::label_dollar()
    
  } else {
    w.factor=1
    p=""
    s=""
    w.label=scales::label_comma()
  }
  
  if (w.interactive == 'yes') {
    
    c <- ggplot2::ggplot(data=t,
                         ggplot2::aes(y=get(eval(w.y)),
                                      x=get(eval(w.x)),
                                      fill = get(eval(f)),
                                      tooltip=paste0(get(eval(w.x)), " ", get(eval(f)),": ", p, prettyNum(round(get(eval(w.y))*w.factor,w.dec), big.mark = ","),s),
                                      data_id=get(eval(w.y)))) +
      ggiraph::geom_bar_interactive(position=w.pos, stat="identity") +
      ggplot2::ggtitle(w.title, subtitle = w.sub.title) +
      ggplot2::scale_y_continuous(labels = w.label) +
      scale_fill_discrete_psrc(w.color)
    
    if (!(is.null(w.moe))) {
      c <- c + ggplot2::geom_errorbar(ggplot2::aes(ymin=get(eval(w.y))-get(eval(w.moe)), ymax=get(eval(w.y))+get(eval(w.moe))),width=0.2, position = ggplot2::position_dodge(0.9))
    }
    
    c <- c + 
      ggplot2::facet_wrap(ggplot2::vars(get(eval(g))), scales=w.scales, ncol=w.facet) +
      psrc_style() +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_text(size=12,color="#4C4C4C"))
    
    c <- ggiraph::girafe(ggobj = c)
    
  } else {
    
    c <- ggplot2::ggplot(data=t,
                         ggplot2::aes(y=get(eval(w.y)),
                                      x=get(eval(w.x)),
                                      fill = get(eval(f)))) +
      ggplot2::geom_bar(position=w.pos, stat="identity") +
      ggplot2::ggtitle(w.title, subtitle = w.sub.title) +
      ggplot2::scale_y_continuous(labels = w.label) +
      scale_fill_discrete_psrc(w.color)
    
    if (!(is.null(w.moe))) {
      c <- c + ggplot2::geom_errorbar(ggplot2::aes(ymin=get(eval(w.y))-get(eval(w.moe)), ymax=get(eval(w.y))+get(eval(w.moe))),width=0.2, position = ggplot2::position_dodge(0.9))
    }
    
    c <- c + 
      ggplot2::facet_wrap(ggplot2::vars(get(eval(g))), scales=w.scales, ncol=w.facet) +
      psrc_style() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size=10,color="#4C4C4C"),
                     axis.text.y = ggplot2::element_text(size=12,color="#4C4C4C"))
  }
  
  return(c)
}

#' Create Column Chart
#'
#' This function allows you to create column charts (vertical bars).
#' @param t A tibble in long form for plotting
#' @param x The name of the variable you want plotted on the X-Axis
#' @param y The name of the variable you want plotted on the Y-Axis
#' @param f The name of the variable you want the fill color of the bars to be based on
#' @param pos Determines if the bars are side-by-side(dodge) or stacked(stack) - defaults to "dodge"
#' @param est Type for the Y values - enter "percent", "currency" or "number", defaults to "percent"
#' @param moe The name of the variable to be used for error bars, if desired - default is "NULL"
#' @param href A list of values to be used for any horizontal reference lines - default is "NULL"
#' @param hrefnm A list of names to be used for any horizontal reference lines that is equal length to the number of lines - default is "NULL"
#' @param hrefcl A list of colors to be used for any horizontal reference lines that is equal length to the number of lines - default is "NULL"
#' @param title Title to be used for chart, if desired - defaults to "NULL"
#' @param subtitle Sub-title to be used for chart, if desired - defaults to "NULL"
#' @param source Source reference to be used for chart, if desired - defaults to "NULL"
#' @param dec Number of decimal points in labels - defaults to 0
#' @param color Name of color palette to use - defaults to "psrc_dark"
#' @param interactive Enable hover text and other interactive features - defaults to "no"
#' @return column (vertical bar) chart that is either static or interactive depending on choice
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' 
#' library(dplyr)
#' modes_shares <- read.csv(system.file('extdata', 'example_data.csv', package='psrcplot')) %>% 
#'  filter(Category=="Mode to Work by Race") %>%
#'  filter(Geography=="Region" & Race=="Total") %>%
#'  mutate(Year = as.character(Year))
#'
#' # Static Chart with Error Bars         
#' static_chart <- create_column_chart(t=modes_shares, x="Mode",y="share", f="Year",
#'                                     title="Mode Share to Work",
#'                                     source="Source: ACS 5yr Data Table B3002",
#'                                     moe="share_moe", color="psrc_light")
#' 
#' # Interactive Chart without Error Bars         
#' interactive_chart <- create_column_chart(t=modes_shares, x="Mode",y="share", f="Year",
#'                                          title="Mode Share to Work",
#'                                          source="Source: ACS 5yr Data Table B3002",
#'                                          color="psrc_light", interactive='yes')
#'                                          
#' @export
#'

create_column_chart <- function(t, x, y, f, 
                                pos="dodge", est="percent", moe=NULL,
                                href=NULL, hrefnm=NULL, hrefcl=NULL,
                                title="", subtitle="", source="", 
                                dec = 0, color="psrc_dark", interactive='no') {
  
  # Create a color palette from PSRC palette
  grps <- t %>% dplyr::select(.data[[f]]) %>% unique() %>% dplyr::pull()
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
  
  # First Create the Static Chart
  c <- ggplot2::ggplot(data=t,
                       ggplot2::aes(x=get(eval(x)),
                                    y=get(eval(y)),
                                    text=paste0(get(eval(f)), ": ", p, prettyNum(round(get(eval(y))*fac, dec), big.mark = ","),s),
                                    fill = get(eval(f)))) +
    ggplot2::geom_bar(position=pos, stat="identity") +
    ggplot2::scale_fill_manual(values=cols)  +
    ggplot2::scale_y_continuous(labels = lab) +
    ggplot2::labs(title=title, subtitle = subtitle, caption = source) +
    psrcplot::psrc_style()
  
  # If there is a MOE value then error bars are added to the plot
  if (!(is.null(moe))) {
    c <- c + ggplot2::geom_errorbar(ggplot2::aes(ymin=get(eval(y))-get(eval(moe)), ymax=get(eval(y))+get(eval(moe))),width=0.2, position = ggplot2::position_dodge(0.9))
  }
  
  # Add reference lines if they are included 
  if (!(is.null(href))) {
    c <- c + 
      ggplot2::geom_hline(yintercept = href, color=hrefcl, linetype='solid', size=2, show.legend = FALSE) +
      ggplot2::annotate("text", x = 1, y = href+annot, label = hrefnm)
  }
  

  # Convert to Interactive if desired
  if (interactive=='yes') {
    c <- plotly::ggplotly(c, tooltip = c("text"))
    c <- plotly::layout(c, legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.10, title = ""))
    c <- plotly::layout(c, annotations = list(x = -0.05, y = -0.20, text = source,
                                              xref='paper', yref='paper', showarrow = F, 
                                              xanchor='left', yanchor='auto', xshift=0, yshift=0,
                                              font = list(family="Poppins",size=13, color="#4C4C4C")))
    c <- plotly::layout(c, annotations = list(x = 0, y = 1.05, text = subtitle, 
                                              showarrow = F, xref='paper', yref='paper', 
                                              xanchor='left', yanchor='auto', xshift=0, yshift=0,
                                              font=list(family="Poppins",size=14, color="#4C4C4C")))
    
    c <- plotly::style(c, hoverlabel = list(bgcolor = "black", font=list(family="Poppins",size=14, color="white")))
    
    
  }
  
  return(c)
}

#' Create Bar Chart
#'
#' This function allows you to create bar charts (horizontal bars).
#' @param t A tibble in long form for plotting
#' @param x The name of the variable you want plotted on the X-Axis
#' @param y The name of the variable you want plotted on the Y-Axis
#' @param f The name of the variable you want the fill color of the bars to be based on
#' @param pos Determines if the bars are side-by-side(dodge) or stacked(stack) - defaults to "dodge"
#' @param est Type for the Y values - enter "percent", "currency" or "number", defaults to "percent"
#' @param moe The name of the variable to be used for error bars, if desired - default is "NULL"
#' @param href A list of values to be used for any horizontal reference lines - default is "NULL"
#' @param hrefnm A list of names to be used for any horizontal reference lines that is equal length to the number of lines - default is "NULL"
#' @param hrefcl A list of colors to be used for any horizontal reference lines that is equal length to the number of lines - default is "NULL"
#' @param title Title to be used for chart, if desired - defaults to "NULL"
#' @param subtitle Sub-title to be used for chart, if desired - defaults to "NULL"
#' @param source Source reference to be used for chart, if desired - defaults to "NULL"
#' @param dec Number of decimal points in labels - defaults to 0
#' @param color Name of color palette to use - defaults to "psrc_dark"
#' @param interactive Enable hover text and other interactive features - defaults to "no"
#' @return bar (horizontal bar) chart that is either static or interactive depending on choice
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' 
#' library(psrcplot)
#' library(psrctrends)
#' library(dplyr)
#' 
#' install_psrc_fonts()
#' 
#' ev_registration <- get_ev_registration_ytd()
#' 
#' tbl <- ev_registration %>% 
#'        filter(county=="PSRC Region")
#'
#' static_chart <- create_bar_chart(t=tbl, 
#'                                  x="date", y="share", f="variable",
#'                                  title="Year to Date Share of Vehicle Registrations", 
#'                                  source="Source: Washington State Open Data Portal",
#'                                  pos = "stack")
#'                                    
#' interactive_chart <- create_bar_chart(t=tbl, 
#'                                       x="date", y="share", f="variable",
#'                                       title="Year to Date Share of Vehicle Registrations", 
#'                                       source="Source: Washington State Open Data Portal",
#'                                       pos = "stack", interactive='yes')                                    
#'                                   
#' 
#' @export
#'

create_bar_chart <- function(t, x, y, f, 
                             href=NULL, hrefnm=NULL, hrefcl=NULL,
                             moe=NULL, pos="dodge", est="percent",
                             title=NULL, subtitle=NULL, source=NULL,
                             dec = 0, color="psrc_dark", interactive='no') {
  
  # First create a static column chart using the column chart function
  c <- psrcplot::create_column_chart(t, x, y, f, 
                                     pos, est, moe,
                                     href, hrefnm, hrefcl,
                                     title, subtitle, source, 
                                     dec, color, interactive='no')
  
  # Flip the coordinates to make into a bar chart
  c <- c + ggplot2::coord_flip() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color="#cbcbcb"),
                   panel.grid.major.y = ggplot2::element_blank(),
                   axis.line.x = ggplot2::element_line(color="#cbcbcb"))
  
  # Convert to Interactive if desired
  if (interactive=='yes') {
    c <- plotly::ggplotly(c, tooltip = c("text"))
    c <- plotly::layout(c, legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.25, title = ""))
    c <- plotly::layout(c, annotations = list(x = -0.05, y = -0.25, text = source, 
                                              showarrow = F, xref='paper', yref='paper', 
                                              xanchor='left', yanchor='auto', xshift=0, yshift=0,
                                              font=list(family="Poppins",size=10, color="#4C4C4C")))
  }
  
  return(c)
}

#' Create PSRC TreeMap Chart
#'
#' This function allows you to create treemap charts.
#' @param t A tibble or dataframe in long form for plotting
#' @param w.area The name of the variable with thee value you want to use to size the bars
#' @param w.fill The name of the variable you want to fill the bars
#' @param w.title Title to be used for chart, if desired - defaults to "NULL"
#' @param w.sub.title Sub-title to be used for chart, if desired - defaults to "NULL"
#' @param est.type Type for the Y values - enter "percent", "currency" or "number", defaults to "percent"
#' @param w.dec Number of decimal points in labels - defaults to 0
#' @param w.color Name of color palette to use - defaults to "psrc_light"
#' @return static treemap chart
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' 
#' library(dplyr)
#' 
#' # Read in the example data and filter to Mode to Work for Everyone for all years in the data
#' df <- read.csv(system.file('extdata', 'example_data.csv', package='psrcplot')) %>% 
#'       filter(Category=="Mode to Work by Race") %>%
#'       filter(Geography=="Region" & Race=="Total") %>%
#'       mutate(Year = as.character(Year)) %>%
#'       filter(Year=="2020")
#' 
#' my.chart <- create_treemap_chart(t=df, w.area="share", w.fill="Mode", w.title="Mode Share to Work")
#' 
#' 
#' @export
#'

create_treemap_chart <- function(t, w.area, w.fill, w.title=NULL, w.sub.title=NULL, est.type="percent", w.dec=0, w.color="psrc_light") {
  
  tot <- t %>% dplyr::select(.data[[w.area]]) %>% dplyr::pull() %>% sum()
  t <- t %>% dplyr::mutate(total_share = .data[[w.area]]/tot)
  
  # Estimate type determines the labels
  if (est.type=="percent") {
    w.factor=100
    p=""
    s="%"
    
  } else if (est.type=="currency") {
    w.factor=1
    p="$"
    s=""
    
  } else {
    w.factor=1
    p=""
    s=""
  }
  
  if (est.type=="percent") {
    c <- ggplot2::ggplot(t,
                         ggplot2::aes(area = get(eval(w.area)),
                                      fill = get(eval(w.fill)), 
                                      label = paste(get(eval(w.fill)), 
                                                    paste0(p, prettyNum(round(get(eval(w.area))*w.factor,w.dec), big.mark = ","), s),
                                                    sep = "\n"))) +
      treemapify::geom_treemap() +
      treemapify::geom_treemap_text(colour = "white",
                                    place = "centre",
                                    size = 28) +
      psrc_style() +
      ggplot2::theme(legend.position = "none") +
      scale_fill_discrete_psrc(w.color) +
      ggplot2::ggtitle(w.title, subtitle = w.sub.title)
    
  } else {
    
    c <- ggplot2::ggplot(t,
                         ggplot2::aes(area = get(eval(w.area)),
                                      fill = get(eval(w.fill)), 
                                      label = paste(get(eval(w.fill)), 
                                                    paste0(p, prettyNum(round(get(eval(w.area))*w.factor,w.dec), big.mark = ","), s),
                                                    paste0(prettyNum(round(.data$total_share*100,0), big.mark = ","), "%"), 
                                                    sep = "\n"))) +
      treemapify::geom_treemap() +
      treemapify::geom_treemap_text(colour = "white",
                                    place = "centre",
                                    size = 28) +
      psrc_style() +
      ggplot2::theme(legend.position = "none") +
      scale_fill_discrete_psrc(w.color) +
      ggplot2::ggtitle(w.title, subtitle = w.sub.title)
  }
  
  return(c)
}

#' Create PSRC Bubble Chart
#'
#' This function allows you to create a bubble charts.
#' @param t A tibble or dataframe in long form for plotting
#' @param w.x The name of the variable you want plotted on the X-Axis
#' @param w.y The name of the variable you want plotted on the Y-Axis
#' @param f The name of the variable you want the fill color of the bubbles to be based on
#' @param s The name of the variable used to size the bubbles
#' @param w.color Name of color palette to use - defaults to "psrc_light"
#' @param w.title Title to be used for chart, if desired - defaults to "NULL"
#' @param w.sub.title Sub-title to be used for chart, if desired - defaults to "NULL"
#' 
#' @return bubble chart
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' 
#' library(dplyr)
#' library(psrctrends)
#' library(psrcplot)
#' 
#' # Pull NTD Transit Data by Urbanized Area
#' uza.data <- process_ntd_uza_data(yr="2022", census.yr="2020")
#' 
#' # Create Bubble chart for Transit Boardings per Capita
#' uza.chart <- create_bubble_chart(t=uza.data %>% dplyr::filter(variable == "Boardings per Capita"),
#'                                  w.x="population", w.y="estimate", f <- "plot_id", 
#'                                  s <- "population", w.color <- "psrc_light",
#'                                  w.title = "Boardings per Capita", 
#'                                  w.sub.title = "Urbanized Areas with at least 1 million people")
#' 
#' @export
#'
create_bubble_chart <- function(t, w.x, w.y, f, s, w.color="psrc_light", w.title=NULL, w.sub.title=NULL) {
  
  # Create a color palette from PSRC palette
  grps <- t %>% dplyr::select(.data[[f]]) %>% unique() %>% dplyr::pull()
  num.grps <- length(grps)
  l.colors <- unlist(psrc_colors[w.color])
  l.colors <- l.colors[1:num.grps]
  cols <- stats::setNames(l.colors, grps)
  
  p <- ggplot2::ggplot(data=t, ggplot2::aes(x = get(eval(w.x)), y = get(eval(w.y)))) + 
    ggplot2::geom_point(ggplot2::aes(color = get(eval(f)), size = get(eval(s))), alpha = 1.0) +
    ggplot2::scale_size(range = c(0.5, 12)) +
    ggplot2::scale_color_manual(values=cols) +
    ggplot2::ggtitle(w.title, subtitle = w.sub.title) +
    psrc_style() +
    ggplot2::guides(size = "none") +
    ggplot2::theme(legend.position = "bottom",
                   axis.text.x = ggplot2::element_blank())
  
  return(p)
  
}


#' Create PSRC Line Chart
#'
#' This function allows you to create a line chart.
#' @param t A tibble or dataframe in long form for plotting
#' @param w.x The name of the variable you want plotted on the X-Axis
#' @param w.y The name of the variable you want plotted on the Y-Axis
#' @param w.g The name of the variable you want the fill color of the lines to be based on
#' @param w.color Name of color palette to use - defaults to "psrc_light"
#' @param w.title Title to be used for chart, if desired - defaults to "NULL"
#' @param w.sub.title Sub-title to be used for chart, if desired - defaults to "NULL"
#' @param est.type Type for the Y values - enter "percent", "currency" or "number", defaults to "number"
#' @param w.dec Number of decimal points in labels - defaults to 0
#' @param x.type Type of values for the X-Axis either "Continuous" or "Date", defaults to "Date"
#' @param d.form Format for Date values 
#' @param w.breaks Break points to use if using a continuous scale, defaults to NULL
#' @param w.lwidth Width of lines, defaults to 1
#' @param w.interactive Enable hover text and other interactive features - defaults to "no"
#' 
#' @return line chart
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' 
#' library(dplyr)
#' library(psrctrends)
#' library(psrcplot)
#' 
#' airport.operations <- process_sea_operations_data(c.yr=2022, c.mo=4, f.yr = 2019)
#' sea.chart.2022 <- create.line.chart(t=airport.operations %>% 
#'                                     dplyr::filter(variable == "PASSENGER GRAND TOTAL"),
#'                                     w.x="equiv_day", w.y="estimate", w.g="year", 
#'                                     d.form="%B", w.lwidth=2,
#'                                     w.title = "Monthly Passenger Enplanements: 2019 to 2022", 
#'                                     w.sub.title = "Seattle-Tacoma International Airport")
#'
#' jobs.data <- process_qcew_monthly_msa(c.yr=2022, c.mo=5)
#'
#' tbl <- jobs.data %>% filter(variable=="Total Nonfarm"
#'                            & year>="2018"
#'                            & !(geography %in% c("Washington State", "Region")))
#'                            
#' jobs.chart <- create.line.chart(t=tbl, w.x="data_day", w.y="estimate", 
#'                                 w.g="geography", w.lwidth=2, d.form ="%Y", 
#'                                 w.interactive="yes",
#'                                 w.title = "Monthly Wage & Salary Jobs: 2018 to 2022")                            
#' 
#' @export
#'
create.line.chart <- function(t, w.x, w.y, w.g, w.title=NULL, w.sub.title=NULL, est.type="number", w.dec=0, x.type="Date", d.form="%b-%Y", w.breaks=NULL, w.lwidth=1, w.color="psrc_light", w.interactive='no') {
  
  grps <- t %>% dplyr::select(.data[[w.g]]) %>% unique() %>% dplyr::pull()
  num.grps <- length(grps)
  l.colors <- unlist(psrc_colors[w.color])
  l.colors <- l.colors[1:num.grps]
  cols <- stats::setNames(l.colors, grps)
  
  # Estimate type determines the labels for the axis and the format of the number for the hover-text
  if (est.type=="percent") {
    w.factor=100
    p=""
    s="%"
    w.label=scales::label_percent()
    
  } else if (est.type=="currency") {
    w.factor=1
    p="$"
    s=""
    w.label=scales::label_dollar()
    
  } else {
    w.factor=1
    p=""
    s=""
    w.label=scales::label_comma()
  }
  
  if (w.interactive == 'yes') {
    
    if (x.type!="Continuous") {
      
      g <- ggplot2::ggplot(data=t, 
                           ggplot2::aes(x=get(eval(w.x)),
                                        y=get(eval(w.y)), 
                                        group=get(eval(w.g))))  + 
        ggiraph::geom_line_interactive(ggplot2::aes(color=get(eval(w.g))), size = w.lwidth, linejoin = "round")+
        ggiraph::geom_point_interactive(ggplot2::aes(x=get(eval(w.x)),
                                                     y=get(eval(w.y)),
                                                     color=get(eval(w.g)),
                                                     tooltip = paste0(get(eval(w.g)), ": ", p, prettyNum(round(get(eval(w.y))*w.factor,w.dec), big.mark = ","),s),
                                                     data_id = get(eval(w.y))), size = w.lwidth/2) +
        ggplot2::scale_x_date(labels = scales::date_format(d.form)) +
        ggplot2::scale_y_continuous(labels = w.label) +
        ggplot2::scale_color_manual(values=cols)  +
        ggplot2::ggtitle(w.title, subtitle = w.sub.title) +
        psrc_style()
      
      g <- ggiraph::girafe(ggobj = g)
      
    } else {
      
      g <- ggplot2::ggplot(data=t, 
                           ggplot2::aes(x=get(eval(w.x)),
                                        y=get(eval(w.y)), 
                                        group=get(eval(w.g))))  + 
        ggiraph::geom_line_interactive(ggplot2::aes(color=get(eval(w.g))), size = w.lwidth, linejoin = "round")+
        ggiraph::geom_point_interactive(ggplot2::aes(x=get(eval(w.x)),
                                                     y=get(eval(w.y)),
                                                     color=get(eval(w.g)),
                                                     tooltip = paste0(get(eval(w.g)), ": ", p, prettyNum(round(get(eval(w.y))*w.factor,w.dec), big.mark = ","),s),
                                                     data_id = get(eval(w.y))), size = w.lwidth/2) +
        ggplot2::scale_x_discrete(breaks=w.breaks) +
        ggplot2::scale_y_continuous(labels = w.label) +
        ggplot2::scale_color_manual(values=cols)  +
        ggplot2::ggtitle(w.title, subtitle = w.sub.title) +
        psrc_style()
      
      g <- ggiraph::girafe(ggobj = g)
      
    }
  } else {
    
    if (x.type=="Continuous") {
      g <- ggplot2::ggplot(data=t, 
                           ggplot2::aes(x=get(eval(w.x)),
                                        y=get(eval(w.y)), 
                                        group=get(eval(w.g))))  + 
        ggplot2::geom_line(ggplot2::aes(color=get(eval(w.g))), size=w.lwidth, linejoin = "round") +
        ggplot2::geom_point(ggplot2::aes(color=get(eval(w.g))))+
        ggplot2::scale_x_discrete(breaks=w.breaks) +
        ggplot2::scale_y_continuous(labels = w.label) +
        ggplot2::scale_color_manual(values=cols)  +
        ggplot2::ggtitle(w.title, subtitle = w.sub.title) +
        psrc_style()
      
    } else {
      
      g <- ggplot2::ggplot(data=t, 
                           ggplot2::aes(x=get(eval(w.x)),
                                        y=get(eval(w.y)), 
                                        group=get(eval(w.g))))  + 
        ggplot2::geom_line(ggplot2::aes(color=get(eval(w.g))), size = w.lwidth, linejoin = "round") +
        ggplot2::scale_x_date(labels = scales::date_format(d.form)) +
        ggplot2::scale_y_continuous(labels = w.label) +
        ggplot2::scale_color_manual(values=cols)  +
        ggplot2::ggtitle(w.title, subtitle = w.sub.title) +
        psrc_style()
      
    }
  }
  
  return(g)
}

