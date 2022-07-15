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
#' @param w.color Name of color palette to use - defaults to "psrc_distinct_10"
#' @return facet bar chart that is either static or interactive depending on choice
#' @importFrom magrittr %<>% %>%
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

create_facet_bar_chart <- function(t, w.x, w.y, f, g, est.type="percent", w.scales="free", w.facet=3, w.dec = 0, l.pos="above", w.color="psrc_dark") {

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
  
  c <- ggplot2::ggplot(data=t,
                       ggplot2::aes(y=get(eval(w.y)),
                                    x=get(eval(w.x)),
                                    fill = get(eval(f)))) +
    ggplot2::geom_bar(position=w.pos, stat="identity") +
    ggplot2::geom_text(ggplot2::aes(label = paste0(p,prettyNum(round(get(eval(w.y))*w.factor,w.dec), big.mark = ","),s)), vjust = l, colour = l.clr, size=l.sz, fontface='bold') +
    ggplot2::scale_y_continuous(labels = w.label) +
    scale_fill_discrete_psrc(w.color) +
    ggplot2::facet_wrap(ggplot2::vars(get(eval(g))), scales=w.scales, ncol=w.facet) +
    psrc_style() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size=12,color="#4C4C4C"))

  return(c)
}

#' Create PSRC Bar Chart
#'
#' This function allows you to create bar charts.
#' @param t A tibble or dataframe in long form for plotting
#' @param w.x The name of the variable you want plotted on the X-Axis
#' @param w.y The name of the variable you want plotted on the Y-Axis
#' @param f The name of the variable you want the fill color of the bars to be based on
#' @param w.moe The name of the variable to be used for error bars, if desired - default to "NULL"
#' @param w.title Title to be used for chart, if desired - defaults to "NULL"
#' @param w.sub.title Sub-title to be used for chart, if desired - defaults to "NULL"
#' @param w.pos Determines if the bars are side-by-side(dodge) or stacked(stack) - defaults to "dodge"
#' @param est.type Type for the Y values - enter "percent", "currency" or "number", defaults to "percent"
#' @param w.dec Number of decimal points in labels - defaults to 0
#' @param w.color Name of color palette to use - defaults to "psrc_dark"
#' @param w.interactive Enable hover text and other interactive features - defaults to "no"
#' @return bar chart that is either static or interactive depending on choice
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
#'       mutate(Year = as.character(Year))
#' 
#' # Create a chart for mode shares by year with error bars
#' my.chart <- create_bar_chart(t=df, w.x="Mode", w.y="share", f="Year", w.moe="share_moe", 
#'                              w.color="psrc_light")
#'
#' # Create a chart for mode shares by year without error bars
#' my.chart <- create_bar_chart(t=df, w.x="Mode", w.y="share", f="Year", 
#'                              w.color="psrc_light")
#'                              
#' # Create a chart for mode shares by year without error bars with Titles
#' my.chart <- create_bar_chart(t=df, w.x="Mode", w.y="share", f="Year", 
#'                              w.color="psrc_light",
#'                              w.title="Mode Share to Work",
#'                              w.sub.title="by Census Year")
#'                                                                                   
#' # Create a chart for mode shares by year with error bars with Main Title only and make interactive
#' my.chart <- create_bar_chart(t=df, w.x="Mode", w.y="share", f="Year", 
#'                              w.color="psrc_light",
#'                              w.title="Mode Share to Work",
#'                              w.moe="share_moe",
#'                              w.interactive="yes") 
#' 
#' 
#' @export
#'

create_bar_chart <- function(t, w.x, w.y, f, w.moe=NULL, w.title=NULL, w.sub.title=NULL, w.pos="dodge", est.type="percent", w.dec = 0, w.color="psrc_dark", w.interactive='no') {
  
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
    
    c <- ggplot2::ggplot(data=t,
                         ggplot2::aes(y=get(eval(w.y)),
                                      x=get(eval(w.x)),
                                      fill = get(eval(f)),
                                      tooltip=paste0(get(eval(f)), ": ", p, prettyNum(round(get(eval(w.y))*w.factor,w.dec), big.mark = ","),s),
                                      data_id=get(eval(w.y)))) +
      ggiraph::geom_bar_interactive(position=w.pos, stat="identity")+
      ggplot2::scale_y_continuous(labels = w.label) +
      scale_fill_discrete_psrc(w.color)  +
      psrc_style()
    
    if (!(is.null(w.moe))) {
    
      c <- c + ggplot2::geom_errorbar(ggplot2::aes(ymin=get(eval(w.y))-get(eval(w.moe)), ymax=get(eval(w.y))+get(eval(w.moe))),width=0.2, position = ggplot2::position_dodge(0.9))
    
    }
    
    if (!(is.null(w.title))) {
      c <- c + ggplot2::ggtitle(w.title, subtitle = w.sub.title)
    } else {
      c <- c
    }
    
    c <- ggiraph::girafe(ggobj = c)
    
  } else {
  
    c <- ggplot2::ggplot(data=t,
                       ggplot2::aes(y=get(eval(w.y)),
                                    x=get(eval(w.x)),
                                    fill = get(eval(f)))) +
      ggplot2::geom_bar(position=w.pos, stat="identity") +
      ggplot2::scale_y_continuous(labels = w.label) +
      scale_fill_discrete_psrc(w.color)  +
      psrc_style()

    if (!(is.null(w.moe))) {
      c <- c + ggplot2::geom_errorbar(ggplot2::aes(ymin=get(eval(w.y))-get(eval(w.moe)), ymax=get(eval(w.y))+get(eval(w.moe))),width=0.2, position = ggplot2::position_dodge(0.9))
    }
  
    if (!(is.null(w.title))) {
      c <- c + ggplot2::ggtitle(w.title, subtitle = w.sub.title)
    } else {
      c <- c
    }
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

  c <- ggplot2::ggplot(t,
                       ggplot2::aes(area = get(eval(w.area)),
                                    fill = get(eval(w.fill)), 
                                    label = paste(get(eval(w.fill)), paste0(p, prettyNum(round(get(eval(w.area))*w.factor,w.dec), big.mark = ","), s), sep = "\n"))) +
    treemapify::geom_treemap() +
    treemapify::geom_treemap_text(colour = "white",
                                place = "centre",
                                size = 28) +
    psrc_style() +
    ggplot2::theme(legend.position = "none") +
    scale_fill_discrete_psrc(w.color)

  if (!(is.null(w.title))) {
    c <- c + ggplot2::ggtitle(w.title, subtitle = w.sub.title)
  }

  return(c)
}

