#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
NULL

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

#' Make similiarly sized multi-line strings from single-line strings
#' 
#' @param str_vector string or character vector to insert line breaks
#' @param wrap_width max string width
#' @return identical string or character vector, evenly wrapped
#' @author Michael Jensen
#' 
#' @importFrom stringr str_trim str_replace_all
#' 
#' @export
wrap_evenly <- function(str_vector, wrap_width){
  mini_me <- function(in_string, wrap_width){
    in_string <- str_trim(in_string)
    str_length <- nchar(in_string)
    if(str_length <= wrap_width){
      out_string <- in_string
    }else{
      lines <- ceiling(str_length / wrap_width)
      equi_width <- ceiling(str_length / lines)
      regex <- paste0(" ?\\b(.{", ceiling(equi_width * 0.8), ",", floor(equi_width * 1.2), "}) ")
      out_string <- str_replace_all(in_string, regex, "\\1\n")
      if(is.null(out_string)){out_string <- in_string}
    }
    return(out_string)
  }
  rs <- mapply(mini_me, stringr::str_trim(str_vector), wrap_width)
}

#' wrap_evenly function in form to apply to ggplot2 labels
#' 
#' @param width max string width
#' @return identical string or character vector, evenly wrapped
wrap_labels_evenly <- function(width) {
  force(width)
  function(x) {
    unlist(vapply(wrap_evenly(x, wrap_width = width), paste0, character(1), collapse = "\n"))
  }
}

#' wrap_evenly function in form to apply to ggplot2 labeller spec
#' 
#' @param width max string width
labeller_wrap_evenly <- function(width) {
  force(width)
  function(labels) {
    labelles <- label_value(labels, multi_line = TRUE)
    lapply(labelles, function(x) {
      vapply(wrap_evenly(x, wrap_width = width), paste0, character(1), collapse = "\n")
    })
  }
}

#' Helper - return estimate type if null
#' 
#' @param x numeric vector or dataframe column
est_type_default <- function(x){
  est <- "number"
  if(is.numeric(x)){
    xmin <- min(x)
    xmax <- max(x)
    if(xmin>=0 & xmax<=1){
      est <- "percent"
      }
  }
  return(est)
}

#' Helper - return number formatting to match estimate type
#' 
#' @param est Type for the numeric values - enter "percent", "currency" or "number"
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
#' @param est Type for the numeric values - enter "percent", "currency" or "number"
est_label_formats <- function(est){
  lab <- if(est=="percent"){scales::label_percent()
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
#' @return plotly object
#' @author Craig Helmann, Michael Jensen
#' 
#' @export
make_interactive <- function(p, title=NULL, subtitle=NULL){
  x.vals <- length(ggplot2::layer_scales(p)$x$range$range)                                         # Number of x categories in ggplot object
  x.pos <- ggplot2::layer_scales(p)$x$position                                                     # Left or bottom (i.e. bar or column chart)
  geom_list <- sapply(p$layers, function(x) class(x$geom)[1])                                      # Used to differentiate between chart types  
  hover_yn <- if("GeomBar" %in% geom_list){NULL}else{"x"}
  vlift <- if("GeomBar" %in% geom_list){1.10}else{1.05}
  
  p <- p + ggplot2::theme(axis.title = ggplot2::element_blank())                                   # Remove Bar labels and axis titles
  m <- list(l = 50, r = 50, b = 200, t = 200, pad = 4)
  p <- plotly::ggplotly(p, tooltip=c("text"), autosize = T, margin = m)                            # Make Interactive
  p <- plotly::style(p, hoverlabel=list(font=list(family="Poppins", size=11, color="white")))      # Set Font for Hover-Text
  p <- plotly::layout(p, xaxis=list(tickfont=list(family="Poppins", size=11, color="#2f3030")))    # Format X-Axis
  p <- plotly::layout(p, yaxis=list(tickfont=list(family="Poppins", size=11, color="#2f3030")))    # Format Y-Axis
  
                                                                                                  # Turn on Legend
  # if labels are rotated, they might run into the legend now?
  p <- plotly::layout(p,
                        legend=list(orientation="h", xanchor="center", xref="container", x=0.5, y=-0.10,         
                                    title="", font=list(family="Poppins", size=11, color="#2f3030"),
                                    pad=list(b=50, t=50)),
                        hovermode = hover_yn)

  
  p <- plotly::layout(p, title= list(text = ""))                                                   # Remove Plotly Title
  
  if(!(is.null(title)) & !(is.null(subtitle))) {                                                   # If there is both title and subtitle
    
    p <- plotly::layout(p, 
                        annotations = list(x= 0 , y = vlift + 0.05, text = title,                  # -- add the title, located high enough for room for subtitle
                                           xref='paper', yref='paper', showarrow = FALSE, 
                                           font = list(family="Poppins Black",size=14, color="#4C4C4C")))
    p <- plotly::layout(p, 
                        annotations = list(x= 0, y = vlift, text = subtitle,                       # -- then add the subtitle 
                                           showarrow = FALSE, xref='paper', yref='paper', 
                                           font=list(family="Poppins",size=12, color="#4C4C4C")))
  }else if(!(is.null(title)) & is.null(subtitle)) {                                                # If there is no Subtitle
    
    p <- plotly::layout(p, 
                        annotations = list(x= 0, y = vlift, text = title,                          # -- just add the title
                                           xref='paper', yref='paper', showarrow = FALSE,
                                           font = list(family="Poppins Black",size=14, color="#4C4C4C")))
  }
  return(p)
}

#' Helper - add source citation to plotly object
#' 
#' @param p plotly object
#' @param source Source reference as character string
#' @return plotly object with citation
#' @author Michael Jensen
add_citation <- function(p, source){
  if("plotly" %in% class(p) & is.character(source)){
    p <- plotly::layout(p, 
                        annotations = list(x= -0.04, y= -0.2, text=source,
                                           xref='paper', yref='paper', showarrow=FALSE, 
                                           xanchor='left', yanchor='auto', xshift=0, yshift=0,
                                           font = list(family="Poppins",size=10, color="#4C4C4C")))
  }
  return(p)
}
