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

create_treemap_chart <- function(t, s, fill, title=NULL, subtitle=NULL, est=NULL, dec=0, color=NULL) {
  
  confirm_fonts() 
  
  # Determine number of fill groups
  grps <- select(t, all_of(fill)) %>% unique() %>% dplyr::pull()
  num.grps <- length(grps)
  
  total_share<- NULL
  s_vctr <- t %>% select(all_of(s)) %>% dplyr::pull()
  est <- est_type_default(s)
  tot <- sum(s_vctr)
  t %<>% dplyr::mutate(total_share = .data[[s]]/tot) %>% dplyr::arrange(total_share)
  
  # Estimate type determines the labels
  valfrmt <- est_number_formats(est)
  
  if (est=="percent") {
    c <- ggplot2::ggplot(t,
                         ggplot2::aes(area = .data[[s]],
                                      fill = .data[[fill]], 
                                      label = paste(.data[[fill]], 
                                                    paste0(valfrmt$pfx, prettyNum(formattable::digits(round(.data[[s]] * valfrmt$fac, dec), digits=max(0, dec)), big.mark = ","), valfrmt$sfx),
                                                    sep = "\n")))
  } else {
    c <- ggplot2::ggplot(t,
                         ggplot2::aes(area = .data[[s]],
                                      fill = .data[[fill]], 
                                      label = paste(.data[[fill]], 
                                                    paste0(valfrmt$pfx, prettyNum(formattable::digits(round(.data[[s]] * valfrmt$fac, dec), digits=max(0, dec)), big.mark = ","), valfrmt$sfx),
                                                    paste0(prettyNum(round(.data$total_share * 100,0), big.mark = ","), "%"), 
                                                    sep = "\n")))
  }
  c <- c + treemapify::geom_treemap() +
    treemapify::geom_treemap_text(colour = "white",
                                  place = "centre",
                                  size = 28, 
                                  na.rm = TRUE) +
    psrc_style() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle(title, subtitle = subtitle)
  
  # Apply color palette if available, check enough colors are available
  if(!is.null(color)) {
    equal_colors_and_groups <- length(color) == length(grps)
    
    if(equal_colors_and_groups == FALSE & length(color) < length(grps)) {
      stop(paste("Not enough colors in color palette. There are", length(color), "colors but", num.grps, "groups"))
    }
    
    cols <- stats::setNames(color, grps)
    c <- c +
      ggplot2::scale_fill_manual(values=cols)
  }
  
  return(c)
}
