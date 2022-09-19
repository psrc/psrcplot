#' PSRC Styles
#' Useful Functions to Develop PSRC Data Visualizations
#'
#' This function allows you to add the PSRC theme to your ggplot graphics.
#' @importFrom magrittr %<>% %>%
#' @export
#'

psrc_style <- function() {
  font <- "Poppins"

  ggplot2::theme(

    #Text format:
    #This sets the font, size, type and color of text for the chart's title
    plot.title = ggplot2::element_text(family=font,
                                       size=18,
                                       face="bold",
                                       color="#4C4C4C"),

    #This sets the font, size, type and color of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(family=font,
                                          size=14,
                                          margin=ggplot2::margin(9,0,9,0)),

    #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
    plot.caption =  ggplot2::element_text(family=font,
                                          size=12,
                                          face="bold",
                                          color="#4C4C4C"),
    #Legend format
    #This sets the position and alignment of the legend, removes a title and background for it and sets the requirements for any text within the legend.
    legend.position = "bottom",
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font,
                                        size=12,
                                        color="#4C4C4C"),

    #Axis format
    #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(family=font,
                                      size=14,
                                      color="#4C4C4C"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),

    #Grid lines
    #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines.
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),

    #Blank background
    #This sets the panel background as blank, removing the standard grey ggplot background color from the plot
    panel.background = ggplot2::element_blank(),

    #Strip background sets the panel background for facet-wrapped plots to PSRC Gray and sets the title size of the facet-wrap title
    strip.background = ggplot2::element_rect(fill="#BCBEC0"),
    strip.text = ggplot2::element_text(size  = 16,  hjust = 0)
    )
}
