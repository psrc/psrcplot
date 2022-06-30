#' Create PSRC Charts
#'
#' This function allows you to create facet bar charts.
#' @param t A tibble or dataframe in long form for plotting
#' @param w.x The name of the variable you want plotted on the X-Axis
#' @param w.y The name of the variable you want plotted on the Y-Axis
#' @param f The name of the variable you want the fill color of the bars to be based on
#' @param g The name of the variable to be the facets
#' @param est.type Type for the Y values - enter "percent", "currency" or "number", default in "percent"
#' @param w.scales Value for axis in facets, either "fixed" or "free" - defaults to "free"
#' @param w.facet Value for the number of columns in your facet - defaults to 3
#' @param w.dec Number of decimal points in labels - defaults to 0
#' @param l.pos Position for the bar labels of "above" or "within" - defaults to "above"
#' @param w.color Name of color palette to use - defaults to "psrc_distinct_10"
#' @return facet bar chart that is either static or interactive depending on choice
#' @importFrom magrittr %<>% %>%
#' @export
#'

create_facet_bar_chart <- function(t, w.x, w.y, f, g, est.type="percent", w.scales="free", w.facet=3, w.dec = 0, l.pos="above", w.color="psrc_distinct_10") {

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
