#' PSRC Color Palettes
#'
#' Functions to create PSRC Color Palettes
#' @importFrom magrittr %<>% %>%
#' @export
#'

psrc_colors = list(
  psrc_purples = c("#4a0048", "#630460", "#91268F", "#AD5CAB", "#C388C2", "#E3C9E3"),
  psrc_oranges = c("#7a2700", "#9f3913", "#F05A28", "#F4835E", "#F7A489", "#FBD6C9"),
  psrc_greens = c("#3f6618", "#588527", "#8CC63E", "#A9D46E", "#C0E095", "#E2F1CF"),
  psrc_blues = c("#005753", "#00716c", "#00A7A0", "#40BDB8", "#73CFCB", "#BFE9E7"),
  psrc_grays = c("#2f3030", "#3E4040", "#4C4C4C", "#76787A", "#999999", "#BCBEC0"),
  # Dark and Light Colors using Original PSRC Colors
  psrc_dark = c("#630460", "#9f3913", "#588527", "#00716c","#3e4040", "#C388C2", "#F7A489", "#C0E095", "#73CFCB", "#999999"),
  psrc_light = c("#A9D46E", "#73CFCB", "#F4835E", "#C388C2","#76787A","#588527","#00716c","#9f3913","#630460","#3e4040"),
  psrc_pairs = c("#91268F","#C388C2","#8CC63E","#C0E095","#F05A28","#F7A489","#00A7A0","#73CFCB","#4C4C4C","#999999"),
  # Light and Dark Pairs with new PSRC Website Colors
  DkGnLtGn = c("#005753","#A9D46E"),
  LtGnDkGn = c("#A9D46E","#005753"),
  DkPrLtPr = c("#4A0048","#E3C9E3"),
  LtPrDkPr = c("#E3C9E3","#4A0048"),
  DkOrLtOr = c("#EC9B21", "#FBD6C9"),
  LtOrDkOr = c("#FBD6C9", "#EC9B21"),
  # Green Pairs with new PSRC Website Colors 
  GnPr = c("#005753","#4A0048"),
  PrGn = c("#4A0048","#005753"),
  GnOr = c("#005753","#EC9B21"),
  OrGn = c("#EC9B21", "#005753"),
  GnBk = c("#005753","#2F3030"),
  BkGn = c("#2F3030", "#005753"),
  # Purple Pairs with Orange & Black
  PrOr = c("#4A0048","#EC9B21"),
  OrPr = c("#EC9B21", "#4A0048"),
  PrBk = c("#4A0048","#2F3030"),
  BkPr = c("#2F3030","#4A0048"),
  # Orange Paris with Balck
  OrBk = c("#EC9B21", "#2F3030"),
  BkOr = c("#2F3030", "#EC9B21"))

psrc_palettes = function(name, n, all_palettes = psrc_colors, type = c("continuous","continuous","continuous","continuous","continuous",
                                                                       "discrete","discrete","discrete",
                                                                       "discrete","discrete","discrete","discrete","discrete","discrete",
                                                                       "discrete","discrete","discrete","discrete","discrete","discrete",
                                                                       "discrete","discrete","discrete","discrete",
                                                                       "discrete","discrete")) {
  palette = all_palettes[[name]]
  if (missing(n)) {
    n = length(palette)
  }
  type = match.arg(type)
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               discrete = palette[1:n]
  )
  structure(out, name = name, class = "palette")
}

#' PSRC Discrete Color Palettes
#'
#' Function for a discrete color fill for use in ggplot2
#' @param name The name of the discrete color palette
#' @export
#'
scale_fill_discrete_psrc = function(name) {
  ggplot2::scale_fill_manual(values = psrc_palettes(name, type = "discrete"))
}

#' PSRC Continuous Color Palettes
#'
#' Function for a continuous color fill for use in ggplot2
#' @param name The name of the continuous color palette
#' @export
#'
scale_fill_continuous_psrc = function(name) {
  ggplot2::scale_fill_gradientn(colors = psrc_palettes(name = name, type = "continuous"))
}

#' Print PSRC Color Palettes
#'
#' Function to print to screen PSRC Color Palettes
#' @examples
#' 
#' # Print the PSRC palettes to the viewer window
#' psrc_print_palette()
#' 
#' @export
#'

psrc_print_palette <- function() {
  bordercolor <- "black"
  x <- c(-8,30)
  y <- c(0,(length(psrc_colors)*3))
  i <- (length(psrc_colors)*3)
  n <- 1
  plot(1, type="n", xlab="", ylab="", xlim=x, ylim=y,axes=FALSE, frame.plot=FALSE)
  for (p in psrc_colors) {
    graphics::rect(0,i,3,i-1, col = p[1], border = bordercolor, lwd = 2)
    graphics::rect(3,i,6,i-1, col = p[2], border = bordercolor, lwd = 2)
    graphics::rect(6,i,9,i-1, col = p[3], border = bordercolor, lwd = 2)
    graphics::rect(9,i,12,i-1, col = p[4], border = bordercolor, lwd = 2)
    graphics::rect(12,i,15,i-1, col = p[5], border = bordercolor, lwd = 2)
    graphics::rect(15,i,18,i-1, col = p[6], border = bordercolor, lwd = 2)
    graphics::rect(18,i,21,i-1, col = p[7], border = bordercolor, lwd = 2)
    graphics::rect(21,i,24,i-1, col = p[8], border = bordercolor, lwd = 2)
    graphics::rect(24,i,27,i-1, col = p[9], border = bordercolor, lwd = 2)
    graphics::rect(27,i,30,i-1, col = p[10], border = bordercolor, lwd = 2)
    graphics::text(x = -10, y = i-.5, # Coordinates
         label = names(psrc_colors[n]), pos =4)
    i = i-3
    n= n+1
  }
}
