#' PSRC Color Palettes
#'
#' Functions to create PSRC Color Palettes
#' 
#' Available Options: "purples_inc", "oranges_inc", "greens_inc", "blues_inc", "grays_inc",
#'    "purples_dec", "oranges_dec", "greens_dec", "blues_dec", "grays_dec",
#'    "pognbgy_5", "pgnobgy_5", "gnbopgy_5", "obgnpgy_5",
#'    "pognbgy_10", "pgnobgy_10", "gnbopgy_10", "obgnpgy_10",
#'    "psrc_light", "psrc_dark", "psrc_pairs"
#' 
#' @importFrom magrittr %<>% %>%
#' @export
#'

psrc_colors = list(
  # PSRC colors light to dark (increasing)
  purples_inc = c("#E3C9E3", "#C388C2", "#AD5CAB", "#91268F", "#630460", "#4a0048"),
  oranges_inc = c("#FBD6C9", "#F7A489", "#F4835E", "#F05A28", "#9f3913", "#7a2700"),
  greens_inc = c("#E2F1CF", "#C0E095", "#A9D46E", "#8CC63E", "#588527", "#3f6618"),
  blues_inc = c("#BFE9E7", "#73CFCB", "#40BDB8", "#00A7A0", "#00716c", "#005753"),
  grays_inc = c("#BCBEC0", "#999999", "#76787A", "#4C4C4C", "#3E4040", "#2f3030"),
  # PSRC Colors dark to light (decreasing)
  purples_dec = c("#4a0048", "#630460", "#91268F", "#AD5CAB", "#C388C2", "#E3C9E3"),
  oranges_dec = c("#7a2700", "#9f3913", "#F05A28", "#F4835E", "#F7A489", "#FBD6C9"),
  greens_dec = c("#3f6618", "#588527", "#8CC63E", "#A9D46E", "#C0E095", "#E2F1CF"),
  blues_dec = c("#005753", "#00716c", "#00A7A0", "#40BDB8", "#73CFCB", "#BFE9E7"),
  grays_dec = c("#2f3030", "#3E4040", "#4C4C4C", "#76787A", "#999999", "#BCBEC0"),
  # Primary PSRC 5 Color Palettes
  pognbgy_5 = c("#91268F", "#F05A28", "#8CC63E", "#00A7A0","#4C4C4C"),
  pgnobgy_5 = c("#91268F", "#8CC63E", "#F05A28",  "#00A7A0","#4C4C4C"),
  gnbopgy_5 = c("#8CC63E", "#00A7A0", "#F05A28", "#91268F","#4C4C4C"),
  obgnpgy_5 = c("#F05A28", "#00A7A0", "#8CC63E", "#91268F","#4C4C4C"),
  # Primary PSRC 10 Color Palettes
  pognbgy_10 = c("#91268F", "#F05A28", "#8CC63E", "#00A7A0", "#4C4C4C", "#630460", "#9f3913", "#588527", "#00716c", "#3e4040"),
  pgnobgy_10 = c("#91268F", "#8CC63E", "#F05A28", "#00A7A0", "#4C4C4C", "#630460", "#588527", "#9f3913", "#00716c", "#3e4040"),
  gnbopgy_10 = c("#8CC63E", "#00A7A0", "#F05A28", "#91268F", "#4C4C4C", "#588527", "#00716c", "#9f3913", "#630460", "#3e4040"),
  obgnpgy_10 = c("#F05A28", "#00A7A0", "#8CC63E", "#91268F", "#4C4C4C", "#9f3913", "#00716c", "#588527", "#630460", "#4C4C4C"),
  # Dark and Light Colors
  psrc_dark = c("#630460", "#9f3913", "#588527", "#00716c","#3e4040", "#C388C2", "#F7A489", "#C0E095", "#73CFCB", "#999999"),
  psrc_light = c("#A9D46E", "#73CFCB", "#F4835E", "#C388C2","#76787A","#588527","#00716c","#9f3913","#630460","#3e4040"),
  psrc_pairs = c("#91268F","#C388C2","#8CC63E","#C0E095","#F05A28","#F7A489","#00A7A0","#73CFCB","#4C4C4C","#999999"))

psrc_palettes = function(name, n, all_palettes = psrc_colors, type = c("continuous","continuous","continuous","continuous","continuous",
                                                                       "continuous","continuous","continuous","continuous","continuous",
                                                                       "discrete","discrete","discrete","discrete",
                                                                       "discrete","discrete","discrete")) {
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
#' print_psrc_palette()
#' 
#' @export
#'

print_psrc_palette <- function() {
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
