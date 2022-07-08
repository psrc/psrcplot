#' PSRC Color Palettes
#'
#' Functions to create PSRC Color Palettes
#' @importFrom magrittr %<>% %>%
#'

psrc_colors = list(
  psrc_purples = c("#4a0048", "#630460", "#91268F", "#AD5CAB", "#C388C2", "#E3C9E3"),
  psrc_oranges = c("#7a2700", "#9f3913", "#F05A28", "#F4835E", "#F7A489", "#FBD6C9"),
  psrc_greens = c("#3f6618", "#588527", "#8CC63E", "#A9D46E", "#C0E095", "#E2F1CF"),
  psrc_blues = c("#005753", "#00716c", "#00A7A0", "#40BDB8", "#73CFCB", "#BFE9E7"),
  psrc_grays = c("#2f3030", "#3E4040", "#4C4C4C", "#76787A", "#999999", "#BCBEC0"),
  psrc_dark = c("#630460", "#9f3913", "#588527", "#00716c","#3e4040", "#C388C2", "#F7A489", "#C0E095", "#73CFCB", "#999999"),
  psrc_light = c("#A9D46E", "#73CFCB", "#F4835E", "#C388C2","#76787A","#588527","#00716c","#9f3913","#630460","#3e4040"),
  psrc_pairs = c("#91268F","#C388C2","#8CC63E","#C0E095","#F05A28","#F7A489","#00A7A0","#73CFCB","#4C4C4C","#999999"))

psrc_palettes = function(name, n, all_palettes = psrc_colors, type = c("continuous","continuous","continuous","continuous","continuous","discrete","discrete","discrete")) {
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
