#' Install PSRC Fonts
#' Installs Google Fonts used in PSRC Visualizations
#'
#' This function will install fonts from Google for use in PSRC visualizations.
#' 
#' @examples
#' 
#' install_psrc_fonts()
#' 
#' @export
#'

install_psrc_fonts <- function() {

  sysfonts::font_add_google(name="Poppins", family="Poppins")
  showtext::showtext_auto()
  
}

