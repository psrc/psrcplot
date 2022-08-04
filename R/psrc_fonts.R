#' Install PSRC Fonts
#' Installs Google Fonts used in PSRC Visualizations
#'
#' This function will install fonts from Google for use in PSRC visualizations. Run it at the start of each R session.
#' 
#' @examples
#' 
#' install_psrc_fonts()
#' 
#' @export
#'

install_psrc_fonts <- function() {

  sysfonts::font_add_google(name="Poppins", family="Poppins")
  sysfonts::font_add_google(name="Lato", family="Lato")
  sysfonts::font_add_google(name="Oswald", family="Oswald")
  
  showtext::showtext_auto()
  
}
