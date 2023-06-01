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

`%not_in%` <- Negate(`%in%`)

confirm_fonts <- function(){
  if("Poppins" %not_in% sysfonts::font_files()$family){
    sysfonts::font_add_google(name="Poppins", family="Poppins")
  }else{
    sysfonts::font_add(family = "Poppins",
                       regular="C:/Windows/Fonts/Poppins-Regular.ttf",
                       bold = "C:/Windows/Fonts/Poppins-Bold_0.ttf",
                       italic = "C:/Windows/Fonts/Poppins-Italic.ttf",
                       bolditalic = "C:/Windows/Fonts/Poppins-BoldItalic_0.ttf")
  }
  showtext::showtext_auto()
}