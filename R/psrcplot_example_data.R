#' Mode share example data included in psrcplot
#'
"mode_share_example_data"

#' Transit access example data included in psrcplot
#'
"transit_access_example_data"

#' Summary PUMS example data included in psrcplot
#' 
#' see frequently-asked-questions.Rmd vignette for script
#'
"summary_pums_example_data"

#' Airport example data included in psrcplot
#'
#' SELECT ad.loc_id, ad.airport_name, ef.data_year, ef.enplanements
#' FROM Elmer.faa.airport_dims AS ad JOIN Elmer.faa.enplanement_facts AS ef 
#' ON ad.airport_dim_id=ef.airport_dim_id
#' WHERE ad.st='WA' AND ad.airport_dim_id IN(657,671)
#' ORDER BY ad.airport_name, ef.data_year;
#'
"enplanement_example_data"