
test_that("static treemap charts look correct", {
  df <- psrcplot::mode_share_example_data %>% 
    dplyr::filter(Category=="Mode to Work by Race") %>%
    dplyr::filter(Geography=="Region" & Race=="Total") %>%
    dplyr::mutate(Year = as.character(Year)) %>%
    dplyr::filter(Year=="2020")
  
  my.chart <- create_treemap_chart(t=df, s="share", fill="Mode", title="Mode Share to Work")
    mode_shares <- psrcplot::mode_share_example_data %>% 
      dplyr::filter(Category=="Mode to Work by Race") %>%
      dplyr::filter(Geography=="Region" & Race=="Total") %>%
      dplyr::mutate(Year = as.character(Year))
    
  vdiffr::expect_doppelganger('example-psrc-treemap-chart', my.chart) 
})