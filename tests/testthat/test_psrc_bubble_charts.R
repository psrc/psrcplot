
test_that("static bubble chart looks correct", {
  mode_shares <- psrcplot::mode_share_example_data %>% 
    dplyr::filter(Category=="Mode to Work by Race") %>%
    dplyr::filter(Geography=="Region" & Race=="Total") %>%
    dplyr::mutate(Year = as.character(Year))
  
  bubble_chart <- create_bubble_chart(t=mode_shares, x="Mode", y='share', fill='Year',
                            s='count',
                            title='This is a test title',
                            subtitle='This is a sample subtitle' )
  
  vdiffr::expect_doppelganger('example-mode-shares-bubble-chart', bubble_chart)
})