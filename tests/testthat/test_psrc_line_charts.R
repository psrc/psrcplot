
test_that("static generic line charts look correct", {
    
  
  df <- psrcplot::mode_share_example_data %>%
    dplyr::filter(Geography=='King')
  
  
  my.line.chart <- generic_line(t=df, x='Year', y='count', fill='Race',
                                est='number',
                                title='test line chart',
                                subtitle='for consistency checking in unit tests',
                                color=psrcplot::psrc_colors$pgnobgy_10)
  
  vdiffr::expect_doppelganger('example-psrc-generic-line-chart', my.line.chart) 
})