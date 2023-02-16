
test_that("static treemap charts look correct", {
  df <- psrcplot::mode_share_example_data %>% 
    filter(Category=="Mode to Work by Race") %>%
    filter(Geography=="Region" & Race=="Total") %>%
    mutate(Year = as.character(Year)) %>%
    filter(Year=="2020")
  
  my.chart <- create_treemap_chart(t=df, s="share", fill="Mode", title="Mode Share to Work")
    mode_shares <- psrcplot::mode_share_example_data %>% 
      filter(Category=="Mode to Work by Race") %>%
      filter(Geography=="Region" & Race=="Total") %>%
      mutate(Year = as.character(Year))
    
  expect_doppelganger('example-psrc-treemap-chart', my.chart) 
})