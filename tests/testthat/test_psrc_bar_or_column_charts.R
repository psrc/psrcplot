
test_that("static charts look correct", {
  mode_shares <- psrcplot::mode_share_example_data %>% 
    filter(Category=="Mode to Work by Race") %>%
    filter(Geography=="Region" & Race=="Total") %>%
    mutate(Year = as.character(Year))
  
  # Static Chart         
  modes_chart <- static_bar_chart(t=mode_shares, y="Mode",x="share", fill="Year",
                                  title="Mode Share to Work",
                                  alt="Chart of Work Mode Shares",
                                  source=paste("Source: ACS 5-Year Estimates, table B3002",
                                               "for King, Kitsap, Pierce and Snohomish counties.",
                                               sep = "\n"),
                                  color="pgnobgy_10")
  
  expect_doppelganger('example-mode-shares-bar-chart', modes_chart)
  
  modes_chart <- static_column_chart(t=mode_shares, x="Mode",y="share", fill="Year",
                                  title="Mode Share to Work",
                                  alt="Chart of Work Mode Shares",
                                  source=paste("Source: ACS 5-Year Estimates, table B3002",
                                               "for King, Kitsap, Pierce and Snohomish counties.",
                                               sep = "\n"),
                                  color="psrc_dark")
  
  expect_doppelganger('example-mode-shares-column-chart', modes_chart)
  
})

