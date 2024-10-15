
test_that("static charts look correct", {
  mode_shares <- psrcplot::mode_share_example_data %>% 
    dplyr::filter(Category=="Mode to Work by Race") %>%
    dplyr::filter(Geography=="Region" & Race=="Total") %>%
    dplyr::mutate(Year = as.character(Year))
  
  # Static Chart         
  modes_chart <- static_bar_chart(t=mode_shares, y="Mode",x="share", fill="Year",
                                  title="Mode Share to Work",
                                  alt="Chart of Work Mode Shares",
                                  source=paste("Source: ACS 5-Year Estimates, table B3002",
                                               "for King, Kitsap, Pierce and Snohomish counties.",
                                               sep = "\n"),
                                  color=psrcplot::psrc_colors$pgnobgy_10)
  
  vdiffr::expect_doppelganger('example-mode-shares-bar-chart', modes_chart)
  
  modes_chart <- static_column_chart(t=mode_shares, x="Mode",y="share", fill="Year",
                                  title="Mode Share to Work",
                                  alt="Chart of Work Mode Shares",
                                  source=paste("Source: ACS 5-Year Estimates, table B3002",
                                               "for King, Kitsap, Pierce and Snohomish counties.",
                                               sep = "\n"),
                                  color=psrcplot::psrc_colors$pgnobgy_10,
                                  dec = 0,
                                  xlabel = 'Mode to Work')
  
  vdiffr::expect_doppelganger('example-mode-shares-column-chart', modes_chart)
  
  modes_column_chart_moe <- static_column_chart(t=mode_shares, x="Mode",y="count", fill="Year",
                                                alt="Chart of Workers by Mode",
                                                source=paste("Source: U.S. Census Bureau, ACS 5-Year Estimates, table B3002",
                                                             "for King, Kitsap, Pierce and Snohomish counties.",
                                                             sep = "\n"),
                                                color=psrcplot::psrc_colors$pgnobgy_5,
                                                est = "number",
                                                moe = "count_moe",
                                                xlabel = "Mode to Work",
                                                ylabel = "Total Workers")
  
  vdiffr::expect_doppelganger('static-modes-column-chart-with-moe', modes_column_chart_moe)
  
  modes_bar_chart_moe <- static_bar_chart(t=mode_shares, x="share", y="Mode", fill="Year",
                                          alt="Chart of Work Mode Shares",
                                          source=paste("Source: U.S. Census Bureau, ACS 5-Year Estimates, table B3002",
                                                       "for King, Kitsap, Pierce and Snohomish counties.",
                                                       sep = "\n"),
                                          color=psrcplot::psrc_colors$obgnpgy_5,
                                          moe = "share_moe",
                                          xlabel = "Share of Workers",
                                          ylabel = "Mode to Work")
  
  vdiffr::expect_doppelganger('static-modes-bar-chart-with-moe', modes_bar_chart_moe)
  
})

test_that('rtp charts look correct', {
  rtp_data<- read.csv(file='../rtp-dashboard-data.csv')
  rtp_data <- rtp_data %>%
    dplyr::mutate(data_year = as.character(lubridate::year(date))) %>% 
    dplyr::filter(metric=="1yr Fatality Rate" & geography_type=="PSRC Region" & variable%in%c("Fatal Collisions") & data_year >= 2015)
  
  rtp_static_facet_column_chart <- static_facet_column_chart(t=rtp_data, 
                            x="data_year", y="estimate", 
                            fill="data_year", facet="geography", 
                            est = "number",
                            color = psrcplot::psrc_colors$pgnobgy_10,
                            title="Fatal Collisions by County: 2015 to 2020",
                            ncol=2, scales="fixed")
  
  vdiffr::expect_doppelganger('static-rtp-facet-column-chart',rtp_static_facet_column_chart)
})

# test_that('interactive charts look correct', {
#   interactive_modes_column_chart <- interactive_column_chart(t=mode_shares, x="Mode",y="share", fill="Year",
#                                                              est = "percent",
#                                                              title="Mode Share to Work",
#                                                              subtitle="For People 16+ with a job",
#                                                              alt="Chart of Work Mode Shares",
#                                                              source=paste("Source: U.S. Census Bureau, ACS 5-Year Estimates, table B3002"),
#                                                              color="pgnobgy_5",
#                                                              dec = 0)
#   
#   #interactive_modes_column_chart
#   vdiffr::expect_doppelganger('interactive-modes-column-chart', interactive_modes_column_chart)
#   
# })