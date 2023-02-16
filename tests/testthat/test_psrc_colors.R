
test_that("there are at least 21 color groups in psrc_colors", {
  expect_gte(length(psrc_colors), 21)
})