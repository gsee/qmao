#context("getEarnings")

# getEarnings isn't working because Reuters doesn't like you

#test_that("return.class of xts", {
#  earn <- getEarnings("AAPL")
#  expect_identical(colnames(earn), c("EPS.ESTIMATE", "EPS.ACTUAL", "PREV.YEAR.ACTUAL"))
#  expect_match(earn$SYMBOL[1], "AAPL")
#})

#test_that("return.class of data.frame", {
#  earn <- getEarnings("AAPL", return.class="data.frame")
#  expect_identical(colnames(earn), 
#                   c("SYMBOL", "PERIOD", "EVENT.TITLE", "EPS.ESTIMATE", 
#                     "EPS.ACTUAL", "PREV.YEAR.ACTUAL", "TIME"))
#  expect_match(earn$SYMBOL[1], "AAPL")
#})


