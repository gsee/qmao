context("makePriceFrame")

ibak <- as.list(FinancialInstrument:::.instrument, all.names=TRUE)
rm_instruments(keep=FALSE)
data(sample_matrix)
set.seed(42)
dat <- new.env()
with(dat, {
  SPY <- as.xts(sample_matrix)
  ESH3 <- reclass(10 * sweep(SPY, 1, cumsum(runif(n=nrow(SPY), 0, 0.03))), SPY)
})
     
test_that("undefined gives warning", {
  expect_warning(PF(c("SPY", "ESH3"), env=dat), "SPY not found")
  expect_warning(PF(c("SPY", "ESH3"), env=dat), "ESH3 not found")
})

stock("SPY", currency("USD"))
future("ES", "USD", 50)
future_series("ESH3")

test_that("notional", {
  p <- PF(c("SPY", "ESH3"), notional=TRUE, env=dat)
  expect_equal(round(as.numeric(last(p)), 2), c(47.77, 22458.84))
  expect_equal(round(as.numeric(first(p)), 2), c(50.12, 25045.17))
})

test_that("no notional", {
  p <- PF(c("SPY", "ESH3"), notional=FALSE, env=dat)
  expect_equal(round(as.numeric(last(p)), 2), c(47.77, 449.18))
  expect_equal(round(as.numeric(first(p)), 2), c(50.12, 500.90))
})

rm(dat)
reloadInstruments(ibak)
