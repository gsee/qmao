context(".getDividendCalendar")

test_that("file downloads correctly", {
  dc <- .getDividendsCalendar()
  expect_identical(colnames(dc), 
        c("SYMBOL", "COMPANY", "AMOUNT", "EX-DATE", "PAYABLE", "RECORD", "DECLARATION"))
  expect_identical(sapply(dc, class),
                   structure(c("character", "character", "numeric", "Date", "Date", "Date", "Date"), 
                             .Names = c("SYMBOL", "COMPANY", "AMOUNT", "EX-DATE", "PAYABLE", "RECORD", "DECLARATION")))

})

