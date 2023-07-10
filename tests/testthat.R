library(testthat)
library(pipeflow)


options(Ncpus = 6)

test_check(
    "pipeflow",
    reporter = JunitReporter$new(file = "test-result.xml")
)
