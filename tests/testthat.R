library(testthat)
library(pipeflow)
lgr::suspend_logging()

test_check("pipeflow")

lgr::unsuspend_logging()
