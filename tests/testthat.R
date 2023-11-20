library(testthat)
library(pipeflow)
lgr::suspend_logging()

options(Ncpus = 6)

test_check("pipeflow")

lgr::unsuspend_logging()
