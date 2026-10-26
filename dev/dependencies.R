# Development tools used outside the package code (CI, AGENTS.md). Not part
# of the package: this file only lets renv::snapshot() see the dependencies.
library(covr)
library(devtools)
library(DT) # required for covr::report()
library(htmltools) # required for covr::report()
library(gridExtra)
library(lintr)
library(mockery)
library(rcmdcheck)
