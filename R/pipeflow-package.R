#' @keywords internal
#' @useDynLib pipeflow, .registration = TRUE
#' @import data.table
#' @importFrom Rcpp evalCpp
"_PACKAGE"


# data.table group-by expressions (`.` and the column names in `j`) are
# non-standard evaluation and are not visible to codetools.
utils::globalVariables(c(".", "step", "out"))


.env <- new.env(parent = emptyenv())

.step_states <- new.env(parent = emptyenv())
.step_states[["new"]] <- c(name = "new", color = "#47b8ffff")
.step_states[["done"]] <- c(name = "done", color = "#87ff6fff")
.step_states[["outdated"]] <- c(name = "outdated", color = "#ffa040ff")
.step_states[["failed"]] <- c(name = "failed", color = "#ff4c4cff")
