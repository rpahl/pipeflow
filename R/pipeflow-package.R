#' @keywords internal
#' @useDynLib pipeflow, .registration = TRUE
#' @importFrom Rcpp evalCpp
"_PACKAGE"


.env <- new.env(parent = emptyenv())

.step_states <- new.env(parent = emptyenv())
.step_states[["new"]] <- c(name = "new", color = "#47b8ffff")
.step_states[["done"]] <- c(name = "done", color = "#87ff6fff")
.step_states[["outdated"]] <- c(name = "outdated", color = "#ffa040ff")
.step_states[["failed"]] <- c(name = "failed", color = "#ff4c4cff")


.this_package_name <- function() {
    methods::getPackageName()
}


.this_package_path <- function(...) {
    system.file(package = .this_package_name()) |>
        file.path(...) |>
        normalizePath(winslash = "/", mustWork = FALSE)
}
