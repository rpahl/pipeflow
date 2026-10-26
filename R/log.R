.formatted_time <- function(time = Sys.time()) {
    format(
        time,
        usetz = TRUE,
        tz = "UTC",
        format = "%Y-%m-%d %H:%M:%OS",
        digits = 3
    )
}


#' Format a duration for the log and for print()
#'
#' Durations below a minute are shown in seconds with milliseconds, longer ones in
#' minutes and seconds, or in hours and minutes.
#'
#' @param duration A `difftime` or a number of seconds.
#' @return A single string, e.g. `"0.251 s"`, `"2 min 5 s"` or `"1 h 3 min"`.
#' @noRd
.formatted_duration <- function(duration) {
    secs <- as.numeric(duration, units = "secs")
    if (secs < 60) {
        return(sprintf("%.3f s", secs))
    }
    secs <- round(secs)
    if (secs < 3600) {
        return(sprintf("%d min %d s", secs %/% 60, secs %% 60))
    }
    mins <- round(secs / 60)
    sprintf("%d h %d min", mins %/% 60, mins %% 60)
}


#' Default logger of pipeflow
#'
#' The logging function that [pip_run()] uses by default. It writes one line per message
#' to the console in the form `<level> [<UTC time stamp>]: <msg>`, for example
#' `info [2026-01-01 12:34:56.789 UTC]: Step 1/2 load`.
#'
#' It is exported so that custom loggers can build on it, for example to add a prefix,
#' or to drop some levels and pass the rest on to the default logger. A logger is any
#' function with the arguments `level` and `msg`. [pip_run()] calls it as
#' `lgr(level = level, msg = msg)`.
#'
#' @param level The log level, one of `"info"`, `"warn"` or `"error"`.
#' @param msg The message, a single string.
#' @return `NULL`, invisibly.
#' @seealso [pip_run()] for how loggers are used and how to switch logging off with
#' `lgr = NULL`.
#' @examples
#' pipeflow_lgr("info", "Hello")
#'
#' p <- pip_new("demo") |>
#'     pip_add("load", \(n = 3) seq_len(n)) |>
#'     pip_add("double", \(x = ~load) x * 2)
#'
#' # Add a prefix to every message
#' prefixed <- function(level, msg) {
#'     pipeflow_lgr(level, paste("[demo]", msg))
#' }
#' pip_run(p, lgr = prefixed)
#'
#' # Drop the "info" messages and pass the rest on
#' quiet <- function(level, msg) {
#'     if (level != "info") {
#'         pipeflow_lgr(level, msg)
#'     }
#' }
#' pip_run(p, lgr = quiet, force = TRUE)
#' @export
pipeflow_lgr <- function(level, msg) {
    cat(level, " [", .formatted_time(), "]: ", msg, "\n", sep = "")
}
