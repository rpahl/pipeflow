.formatted_time <- function(time = Sys.time()) {
    format(
        time,
        usetz = TRUE,
        tz = "UTC",
        format = "%Y-%m-%d %H:%M:%OS",
        digits = 3
    )
}


pipeflow_lgr <- function(level, msg) {
    cat(level, " [", .formatted_time(), "]: ", msg, "\n", sep = "")
}
