
.get_formatted_time <- function(time = Sys.time())
{
    local <- time
    gmt <- strptime(
        as.POSIXlt(time, tz = "GMT"),
        format = "%Y-%m-%d %H:%M:%S"
    )
    hours_diff <- round(as.numeric(difftime(local, gmt, units = "hours")))

    sign_str <- if (sign(hours_diff) > 0) "+" else "-"
    number_str <- paste0(toString(abs(hours_diff)), ":00")

    paste0(format(local, format = "%Y-%m-%dT%H:%M:%S"), sign_str, number_str)
}


#' @importFrom R6 R6Class
LogLayoutJson <- R6::R6Class(
    "LogLayoutJson",
    inherit = lgr::LayoutJson,
    public = list(
        format_event = function(event) {
            default_fields <- list(
                "application" = event$logger,
                "level" = unname(event$level_name),
                "time" = .get_formatted_time(event$timestamp),
                "message" = event$msg
            )

            custom_names = setdiff(
                names(event$values),
                c("msg", "timestamp", "level", "caller", "logger")
            )

            log_fields = c(default_fields, event$values[custom_names])

            do.call(
                jsonlite::toJSON,
                args = c(list(x = log_fields), get(".toJSON_args", private))
            )
        }
    )
)


#' @title Set pipeflow log layout
#' @description This function provides an easy way to set the basic log
#' layout of the pipeline logging. For a fine-grained control of the logger,
#' which you can retrieve via `lgr::get_logger("pipeflow")`, see e.g. the
#' \link[lgr]{logger_config} function from the \link[lgr]{lgr} package.
#' @param layout Layout name, which at this point can be either 'text' or
#' 'json'.
#' @return invisibly returns a `Logger` object
#' @importFrom lgr get_logger
#' @export
#' @examples
#' p <- Pipeline$new("pipe", data = 1:2)
#' p$add("add1", \(data = ~data, x = 1) x + data)
#' p$run()
#'
#' lg <- set_log_layout("json")
#' print(lg)
#'
#' p$run()
#'
#' set_log_layout("text")
#' p$run()
set_log_layout <- function(layout = c("text", "json"))
{
    selected_layout <- switch(
        layout[1],
        "text" = lgr::LayoutFormat$new(),
        "json" = LogLayoutJson$new(),
        stop("unknown log layout '", layout, "'")
    )

    lg <- lgr::get_logger(name = .this_package_name())

    lg$config(
        list(
            threshold = "info",
            propagate = FALSE,
            appenders = lgr::AppenderConsole$new(layout = selected_layout)
        )
    )

    invisible(lg)
}
