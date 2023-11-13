.get_formatted_time <- function(time = Sys.time()) {
    local <- time
    gmt <- strptime(
        as.POSIXlt(time, "GMT"),
        format = "%Y-%m-%d %H:%M:%S"
    )
    hours_diff <- round(as.numeric(difftime(local, gmt, units = "hours")))

    sign_str <- if (sign(hours_diff) > 0) "+" else "-"
    number_str <- paste0(toString(hours_diff), ":00")

    paste0(format(local, format = "%Y-%m-%dT%H:%M:%S"), sign_str, number_str)
}


#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON
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


.log <- function(
    msg, ...,
    level = "info",
    logger = lgr::get_logger(name = methods::getPackageName())
) {
    logfun <- logger[[level]]
    logfun(msg, ...)
}

log_info <- function(msg, ...) {
    .log(msg, level = "info", ...)
}

log_warn <- function(msg, ...) {
    .log(msg, level = "warn", ...)
}

log_error <- function(msg, ...) {
    .log(msg, level = "error", ...)
}


tryCatchLog <- function(
    expr,
    ...,
    execution_context = NULL,
    finally = NULL,
    silent_warnings = TRUE,
    silent_messages = TRUE
) {
    condition_handler <- function(cond) {
        cond_message <- cond[["message"]]
        msg = cond_message

        if (!is.null(execution_context)) {
            msg = paste0("Context: ", execution_context, ", ", msg)
        }

        supported_conditions = c("message", "warning", "error")

        severity <- supported_conditions |>
            sapply(FUN = function(x) inherits(cond, x)) |>
            Filter(f = isTRUE) |>
            names()

        if (length(severity) == 0) {
            stop(sprintf("Unsupported condition %s", toString(class(cond))))
        }

        switch(
            severity[1],
            "message" = log_info(msg),
            "warning" = log_warn(msg, warn = cond_message),
            "error" = log_error(msg, error = cond_message),
            stop(sprintf("Unsupported severity level %s", severity))
        )

        if (silent_warnings && inherits(cond, "warning")) {
            invokeRestart("muffleWarning")
        }

        if (silent_messages && inherits(cond, "message")) {
            invokeRestart("muffleMessage")
        }
    }

    tryCatch(
        withCallingHandlers(expr, condition = condition_handler),
        ...,
        finally = finally
    )
}


#' @title Set pipeflow log layout
#' @description Set pipeflow log layout
#' @param layout Layout name
#' @return invisibly returns logger object
#' @importFrom lgr get_logger
#' @export
#' @examples
#' lg <- set_log_layout("json")
#' print(lg)
#' set_log_layout("text")
set_log_layout <- function(layout)
{
    selected_layout <- switch(
        layout,
        "text" = lgr::LayoutFormat$new(),
        "json" = LogLayoutJson$new(),
        stop("unknown log layout '", layout, "'")
    )

    lg = lgr::get_logger(name = .this_package_name())

    lg$config(
        list(
            threshold = "info",
            propagate = FALSE,
            appenders = lgr::AppenderConsole$new(layout = selected_layout)
        )
    )

    invisible(lg)
}
