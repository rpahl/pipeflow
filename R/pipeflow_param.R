
# -----
# Param
# -----
methods::setClass("Param",
    contains = "VIRTUAL",
    slots = c(
        name = "character",
        value = "ANY",
        advanced = "logical",
        label = "character",
        description = "character",
        source = "character"
    )
)

methods::setValidity("Param",
function(object)
{
    stopifnot(
        is_string(object@name),
        !is.na(object@name),
        nchar(object@name) > 0,
        length(object@advanced) == 1,
        is_string(object@label),
        is_string(object@description)
    )
    TRUE
})

methods::setMethod("initialize", "Param",
function(
    .Object,
    name,
    value,
    advanced = FALSE,
    label = name,
    description = "",
    source = "internal",
    ...
) {
    .Object@name <- name
    .Object@value <- value
    .Object@advanced <- advanced
    .Object@label <- label
    .Object@description <- description
    .Object@source <- source

    methods::validObject(.Object)
    .Object
})



# ------------
# BooleanParam
# ------------
methods::setClass("BooleanParam",
    slots = c(value = "logical"),
    contains = "Param"
)

methods::setValidity("BooleanParam",
function(object)
{
    stopifnot(
        length(object@value) <= 1
    )
    TRUE
})

methods::setMethod("initialize", "BooleanParam",
function(.Object, name, value = TRUE, ...)
{
    value <- if (is.null(value)) {
        as.logical(NA)
    } else {
        as.logical(value)
    }

    methods::callNextMethod(.Object, name = name, value = value, ...)
})

BooleanParam <- function(...) methods::new("BooleanParam", ...)



# ----------------
# CategoricalParam
# ----------------
methods::setClass("CategoricalParam",
    slots = c(value = "character", choices = "character"),
    contains = "Param"
)

methods::setValidity("CategoricalParam",
function(object)
{
    stopifnot(
        is_string(object@value),
        is.character(object@choices),
        length(object@choices) > 0,
        object@value %in% object@choices
    )
    TRUE
})

methods::setMethod("initialize", "CategoricalParam",
function(
    .Object,
    name,
    value = as.character(NA),
    choices = as.character(NA),
    ...
) {
    value <- if (is.null(value)) {
        as.character(NA)
    } else {
        as.character(value)
    }

    .Object@choices <- choices

    .Object <- methods::callNextMethod(.Object, name = name, value = value, ...)
    .Object
})

CategoricalParam <- function(...) methods::new("CategoricalParam", ...)



# --------------
# DataframeParam
# --------------
methods::setClass("DataframeParam",
    slots = c(value = "data.frame"),
    contains = "Param"
)

methods::setValidity("DataframeParam",
function(object)
{
    TRUE
})

methods::setMethod("initialize", "DataframeParam",
function(.Object, name, value = data.frame(), ...)
{
    value <- as.data.frame(value)

    methods::callNextMethod(.Object, name = name, value = value, ...)
})

DataframeParam <- function(...) methods::new("DataframeParam", ...)



# ---------
# ListParam
# ---------
methods::setClass("ListParam",
    slots = c(value = "list"),
    contains = "Param"
)

methods::setValidity("ListParam",
function(object)
{
    stopifnot(is.list(object@value) || is.null(object@value))
    TRUE
})

methods::setMethod("initialize", "ListParam",
function(.Object, name, value = list(), ...)
{
    if (length(value) == 0 || (length(value) == 1 && is.na(value))) {
        value = list()
    }

    methods::callNextMethod(.Object, name = name, value = value, ...)
})

ListParam <- function(...) methods::new("ListParam", ...)



# ------------
# NumericParam
# ------------
methods::setClass("NumericParam",
    slots = c(
        value = "numeric",
        min = "numeric",
        max = "numeric"
    ),
    contains = "Param"
)

methods::setValidity("NumericParam",
function(object)
{
    stopifnot(
        is_number(object@value),
        is_number(object@min),
        is_number(object@max),
        object@min <= object@max
    )

    if (!is.na(object@value)) {
        stopifnot(
            object@value >= object@min,
            object@value <= object@max
        )
    }

    TRUE
})

methods::setMethod("initialize", "NumericParam",
function(
    .Object,
    name,
    value = as.numeric(NA),
    min = .Machine$double.xmin,
    max = .Machine$double.xmax,
    ...
) {
    value = if (is.null(value)) {
        as.numeric(NA)
    } else {
        as.numeric(value)
    }

    .Object@min <- min
    .Object@max <- max
    .Object = methods::callNextMethod(.Object, name = name, value = value, ...)
    .Object
})

NumericParam <- function(...) methods::new("NumericParam", ...)



# -----------
# StringParam
# -----------
methods::setClass("StringParam",
    slots = c(value = "character"),
    contains = "Param"
)

methods::setValidity("StringParam",
function(object)
{
    stopifnot(is_string(object@value))
    TRUE
})

methods::setMethod("initialize", "StringParam",
function(.Object, name, value = as.character(NA), ...)
{
    value = if (is.null(value))
        as.character(NA)
    else as.character(value)

    methods::callNextMethod(.Object, name = name, value = value, ...)
})

StringParam <- function(...) methods::new("StringParam", ...)





param_list_to_json = function(x)
{
    params <- lapply(x, function(p) as.list(attributes(eval(p))))

    # Set call arg names as param names and clean to have unnamed json elements
    for (name in names(params)) {
        params[[name]][["name"]] <- name
    }
    names(params) <- NULL

    jsonlite::toJSON(params, auto_unbox = TRUE, pretty = TRUE)
}


param_list_from_json <- function(x)
{
    stopifnot(inherits(x, "json"))

    dat = jsonlite::fromJSON(x)

    create_Param = function(class, ...) {
        methods::new(class, ...)
    }

    extract_ith_param <- function(i) {
        li <- as.list(dat[i, ])
        if (is.list(li[["value"]])) {
            li[["value"]] <- li[["value"]][[1]]
        }

        do.call(create_Param, args = li)
    }

    paramList <- lapply(seq_len(nrow(dat)), extract_ith_param)
    names(paramList) <- dat[, "name"]
    paramList
}
