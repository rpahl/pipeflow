
# Pipeline

test_that("initialize",
{
    expect_true(is.function(Pipeline$new("pipe")$initialize))

    test_that("returns a pipeline object",
    {
        expect_true(methods::is(Pipeline$new("pipe"), "Pipeline"))
    })

    test_that("pipeline name must be a non-empty string",
    {
        expect_no_error(Pipeline$new("foo"))

        expect_error(
            Pipeline$new(name = 1),
            "name must be a string"
        )

        expect_error(
            Pipeline$new(name = ""),
            "name must not be empty"
        )
    })

    test_that("data is added as first step to pipeline",
    {
        pip <- Pipeline$new("pipe1", data = 1)

        expect_equal(pip$get_step_names(), "data")

        out <- pip$run()$collect_out(all = TRUE)
        expect_equal(pip$get_out("data"), 1)
    })

    test_that("the logger can be customized",
    {
        my_logger <- function(level, msg, ...) {
            message("My Logger: ", msg)
        }

        pip <- Pipeline$new("pipe", logger = my_logger)

        out <- capture.output(
            pip$run(),
            type = "message"
        )
        expect_equal(
            out,
            c(
                "My Logger: Start run of 'pipe' pipeline:",
                "My Logger: Step 1/1 data",
                "My Logger: Finished execution of steps.",
                "My Logger: Done."
            )
        )
    })

    test_that("bad definition of the custom logger is signalled",
    {
        expected_error_msg <- paste(
            "logger function must have the following signature:",
            "function(level, msg, ...)"
        )


        logger_with_missing_level_arg <- function(msg, ...) {
            message("My Logger: ", msg)
        }
        expect_error(
            Pipeline$new("pipe1", logger = logger_with_missing_level_arg),
            expected_error_msg,
            fixed = TRUE
        )


        logger_with_missing_msg_arg <- function(level, ...) {
            message("My Logger: ", ...)
        }
        expect_error(
            Pipeline$new("pipe1", logger = logger_with_missing_msg_arg),
            expected_error_msg,
            fixed = TRUE
        )


        logger_with_missing_dots <- function(msg, level) {
            message("My Logger: ", msg)
        }
        expect_error(
            Pipeline$new("pipe1", logger = logger_with_missing_dots),
            expected_error_msg,
            fixed = TRUE
        )


        logger_with_additional_arg <- function(level, msg, foo, ...) {
            message("My Logger: ", msg)
        }
        expect_error(
            Pipeline$new("pipe1", logger = logger_with_additional_arg),
            expected_error_msg,
            fixed = TRUE
        )
    })
})



test_that("add",
{
    expect_true(is.function(Pipeline$new("pipe")$add))

    test_that("step must be non-empty string",
    {
        pip <- Pipeline$new("pipe1")

        foo <- function(a = 0) a

        expect_error(pip$add("", foo))
        expect_error(pip$add(c("a", "b"), foo))
    })

    test_that("fun must be passed as a function or string",
    {
        pip <- Pipeline$new("pipe")

        expect_error(
            pip$add("step1", fun = 1),
            "is.function(fun) || is_string(fun) is not TRUE",
            fixed = TRUE
        )
    })

    test_that("params must be a list",
    {
        pip <- Pipeline$new("pipe")

        expect_error(
            pip$add("step1", fun = function() 1, params = 1),
            "is.list(params)",
            fixed = TRUE
        )
    })

    test_that("description must be (possibly empty) string",
    {
        pip <- Pipeline$new("pipe")

        expect_error(
            pip$add("step1", fun = function() 1, description = 1),
            "is_string(description)",
            fixed = TRUE
        )
        expect_no_error(pip$add("step1", fun = function() 1, description = ""))
    })

    test_that("group must be non-empty string",
    {
        pip <- Pipeline$new("pipe")

        expect_error(
            pip$add("step1", fun = function() 1, group = 1),
            "is_string(group) && nzchar(group) is not TRUE",
            fixed = TRUE
        )
        expect_error(
            pip$add("step1", fun = function() 1, group = ""),
            "is_string(group) && nzchar(group) is not TRUE",
            fixed = TRUE
        )
    })

    test_that("keepOut must be logical",
    {
        pip <- Pipeline$new("pipe")

        expect_error(
            pip$add("step1", fun = function() 1, keepOut = 1),
            "is.logical(keepOut) is not TRUE",
            fixed = TRUE
        )
    })

    test_that("duplicated step names are signaled",
    {
        pip <- Pipeline$new("pipe1")

        foo <- function(a = 0) a

        pip$add("f1", foo)
        expect_error(pip$add("f1", foo), "step 'f1' already exists")
        expect_error(pip$add("f1", function(x) x), "step 'f1' already exists")
    })

    test_that("missing dependencies are signaled",
    {
        pip <- Pipeline$new("pipe1")

        foo <- function(a = 0) a

        pip$add("f1", foo)
        expect_error(
            pip$add("f2", foo, params = list(a = ~undefined)),
            "dependency 'undefined' not found"
        )
    })

    test_that("step can refer to previous step by relative number",
    {
        pip <- Pipeline$new("pipe1")
        pip$add("f1", function(a = 5) a)
        pip$add("f2", function(x = ~-1) 2*x, keepOut = TRUE)

        out = pip$run()$collect_out()
        expect_equal(out[["f2"]][[1]], 10)

        pip$add("f3", function(x = ~-1, a = ~-2) x + a, keepOut = TRUE)
        out = pip$run()$collect_out()
        expect_equal(out[["f3"]][[1]], 10 + 5)
    })

    test_that("a bad relative step referal is signalled",
    {
        pip <- Pipeline$new("pipe1")
        expect_error(
            pip$add("f1", function(x = ~-10) x),
            paste(
                "step 'f1': relative dependency x=-10",
                "points to outside the pipeline"
            ),
            fixed = TRUE
        )
    })

    test_that("added step can use lambda functions",
    {
        data <- 9
        pip <- Pipeline$new("pipe1", data = data)

        pip$add("f1", function(data = ~data) data, keepOut = TRUE)
        a <- 1
        pip$add("f2", function(a, b) a + b,
            params = list(a = a, b = ~f1),
            keepOut = TRUE
        )

        expect_equal(unlist(pip$get_step("f1")[["depends"]]), c(data = "data"))
        expect_equal(unlist(pip$get_step("f2")[["depends"]]), c(b = "f1"))

        out <- pip$run()$collect_out()
        expect_equal(out[["f1"]][[1]], data)
        expect_equal(out[["f2"]][[1]], a + data)
    })


    test_that(
        "supports functions with wildcard arguments",
    {
        my_mean <- function(x, na.rm = FALSE) {
            mean(x, na.rm = na.rm)
        }
        foo <- function(x, ...) {
            my_mean(x, ...)
        }
        v <- c(1, 2, NA, 3, 4)
        pip <- Pipeline$new("pipe", data = v)

        params <- list(x = ~data, na.rm = TRUE)
        pip$add("mean", fun = foo, params = params, keepOut = TRUE)

        out <- pip$run()$collect_out()
        expect_equal(out[["mean"]], mean(v, na.rm = TRUE))

        pip$set_params_at_step("mean", list(na.rm = FALSE))
        out <- pip$run()$collect_out()
        expect_equal(out[["mean"]], as.numeric(NA))
    })

    test_that("can have a variable defined outside as parameter default",
    {
        x <- 1

        pip <- Pipeline$new("pipe") |>
            pipe_add("f1", function(a) a, params = list(a = x))

        expect_equal(pip$get_params_at_step("f1")$a, x)

        out <- pip$run()$collect_out(all = TRUE)
        expect_equal(out[["f1"]], x)
    })

    test_that("handles Param object args",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("f1", function(a = new("NumericParam", "a", value = 1)) a)

        out <- pip$run()$collect_out(all = TRUE)
        expect_equal(out[["f1"]], 1)
    })

    test_that(
        "can have a Param object defined outside as parameter default",
    {
        x <- 1
        p <- new("NumericParam", "a", value = x)

        pip <- Pipeline$new("pipe") |>
            pipe_add("f1", function(a) a, params = list(a = p))

        expect_equal(pip$get_params_at_step("f1")$a, p)

        out <- pip$run()$collect_out(all = TRUE)
        expect_equal(out[["f1"]], x)
    })

    test_that(
        "function can be passed as a string",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("f1", fun = "mean", params = list(x = 1:5))

        out <- pip$run()$collect_out(all = TRUE)
        expect_equal(out[["f1"]], mean(1:5))

        expect_equal(pip$get_step("f1")[["funcName"]], "mean")
    })

    test_that(
        "if passed as a function, name is derived from the function",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("f1", fun = mean, params = list(x = 1:5))

        expect_equal(pip$get_step("f1")[["funcName"]], "mean")
    })
})


test_that("append",
{
    expect_true(is.function(Pipeline$new("pipe")$append))

    test_that("pipelines can be combined even if their steps share names,
        unless tryAutofixNames is FALSE",
    {
        pip1 <- Pipeline$new("pipe1", data = 1) |>
            pipe_add("f1", function(a = 1) a, keepOut = TRUE) |>
            pipe_add("f2", function(b = ~f1) b)

        pip2 <- Pipeline$new("pipe2") |>
            pipe_add("f1", function(a = 10) a) |>
            pipe_add("f2", function(b = ~f1) b, keepOut = TRUE)

        expect_error(
            pip1$append(pip2, tryAutofixNames = FALSE),
            paste(
                "combined pipeline would have duplicated step names:",
                "'data', 'f1', 'f2',"
            )
        )

        pp <- pip1$append(pip2)
        expect_equal(pp$length(), pip1$length() + pip2$length())

        out1 <- pip1$run()$collect_out()
        out2 <- pip2$run()$collect_out()

        out <- pp$run()$collect_out()

        expect_equivalent(out, c(out1, out2))
    })

    test_that("auto-fixes only the names that need auto-fix",
    {
        pip1 <- Pipeline$new("pipe1", data = 1) |>
            pipe_add("f1", function(a = 1) a, keepOut = TRUE) |>
            pipe_add("f2", function(b = ~f1) b)

        pip2 <- Pipeline$new("pipe2") |>
            pipe_add("f3", function(a = 10) a) |>
            pipe_add("f4", function(b = ~f3) b, keepOut = TRUE)

        pp <- pip1$append(pip2)
        expect_equal(
            pp$get_step_names(),
            c("data", "f1", "f2", "data.pipe2", "f3", "f4")
        )

        out1 <- pip1$run()$collect_out()
        out2 <- pip2$run()$collect_out()

        out <- pp$run()$collect_out()

        expect_equivalent(out, c(out1, out2))
    })

    test_that("the separator used for step names can be customized",
    {
        pip1 <- Pipeline$new("pipe1", data = 1) |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(b = 2) b)

        pip2 <- Pipeline$new("pipe2") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(b = 2) b)

        pp <- pip1$append(pip2, sep = "_")

        expect_equal(
            pp$get_step_names(),
            c("data", "f1", "f2", "data_pipe2", "f1_pipe2", "f2_pipe2")
        )
    })


    test_that(
        "output of first pipeline can be set as input of appended pipeline",
    {
        pip1 <- Pipeline$new("pipe1", data = 1)
        pip1$add("f1", function(a = ~data) a * 2)

        pip2 <- Pipeline$new("pipe2", data = 99)
        pip2$add("f1", function(a = ~data) a * 3)
        pip2$add("f2", function(a = ~f1) a * 4)

        pp <- pip1$append(pip2, outAsIn = TRUE)

        depends <- pp$get_depends()
        expect_equal(depends[["data.pipe2"]], c(data = "f1"))

        out <- pp$run()$collect_out(all = TRUE)
        pipe1_out <- out[["f1"]][["f1"]]
        expect_equal(pipe1_out, 1 * 2)
        expect_equal(out[["data.pipe2"]], pipe1_out)
        expect_equal(out[["f1"]][["f1.pipe2"]], pipe1_out * 3)
        expect_equal(out[["f2"]], out[["f1"]][["f1.pipe2"]] * 4)
    })

    test_that("if duplicated step names would be created, an error is given",
    {
        pip1 <- Pipeline$new("pipe1")
        pip1$add("f1", function(a = ~data) a + 1)
        pip1$add("f1.pipe2", function(a = ~data) a + 1)

        pip2 <- Pipeline$new("pipe2")
        pip2$add("f1", function(a = ~data) a + 1)

        expect_error(
            pip1$append(pip2),
            "Cannot auto-fix name clash for step 'f1' in pipeline 'pipe2'",
            fixed = TRUE
        )
    })
})



test_that("append_to_step_names",
{
    expect_true(is.function(Pipeline$new("pipe")$append_to_step_names))

    test_that("postfix can be appended to step names",
    {
        pip <- Pipeline$new("pipe", data = 1)

        pip$add("f1", function(a = ~data) a + 1)
        pip$add("f2", function(a = ~data, b = ~f1) a + b)
        pip$append_to_step_names("foo")

        expected_names <- c("data.foo", "f1.foo", "f2.foo")
        expect_equal(pip$get_step_names(), expected_names)

        expected_depends <- list(
            data.foo = character(0),
            f1.foo = c(a = "data.foo"),
            f2.foo = c(a = "data.foo", b = "f1.foo")
        )

        depends <- pip$get_depends()
        expect_equal(pip$get_depends(), expected_depends)
    })
})



test_that("collect_out",
{
    expect_true(is.function(Pipeline$new("pipe")$collect_out))

    test_that("data is set as first step but not part of output by default",
    {
        dat <- data.frame(a = 1:2, b = 1:2)
        pip <- Pipeline$new("pipe1", data = dat)
        expect_equal(pip$pipeline[["step"]], "data")

        out <- pip$run()$collect_out()
        expect_equal(out, list())

        pip <- Pipeline$new("pipe1", data = dat) |>
            pipe_add("f1", function(x = ~data) x, keepOut = TRUE)

        out <- pip$run()$collect_out()
        expect_equal(out[["f1"]], dat)
    })

    test_that("at the end, pipeline can clean output that shall not be kept",
    {
        data <- 9
        pip <- Pipeline$new("pipe1", data = data)

        foo <- function(a = 1) a
        bar <- function(a, b) a + b

        a <- 5
        pip$add("f1", foo, params = list(a = a))
        pip$add("f2", bar, params = list(a = ~data, b = ~f1), keepOut = TRUE)

        pip$run(cleanUnkept = TRUE)
        expect_equal(pip$get_out("f1"), NULL)
        expect_equal(pip$get_out("f2"), a + data)
    })

    test_that("output is collected as expected",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a, keepOut = TRUE) |>
            pipe_add("f2", function(a = 2, b = ~f1) a + b) |>
            pipe_add("f3", function(a = 3, b = ~f2) a + b, keepOut = TRUE)

        out <- pip$run()$collect_out()
        expect_equal(length(out), 2)
        expect_equal(names(out), c("f1", "f3"))
    })

    test_that("output can be grouped",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(a = 1, b = 2) a + b, group = "plus") |>
            pipe_add("f3", function(a = 1, b = 2) a / b) |>
            pipe_add("f4", function(a = 2, b = 2) a + b, group = "plus")

        out <- pip$run()$collect_out(all = TRUE)

        expect_equal(out[["plus"]], list(f2 = 3, f4 = 4))
        expect_equal(out[["f1"]], 1)
        expect_equal(out[["f3"]], 1/2)
    })

    test_that("output is ordered in the order of steps",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("f2", function(a = 1) a, keepOut = TRUE) |>
            pipe_add("f1", function(b = 2) b, keepOut = TRUE)

        out <- pip$run()$collect_out()
        expect_equal(names(out), c("f2", "f1"))
    })

    test_that(
        "grouped output is ordered in the order of group definitions",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("f1", function(x = 1) x, group = "g2") |>
            pipe_add("f2", function(x = 2) x, group = "g1") |>
            pipe_add("f3", function(x = 3) x, group = "g2") |>
            pipe_add("f4", function(x = 4) x, group = "g1")

        out <- pip$run()$collect_out(all = TRUE)

        expect_equal(names(out), c("data", "g2", "g1"))
    })

    test_that(
        "if just one group the output name still will take the group name",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(a = 1, b = 2) a + b, group = "plus") |>
            pipe_add("f3", function(a = 1, b = 2) a / b, group = "my f3") |>
            pipe_add("f4", function(a = 2, b = 2) a + b, group = "plus")

        out <- pip$run()$collect_out(all = TRUE)

        expect_equal(names(out), c("data", "f1", "plus", "my f3"))
    })
})



test_that("discard_steps",
{
    expect_true(is.function(Pipeline$new("pipe")$discard_steps))

    test_that("pipeline steps can be discarded by pattern",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("calc", function(a = 1) a) |>
            pipe_add("plot1", function(x = ~calc) x) |>
            pipe_add("plot2", function(x = ~plot1) x)

        out <- capture.output(
            pip$discard_steps("plot"),
            type = "message"
        )
        expect_equal(
            out,
            c(
                "step 'plot2' was removed",
                "step 'plot1' was removed"
            )
        )

        expect_equal(pip$pipeline[["step"]], c("data", "calc"))
    })

    test_that("if no pipeline step matches pattern, pipeline remains unchanged",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("calc", function(a = 1) a) |>
            pipe_add("plot1", function(x = ~calc) x) |>
            pipe_add("plot2", function(x = ~plot1) x)

        steps_before = pip$pipeline[["step"]]

        expect_silent(pip$discard_steps("bla"))
        expect_equal(pip$pipeline[["step"]], steps_before)
    })


    test_that("if step has downstream dependencies, an error is given",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(b = ~f1) b)

        expect_error(
            pip$discard_steps("f1"),
            paste(
                "cannot remove step 'f1' because the following",
                "steps depend on it: 'f2'"
            )
        )

        pip$add("f3", function(x = ~f1) x)
        expect_error(
            pip$discard_steps("f1"),
            paste(
                "cannot remove step 'f1' because the following",
                "steps depend on it: 'f2', 'f3'"
            )
        )
    })
})


test_that("get_data",
{
    expect_true(is.function(Pipeline$new("pipe")$get_data))

    test_that(
        "data can be retrieved",
    {
        p <- Pipeline$new("pipe", data = 1:2)
        expect_equal(p$get_data(), 1:2)

        p$set_data(3:4)
        expect_equal(p$get_data(), 3:4)
    })

    test_that(
        "signals missing data",
    {
        p <- Pipeline$new("pipe", data = 1:2)
        p$pop_step()    # remove data step

        expect_error(
            p$get_data(),
            "no data step defined"
        )
    })
})


test_that("get_depends",
{
    expect_true(is.function(Pipeline$new("pipe")$get_depends))

    test_that(
        "dependencies can be retrieved and are named after the steps",
    {
        pip <- Pipeline$new("pipe", data = 1)

        pip$add("f1", function(a = ~data) a + 1)
        pip$add("f2", function(b = ~f1) b + 1)

        depends <- pip$get_depends()
        expected_depends <- list(
            data = character(0),
            f1 = c(a = "data"),
            f2 = c(b = "f1")
        )

        expect_equal(depends, expected_depends)
        expect_equal(names(depends), pip$get_step_names())
    })
})



test_that("get_depends_down",
{
    expect_true(is.function(Pipeline$new("pipe")$get_depends_down))

    test_that("dependencies can be determined recursively for given step",
    {
        pip <- Pipeline$new("pipe")

        pip$add("f1", function(a = 1) a)
        pip$add("f2", function(a = ~f1) a)
        pip$add("f3", function(a = ~f1, b = ~f2) a + b)
        pip$add("f4", function(a = ~f1, b = ~f2, c = ~f3) a + b + c)

        expect_equal(pip$get_depends_down("f3"), c("f4"))
        expect_equal(pip$get_depends_down("f2"), c("f3", "f4"))
        expect_equal(pip$get_depends_down("f1"), c("f2", "f3", "f4"))
    })

    test_that("if no dependencies an empty character vector is returned",
    {
        pip <- Pipeline$new("pipe")

        pip$add("f1", function(a = 1) a)
        expect_equal(pip$get_depends_down("f1"), character(0))
    })

    test_that("step must exist",
    {
        pip <- Pipeline$new("pipe")

        expect_error(
            pip$get_depends_down("f1"),
            "step 'f1' does not exist"
        )
    })

    test_that(
        "works with complex dependencies as created by data splits",
    {
        dat1 <- data.frame(x = 1:2)
        dat2 <- data.frame(y = 1:2)
        dataList <- list(dat1, dat2)

        pip <- Pipeline$new("pipe") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(a = ~f1, b = 2) b) |>
            pipe_add("f3", function(x = ~f1, y = ~f2) x + y)

        pip$set_data_split(dataList, toStep = "f2")

        expect_equal(
            pip$get_depends_down("f1.1"),
            c("f2.1", "f3")
        )

        expect_equal(
            pip$get_depends_down("f1.2"),
            c("f2.2", "f3")
        )

        expect_equal(pip$get_depends_down("f2.1"), "f3")
        expect_equal(pip$get_depends_down("f2.2"), "f3")
    })
})



test_that("get_depends_up",
{
    expect_true(is.function(Pipeline$new("pipe")$get_depends_up))

    test_that("dependencies can be determined recursively for given step",
    {
        pip <- Pipeline$new("pipe")

        pip$add("f1", function(a = 1) a)
        pip$add("f2", function(a = ~f1) a)
        pip$add("f3", function(a = ~f1, b = ~f2) a + b)
        pip$add("f4", function(a = ~f1, b = ~f2, c = ~f3) a + b + c)

        expect_equal(pip$get_depends_up("f2"), c("f1"))
        expect_equal(pip$get_depends_up("f3"), c("f1", "f2"))
        expect_equal(pip$get_depends_up("f4"), c("f1", "f2", "f3"))
    })

    test_that("if no dependencies an empty character vector is returned",
    {
        pip <- Pipeline$new("pipe")

        pip$add("f1", function(a = 1) a)
        expect_equal(pip$get_depends_up("f1"), character(0))
    })

    test_that("step must exist",
    {
        pip <- Pipeline$new("pipe")

        expect_error(
            pip$get_depends_up("f1"),
            "step 'f1' does not exist"
        )
    })

    test_that("works with complex dependencies as created by data splits",
    {
        dat1 <- data.frame(x = 1:2)
        dat2 <- data.frame(y = 1:2)
        dataList <- list(dat1, dat2)

        pip <- Pipeline$new("pipe") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(a = ~f1, b = 2) b) |>
            pipe_add("f3", function(x = ~f1, y = ~f2) x + y)

        pip$set_data_split(dataList, toStep = "f2")

        expect_equal(pip$get_depends_up("f2.1"), c("f1.1"))
        expect_equal(pip$get_depends_up("f2.2"), c("f1.2"))

        expect_equivalent(
            pip$get_depends_up("f3"),
            c("f1.1", "f2.1", "f1.2", "f2.2")
        )
    })
})


test_that("get_graph",
{
    expect_true(is.function(Pipeline$new("pipe")$get_graph))

    pip <- Pipeline$new("pipe") |>
        pipe_add("f1", function(a = 1) a) |>
        pipe_add("f2", function(a = ~f1, b = ~data) a)

    res <- pip$get_graph()

    test_that("returns a node table with the expected columns",
    {
        tab <- res$nodes
        expect_true(is.data.frame(tab))
        expectedColumns <- c("id", "label", "group", "shape", "color", "title")

        expect_equal(colnames(tab), expectedColumns)
    })

    test_that("the node table contains all steps",
    {
        tab <- res$nodes
        expect_equal(tab$label, pip$get_step_names())
    })

    test_that("returns an edges table with the expected columns",
    {
        tab <- res$edges
        expect_true(is.data.frame(tab))
        expectedColumns <- c("from", "to", "arrows")

        expect_equal(colnames(tab), expectedColumns)
    })

    test_that("can be printed created for certain groups",
    {
        pip <- Pipeline$new("pipe")
        pip$add("step2", \(a = ~data) a + 1, group = "add")
        pip$add("step3", \(a = ~step2) 2 * a, group = "mult")
        pip$add("step4", \(a = ~step2, b = ~step3) a + b, group = "add")
        pip$add("step5", \(a = ~data, b = ~step4) a * b, group = "mult")

        res.add <- pip$get_graph(groups = "add")
        expect_equal(res.add$nodes$label, c("step2", "step4"))

        res.mult <- pip$get_graph(groups = "mult")
        expect_equal(res.mult$nodes$label, c("step3", "step5"))
    })
})


test_that("get_out",
{
    expect_true(is.function(Pipeline$new("pipe")$get_out))

    test_that("output at given step can be retrieved",
    {
        data <- airquality
        pip <- Pipeline$new("pipe", data = data) |>
            pipe_add("model",
                function(data = ~data) {
                    lm(Ozone ~ Wind, data = data)
                },
            )

        pip$run()

        expect_equal(pip$get_out("data"), data)
        expect_equivalent(
            pip$get_out("model"),
            lm(Ozone ~ Wind, data = data)
        )
    })

    test_that("step of requested output must exist",
    {
        pip <- Pipeline$new("pipe")
        pip$run()
        expect_error(pip$get_out("foo"), "step 'foo' does not exist")
    })
})



test_that("get_params",
{
    expect_true(is.function(Pipeline$new("pipe")$get_params))

    test_that("parameters can be retrieved",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a, keepOut = TRUE) |>
            pipe_add("f2", function(a, b = ~f1) a + b,
                params = list(a = 8),
                keepOut = TRUE
            ) |>
            pipe_add("f3", function(a = ~f2, b = 3) a + b, keepOut = TRUE)

        p <- pip$get_params()
        expect_equal(
            p, list(f1 = list(a = 1), f2 = list(a = 8), f3 = list(b = 3))
        )
    })


    test_that("empty pipeline gives empty list of parameters",
    {
        pip <- Pipeline$new("pipe1")
        expect_equivalent(pip$get_params(), list())

        pipe_add(pip, "f1", function() 1)
        expect_equivalent(pip$get_params(), list())
    })

    test_that("hidden parameters are filtered out by default",
    {

        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1, .hidden = 2) a)

        p <- pip$get_params()
        expect_equal(p, list(f1 = list(a = 1)))

        p <- pip$get_params(ignoreHidden = FALSE)
        expect_equal(p, list(f1 = list(a = 1, .hidden = 2)))
    })

    test_that("works with Param objects",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add(
                "f1",
                function(
                    x = new("NumericParam", "x", value = 1),
                    y = new("NumericParam", "y", value = 2)
                ) {
                    x + y
                }
            ) |>
            pipe_add(
                "f2",
                function(
                    s1 = new("StringParam", "s1", "Hello"),
                    s2 = new("StringParam", "s2", "World")
                ) {
                    paste(s1, s2)
                }
            )

        par <- pip$get_params()
        expect_true(all(par$f1 |> sapply(is, "NumericParam")))
        expect_equal(par$f1$x@value, 1)
        expect_equal(par$f1$y@value, 2)

        expect_true(all(par$f2 |> sapply(is, "StringParam")))
        expect_equal(par$f2$s1@value, "Hello")
        expect_equal(par$f2$s2@value, "World")
    })
})



test_that("get_params_at_step",
{
    expect_true(is.function(Pipeline$new("pipe")$get_params_at_step))

    test_that("list of step parameters can be retrieved",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1, b = 2) a + b) |>
            pipe_add("f2", function(x = 1, y = 2) x + y)

        expect_equal(
            pip$get_params_at_step("f1"), list(a = 1, b = 2)
        )

        expect_equal(
            pip$get_params_at_step("f2"), list(x = 1, y = 2)
        )
    })

    test_that("if no parameters empty list is returned",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function() 1)

        expect_equal(
            pip$get_params_at_step("f1"), list()
        )
    })

    test_that(
        "hidden parameters are not returned, unless explicitly requested",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1, .b = 2) a + .b)

        expect_equal(
            pip$get_params_at_step("f1"), list(a = 1)
        )

        expect_equal(
            pip$get_params_at_step("f1", ignoreHidden = FALSE),
            list(a = 1, .b = 2)
        )
    })

    test_that("bound parameters are never returned",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1, b = ~data) a + b)

        expect_equal(
            pip$get_params_at_step("f1"), list(a = 1)
        )

        expect_equal(
            pip$get_params_at_step("f1", ignoreHidden = FALSE),
            list(a = 1)
        )
    })

    test_that("step must exist",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1, b = ~data) a + b)

        expect_error(
            pip$get_params_at_step("foo"),
            "step 'foo' does not exist"
        )
    })

    test_that("works with Param objects",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add(
                "f1",
                function(
                    x = new("NumericParam", "x", value = 1),
                    y = new("NumericParam", "y", value = 2)
                ) {
                    x + y
                }
            ) |>
            pipe_add(
                "f2",
                function(
                    s1 = new("StringParam", "s1", "Hello"),
                    s2 = new("StringParam", "s2", "World")
                ) {
                    paste(s1, s2)
                }
            )

        par1 <- pip$get_params_at_step("f1")
        expect_true(all(par1 |> sapply(is, "NumericParam")))
        expect_equal(par1$x@value, 1)
        expect_equal(par1$y@value, 2)

        par2 <- pip$get_params_at_step("f2")
        expect_true(all(par2 |> sapply(is, "StringParam")))
        expect_equal(par2$s1@value, "Hello")
        expect_equal(par2$s2@value, "World")
    })
})


test_that("get_params_unique",
{
    expect_true(is.function(Pipeline$new("pipe")$get_params_unique))

    test_that("parameters can be retrieved uniquely and if occuring multiple
        times, the 1st default value is used",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(a = 2, b = 3) a + b) |>
            pipe_add("f3", function(a = 4, b = 5, c = 6) a + b)

        p <- pip$get_params_unique()
        expect_equivalent(p, list(a = 1, b = 3, c = 6))
    })

    test_that("empty pipeline gives empty list",
    {
        pip <- Pipeline$new("pipe")
        expect_equivalent(pip$get_params_unique(), list())
    })

    test_that("pipeline with no parameters gives empty list",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("f1", function() 1)
        expect_equivalent(pip$get_params_unique(), list())
    })

    test_that("works with Param objects",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add(
                "f1",
                function(
                    a = 0,
                    x = new("NumericParam", "x", value = 1),
                    y = new("NumericParam", "y", value = 2)
                ) {
                    a * (x + y)
                }
            ) |>
            pipe_add(
                "f2",
                function(
                    a = new("NumericParam", "a", value = 0),
                    x = new("NumericParam", "x", value = 1),
                    y = 2,
                    z = new("NumericParam", "y", value = 3)
                ) {
                    a * (x + y + z)
                }
            )

        par <- pip$get_params_unique()
        expect_equal(names(par), c("a", "x", "y", "z"))
        expect_equal(par$a, 0)
        expect_equal(par$x@value, 1)
        expect_equal(par$y@value, 2)
        expect_equal(par$z@value, 3)
    })
})



test_that("get_params_unique_json",
{
    expect_true(is.function(Pipeline$new("pipe")$get_params_unique_json))

    test_that("the elements are not named",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a, keepOut = TRUE)

        p <- pip$get_params_unique_json()
        pl <- jsonlite::fromJSON(p, simplifyVector = FALSE)
        expect_equal(names(pl), NULL)


        pip <- Pipeline$new("pipe1") |>
            pipe_add(
                "f1", function(x = new("StringParam", "my x", "some x")) x
            ) |>
            pipe_add(
                "f2", function(y = new("StringParam", "my y", "some y")) y
            )

        p <- pip$get_params_unique_json()
        pl <- jsonlite::fromJSON(p, simplifyVector = FALSE)
        expect_equal(names(pl), NULL)
    })


    test_that("standard parameters are returned as name-value pairs",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(a = 1, b = 2) a) |>
            pipe_add("f3", function(a = 1, b = 2, c = list(a = 1, b = 2)) a)

        p <- pip$get_params_unique_json()
        expect_true(methods::is(p, "json"))

        pl <- jsonlite::fromJSON(p, simplifyVector = FALSE)
        expect_equal(
            pl,
            list(
                list(name = "a", value = 1),
                list(name = "b", value = 2),
                list(name = "c", value = list(a = 1, b = 2))
            )
        )
    })

    test_that("Param objects are returned with full information",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add(
                "f1", function(x = new("StringParam", "my x", "some x")) x
            ) |>
            pipe_add(
                "f2", function(y = new("StringParam", "my y", "some y")) y
            )

        p <- pip$get_params_unique_json()
        pl <- jsonlite::fromJSON(p, simplifyVector = FALSE)

        expect_equal(
            pl,
            list(
                list(
                    value = "some x",
                    name = "x",
                    advanced = FALSE,
                    label = "my x",
                    description = "",
                    source = "internal",
                    class = "StringParam"
                ),
                list(
                    value = "some y",
                    name = "y",
                    advanced = FALSE,
                    label = "my y",
                    description = "",
                    source = "internal",
                    class = "StringParam"
                )
            )
        )
    })

    test_that("the name of the arg is set to the name of the Param object",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add(
                "f1", function(x = new("StringParam", "my x", "some x")) x
            ) |>
            pipe_add(
                "f2", function(y = new("StringParam", "my y", "some y")) y
            )

        p <- pip$get_params_unique_json()
        pl <- jsonlite::fromJSON(p)

        expect_true(pl[["label"]][[1]] == "my x")
        expect_false(pl[["name"]][[1]] == "my x")
        hasArgName <- pl[["name"]][[1]] == "x"

        expect_true(pl[["label"]][[2]] == "my y")
        expect_false(pl[["name"]][[2]] == "my y")
        hasArgName <- pl[["name"]][[2]] == "y"
    })

    test_that("works with mixed, that is, standard and Param objects",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(x = 1) x) |>
            pipe_add("f2", function(s = new("StringParam", "my s", "some s")) s)

        p <- pip$get_params_unique_json()
        pl <- jsonlite::fromJSON(p, simplifyVector = FALSE)

        expect_equal(pl[[1]], list(name = "x", value = 1L))
        expect_equal(
            pl[[2]],
            list(
                value = "some s",
                name = "s",
                advanced = FALSE,
                label = "my s",
                description = "",
                source = "internal",
                class = "StringParam"
            )
        )
    })
})



test_that("get_step",
{
    expect_true(is.function(Pipeline$new("pipe")$get_step))

    test_that("single steps can be retrieved",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", identity, params = list(x = 1))

        expect_equal(pip$get_step("data"), pip$pipeline[1, ])
        expect_equal(pip$get_step("f1"), pip$pipeline[2, ])

        expect_error(pip$get_step("foo"), "step 'foo' does not exist")
    })

    test_that("dependencies are recorded as expected",
    {
        pip <- Pipeline$new("pipe1", data = 9)

        foo <- function(a = 0) a
        bar <- function(a = 1, b = 2) a + b

        pip$add("f1", foo)
        pip$add("f2", bar, params = list(a = ~data, b = ~f1))

        expect_true(length(unlist(pip$get_step("f1")[["depends"]])) == 0)
        expect_equal(
            unlist(pip$get_step("f2")[["depends"]]),
            c("a" = "data", b = "f1")
        )
    })
})


test_that("get_step_names",
{
    expect_true(is.function(Pipeline$new("pipe")$get_step_names))

    test_that("step names be retrieved",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function() {}) |>
            pipe_add("f2", function(x = 1) x)

        expect_equal(pip$get_step_names(), pip$pipeline[["step"]])
    })
})



test_that("get_step_number",
{
    expect_true(is.function(Pipeline$new("pipe")$get_step_number))

    test_that("get_step_number",
    {
        expect_true(is.function(Pipeline$new("pipe")$get_step_number))

        test_that("get_step_number works as expected",
        {
            pip <- expect_no_error(Pipeline$new("pipe"))
            pip$add("f1", function(a = 1) a)
            pip$add("f2", function(a = 1) a)

            pip$get_step_number("f1") |> expect_equal(2)
            pip$get_step_number("f2") |> expect_equal(3)
        })

        test_that("signals non-existent step",
        {
            pip <- expect_no_error(Pipeline$new("pipe"))
            pip$add("f1", function(a = 1) a)

            expect_error(
                pip$get_step_number("non-existent"),
                "step 'non-existent' does not exist"
            )
        })
    })
})


test_that("has_step",
{
    expect_true(is.function(Pipeline$new("pipe")$has_step))

    test_that("it can be checked if pipeline has a step",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a)

        expect_true(pip$has_step("f1"))
        expect_false(pip$has_step("f2"))
    })
})


test_that("insert_after",
{
    expect_true(is.function(Pipeline$new("pipe")$insert_after))

    test_that("can insert a step after another step",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a + 1) |>
            pipe_add("f2", function(a = ~f1) a + 1)

        pip$insert_after(
            "f1",
            step = "f3",
            fun = function(a = ~f1) a + 1
        )

        expect_equal(
            pip$get_step_names(),
            c("data", "f1", "f3", "f2")
        )
    })

    test_that("will not insert a step if the step already exists",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a + 1) |>
            pipe_add("f2", function(a = ~f1) a + 1)

        expect_error(
            pip$insert_after("f1", step = "f2"),
            "step 'f2' already exists"
        )
    })

    test_that(
        "will not insert a step if the reference step does not exist",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a + 1) |>
            pipe_add("f2", function(a = ~f1) a + 1)

        expect_error(
            pip$insert_after("non-existent", step = "f3"),
            "step 'non-existent' does not exist"
        )
    })

    test_that(
        "will not insert a step with bad parameter dependencies",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a + 1) |>
            pipe_add("f2", function(a = ~f1) a + 1)

        expect_error(
            pip$insert_after("f1", step = "f3", \(x = ~f2) x),
            "step 'f3': dependency 'f2' not found"
        )
    })

    test_that("will work if insert happens at last position",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a + 1) |>
            pipe_add("f2", function(a = ~f1) a + 1)

        pip$insert_after(
            "f2",
            step = "f3",
            fun = function(a = ~f1) a + 1
        )

        expect_equal(
            pip$get_step_names(),
            c("data", "f1", "f2", "f3")
        )
    })
})


test_that("insert_before",
{
    expect_true(is.function(Pipeline$new("pipe")$insert_before))

    test_that("can insert a step after another step",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a + 1) |>
            pipe_add("f2", function(a = ~f1) a + 1)

        pip$insert_before(
            "f2",
            step = "f3",
            fun = function(a = ~f1) a + 1
        )

        expect_equal(
            pip$get_step_names(),
            c("data", "f1", "f3", "f2")
        )
    })

    test_that("will not insert a step if the step already exists",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a + 1) |>
            pipe_add("f2", function(a = ~f1) a + 1)

        expect_error(
            pip$insert_before("f1", step = "f2"),
            "step 'f2' already exists"
        )
    })

    test_that(
        "will not allow step to be inserted at first position",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a + 1)

        expect_error(
            pip$insert_before("data", step = "f2"),
            "cannot insert before first step"
        )
    })

    test_that("will not insert a step if the reference step does not exist",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a + 1) |>
            pipe_add("f2", function(a = ~f1) a + 1)

        expect_error(
            pip$insert_before("non-existent", step = "f3"),
            "step 'non-existent' does not exist"
        )
    })

    test_that(
        "will not insert a step with bad parameter dependencies",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a + 1) |>
            pipe_add("f2", function(a = ~f1) a + 1)

        expect_error(
            pip$insert_before("f2", step = "f3", \(x = ~f2) x),
            "step 'f3': dependency 'f2' not found"
        )
    })
})


test_that("length",
{
    expect_true(is.function(Pipeline$new("pipe")$length))

    test_that("returns the number of steps",
    {
        pip <- Pipeline$new("pipe")
        expect_equal(pip$length(), 1)

        pip$add("f1", function(a = 1) a)
        expect_equal(pip$length(), 2)

        pip$remove_step("f1")
        expect_equal(pip$length(), 1)
    })
})


test_that("lock_step",
{
    expect_true(is.function(Pipeline$new("pipe")$lock_step))

    test_that("sets state to 'locked'",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("f1", function(a = 1) a)

        pip$lock_step("f1")
        expect_equal(pip$get_step("f1")[["state"]], "Locked")

        pip
    })
})


test_that("print",
{
    expect_true(is.function(Pipeline$new("pipe")$print))

    test_that("pipeline can be printed",
    {
        pip <- Pipeline$new("pipe1", data = 9)

        expect_output(pip$print())
    })

    test_that("missing function is signaled",
    {
        pip <- Pipeline$new("pipe1")

        expect_error(
            pip$add("f1", "non-existing-function"),
            "object 'non-existing-function' of mode 'function' was not found"
        )
    })

    test_that("if verbose is TRUE, all columns are printed",
    {
        op <- options(width = 1000L)
        on.exit(options(op))
        pip <- Pipeline$new("pipe1", data = 9)

        out <- capture.output(pip$print(verbose = TRUE))
        header <- out[1] |> trimws() |> strsplit("\\s+") |> unlist()
        expected_header <- colnames(pip$pipeline)
        expect_equal(header, expected_header)
    })
})


test_that("pop_step",
{
    expect_true(is.function(Pipeline$new("pipe")$pop_step))

    test_that("last pipeline step can be popped",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a)

        pip_copy = pip$clone()

        pip$add("f2", function(b = 2) b)

        expect_equal(pip$length(), 3)
        expect_equal(pip_copy$length(), 2)

        res = pip$pop_step()
        expect_equal(res, "f2")

        expect_equal(pip$length(), 2)
        expect_equal(pip, pip_copy)
    })
})




test_that("pop_steps_after",
{
    expect_true(is.function(Pipeline$new("pipe")$pop_steps_after))

    test_that("all steps after a given step can be removed",
    {

        pip <- Pipeline$new("pipe1", data = 0) |>
            pipe_add("f1", function(x = 1) x) |>
            pipe_add("f2", function(x = ~f1) x) |>
            pipe_add("f3", function(x = ~f2) x)

        steps = pip$pop_steps_after("f1")
        expect_equal(steps, c("f2", "f3"))

        hasAllStepsRemoved = !any(steps %in% pip$pipeline[["name"]])
        expect_true(hasAllStepsRemoved)
    })

    test_that("if given step does not exist, an error is signalled",
    {

        pip <- Pipeline$new("pipe1", data = 0) |>
            pipe_add("f1", function(x = 1) x)

        expect_error(
            pip$pop_steps_after("bad_step"),
            "step 'bad_step' does not exist"
        )
    })

    test_that("if given step is the last step, nothing gets removed",
    {

        pip <- Pipeline$new("pipe1", data = 0) |>
            pipe_add("f1", function(x = 1) x) |>
            pipe_add("f2", function(x = ~f1) x)

        length_before = pip$length()

        res = pip$pop_steps_after("f2")

        expect_equal(res, character(0))
        hasNothingRemoved = pip$length() == length_before
        expect_true(hasNothingRemoved)
    })
})



test_that("pop_steps_from",
{
    expect_true(is.function(Pipeline$new("pipe")$pop_steps_from))

    test_that("all steps from a given step can be removed",
    {

        pip <- Pipeline$new("pipe1", data = 0) |>
            pipe_add("f1", function(x = 1) x) |>
            pipe_add("f2", function(x = ~f1) x) |>
            pipe_add("f3", function(x = ~f2) x)

        steps = pip$pop_steps_from("f2")
        expect_equal(steps, c("f2", "f3"))

        hasAllStepsRemoved = !any(steps %in% pip$pipeline[["name"]])
        expect_true(hasAllStepsRemoved)
    })

    test_that("if given step does not exist, an error is signalled",
    {

        pip <- Pipeline$new("pipe1", data = 0) |>
            pipe_add("f1", function(x = 1) x)

        expect_error(
            pip$pop_steps_from("bad_step"),
            "step 'bad_step' does not exist"
        )
    })

    test_that("if given step is the last step, one step removed",
    {

        pip <- Pipeline$new("pipe1", data = 0) |>
            pipe_add("f1", function(x = 1) x) |>
            pipe_add("f2", function(x = ~f1) x)

        length_before = pip$length()

        res = pip$pop_steps_from("f2")

        expect_equal(res, "f2")
        hasOneStepRemoved = pip$length() == length_before - 1
        expect_true(hasOneStepRemoved)
    })
})



test_that("remove_step",
{
    expect_true(is.function(Pipeline$new("pipe")$remove_step))

    test_that("pipeline step can be removed",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(b = 1) b)

        pip$remove_step("f1")

        expect_equal(pip$get_step_names(), c("data", "f2"))
    })

    test_that("step must exist",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a)

        expect_error(
            pip$remove_step("non-existent-step"),
            "step 'non-existent-step' does not exist"
        )
    })

    test_that("if step has downstream dependencies, an error is given",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(b = ~f1) b)

        expect_error(
            pip$remove_step("f1"),
            paste(
                "cannot remove step 'f1' because the following",
                "steps depend on it: 'f2'"
            )
        )

        pip$add("f3", function(x = ~f1) x)
        expect_error(
            pip$remove_step("f1"),
            paste(
                "cannot remove step 'f1' because the following",
                "steps depend on it: 'f2', 'f3'"
            )
        )
    })

    test_that(
        "if error, only the direct downstream dependencies are reported",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(b = ~f1) b) |>
            pipe_add("f3", function(c = ~f2) c) |>
            pipe_add("f4", function(d = ~f1) d)

        expect_error(
            pip$remove_step("f1"),
            paste(
                "cannot remove step 'f1' because the following",
                "steps depend on it: 'f2', 'f4'"
            )
        )
    })

    test_that(
        "step can be removed together with is downstream dependencies",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(b = ~f1) b) |>
            pipe_add("f3", function(c = ~f2) c) |>
            pipe_add("f4", function(d = ~f1) d) |>
            pipe_add("f5", function(x = ~data) x)

        out <- utils::capture.output(
            pip$remove_step("f1", recursive = TRUE),
            type = "message"
        )

        expect_equal(pip$get_step_names(), c("data", "f5"))
        expect_equal(
            out,
            paste(
                "Removing step 'f1' and its downstream dependencies:",
                "'f2', 'f3', 'f4'"
            )
        )
    })
})


test_that("remove_step",
{
    f <- Pipeline$new("pipe")$remove_step
    expect_true(is.function(f))

    test_that("pipeline step can be renamed",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(b = 2) b)

        pip$rename_step(from = "f1", to = "first")

        pip$get_step_names() |> expect_equal(c("data", "first", "f2"))
    })

    test_that("signals name clash",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(b = 2) b)

        expect_error(
            pip$rename_step(from = "f1", to = "f2"),
            "step 'f2' already exists"
        )
    })

    test_that("renames dependencies as well",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(b = ~f1) b) |>
            pipe_add("f3", function(a = ~f1, b = ~f2) a + b)

        pip$rename_step(from = "f1", to = "first")

        expect_equal(
            pip$get_depends(),
            list(
                data = character(0),
                first = character(0),
                f2 = c(b = "first"),
                f3 = c(a = "first", b = "f2")
            )
        )
    })
})


test_that("replace_step",
{
    expect_true(is.function(Pipeline$new("pipe")$replace_step))

    test_that("pipeline steps can be replaced",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(b = 2) b) |>
            pipe_add("f3", function(c = ~f2) c, keepOut = TRUE)

        out = unname(unlist(pip$run()$collect_out()))
        expect_equal(out, 2)

        pip$replace_step("f2", function(z = 4) z)
        out = unname(unlist(pip$run()$collect_out()))
        expect_equal(out, 4)
    })

    test_that("fun must be passed as a function or string",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("step1", function(a = 1) a)

        expect_error(
            pip$replace_step("step1", fun = 1),
            "is.function(fun) || is_string(fun) is not TRUE",
            fixed = TRUE
        )
    })

    test_that("params must be a list",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("step1", function(a = 1) a)

        expect_error(
            pip$replace_step("step1", fun = function() 1, params = 1),
            "is.list(params)",
            fixed = TRUE
        )
    })

    test_that("description must be (possibly empty) string",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("step1", function(a = 1) a)

        expect_error(
            pip$replace_step("step1", fun = function() 1, description = 1),
            "is_string(description)",
            fixed = TRUE
        )
        expect_no_error(
            pip$replace_step("step1", fun = function() 1, description = "")
        )
    })

    test_that("group must be non-empty string",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("step1", function(a = 1) a)

        expect_error(
            pip$replace_step("step1", fun = function() 1, group = 1),
            "is_string(group) && nzchar(group) is not TRUE",
            fixed = TRUE
        )
        expect_error(
            pip$replace_step("step1", fun = function() 1, group = ""),
            "is_string(group) && nzchar(group) is not TRUE",
            fixed = TRUE
        )
    })

    test_that("keepOut must be logical",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("step1", function(a = 1) a)

        expect_error(
            pip$replace_step("step1", fun = function() 1, keepOut = 1),
            "is.logical(keepOut) is not TRUE",
            fixed = TRUE
        )
    })

    test_that("the replacing function can be passed as a string",
    {

        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(x = 3) x, keepOut = TRUE)

        out = unname(unlist(pip$run()$collect_out()))
        expect_equal(out, 3)

        .my_func <- function(x = 3) {
            2 * x
        }
        assign(".my_func", .my_func, envir = globalenv())
        on.exit(rm(".my_func", envir = globalenv()))

        pip$replace_step("f1", fun = ".my_func", keepOut = TRUE)

        out = unname(unlist(pip$run()$collect_out()))
        expect_equal(out, 6)
    })

    test_that(
        "when replacing function, default parameters can be overridden",
    {

        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(x = 1:3) x, keepOut = TRUE)

        .my_func <- function(x = 3) {
            2 * x
        }
        assign(".my_func", .my_func, envir = globalenv())
        on.exit(rm(".my_func", envir = globalenv()))

        pip$replace_step(
            "f1",
            fun = ".my_func",
            params = list(x = 10),
            keepOut = TRUE
        )

        out = unname(unlist(pip$run()$collect_out()))
        expect_equal(out, 20)
    })

    test_that("the pipeline step that is being replaced must exist",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(b = 2) b) |>
            pipe_add("f3", function(c = ~f2) c, keepOut = TRUE)

        expect_error(pip$replace_step("non-existent", function(z = 4) z))
    })


    test_that(
        "if replacing a pipeline step, dependencies are verifed correctly",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(b = 2) b) |>
            pipe_add("f3", function(c = ~f2) c, keepOut = TRUE)

        expect_error(
            pip$replace_step("f2", function(z = ~foo) z),
            "dependency 'foo' not found up to step 'f1'"
        )

        expect_error(
            pip$replace_step("f2", function(z = ~f2) z),
            "dependency 'f2' not found up to step 'f1'"
        )

        expect_error(
            pip$replace_step("f2", function(z = ~f3) z),
            "dependency 'f3' not found up to step 'f1'"
        )
    })

    test_that(
        "states are updated correctly",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(a = 2) a) |>
            pipe_add("f3", function(a = ~f2) a, keepOut = TRUE) |>
            pipe_add("f4", function(a = ~f3) a, keepOut = TRUE)

        pip$run()

        pip$replace_step("f2", function(a = 2) 2* a)
        expect_equal(pip$get_step("f1")$state, "Done")
        expect_equal(pip$get_step("f2")$state, "New")
        expect_equal(pip$get_step("f3")$state, "Outdated")
        expect_equal(pip$get_step("f4")$state, "Outdated")
    })
})


test_that("reset",
{
    expect_true(is.function(Pipeline$new("pipe")$reset))
    test_that(
        "after reset pipeline is the same as before the run",
    {
        p <- Pipeline$new("pipe", data = 1:2)
        p$add("f1", \(x = 1) x)
        p$add("f2", \(y = 1) y)

        p$run()
        expect_equal(
            p$collect_out(all = TRUE),
            list(data = 1:2, f1 = 1, f2 = 1)
        )
        expect_true(all(p$pipeline[["state"]] == "Done"))


        p$reset()
        expect_equal(
            p$collect_out(all = TRUE),
            list(data = NULL, f1 = NULL, f2 = NULL)
        )
        expect_true(all(p$pipeline[["state"]] == "New"))
    })
})

test_that("run",
{
    expect_true(is.function(Pipeline$new("pipe")$run))
    test_that("empty pipeline can be run",
    {
        expect_no_error(Pipeline$new("pipe1")$run())
    })

    test_that("returns the pipeline object",
    {
        pip <- Pipeline$new("pipe1")$run()
        expect_equal(pip$name, "pipe1")
    })

    test_that("if function result is a list, all names are preserved",
    {
        # Result list length == 1 - the critical case
        resultList = list(foo = 1)

        pip <- Pipeline$new("pipe") |>
            pipe_add("f1", function() resultList, keepOut = TRUE)

        out = pip$run()$collect_out()
        expect_equal(out[["f1"]], resultList)

        # Result list length > 1
        resultList = list(foo = 1, bar = 2)

        pip <- Pipeline$new("pipe") |>
            pipe_add("f1", function() resultList, keepOut = TRUE)

        out = pip$run()$collect_out()
        expect_equal(out[["f1"]], resultList)
    })

    test_that("pipeline execution is correct",
    {
        data <- 9
        pip <- Pipeline$new("pipe1", data = data)

        foo <- function(a = 1) a
        bar <- function(a, b) a + b

        a <- 5
        pip$add("f1", foo, params = list(a = a), keepOut = TRUE)
        pip$add("f2", bar, params = list(a = ~data, b = ~f1), keepOut = TRUE)

        pip$run()

        expect_equal(pip$pipeline[["out"]][[2]], a)
        expect_equal(pip$pipeline[["out"]][[3]], a + data)
    })


    test_that("pipeline execution can cope with void functions",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) {}, keepOut = TRUE) |>
            pipe_add("f2", function(b = 2) b, keepOut = TRUE)

        out <- pip$run()$collect_out()
        expect_equal(out, list(f1 = NULL, f2 = 2))
    })

    test_that(
        "if pipeline execution fails, the error message is returned as error",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(b = ~f1) stop("something went wrong"))

        expect_error(pip$run(), "something went wrong")
    })

    test_that(
        "can handle 'NULL' results",
    {
        pip <- Pipeline$new("pipe", data = 1) |>
            pipe_add("f1", function(x = ~data) x, keepOut = TRUE)

        out <- pip$run()$collect_out()
        expect_equal(out[["f1"]], 1)

        pip$set_data(NULL)
        out <- pip$run()$collect_out()
        expect_equal(out[["f1"]], NULL)
    })

    test_that(
        "can be run recursively to dynamically create and run pipelines",
    {
        pip <- Pipeline$new("pipe", data = 1) |>
            pipe_add(
                "f1",
                fun = function(data = 10) {
                    pip <- Pipeline$new("2nd pipe", data = data) |>
                        pipe_add("step1", function(x = ~data) x) |>
                        pipe_add("step2", function(x = ~step1) {
                            print(x)
                            2 * x
                        }, keepOut = TRUE)
                }
            )

        pip2 <- pip$run(recursive = TRUE)
        expect_equal(pip2$get_step_names(), c("data", "step1", "step2"))

        out <- pip2$collect_out()
        expect_equal(out[["step2"]], 20)
    })

    test_that("will not re-run steps that are already done unless forced",
    {
        pip <- Pipeline$new("pipe", data = 1) |>
            pipe_add("f1", function(x = 2) x) |>
            pipe_add("f2", function(y = ~f1) y + 1)

        pip$run()
        expect_equal(pip$get_step("f1")$state, "Done")
        expect_equal(pip$get_step("f2")$state, "Done")
        expect_equal(pip$pipeline[["out"]][[2]], 2)
        expect_equal(pip$pipeline[["out"]][[3]], 3)

        pip$pipeline[2, "out"] <- 0
        pip$pipeline[3, "out"] <- 0
        pip$run()
        expect_equal(pip$pipeline[["out"]][[2]], 0)
        expect_equal(pip$pipeline[["out"]][[3]], 0)

        # Set parameter, which outdates step and run again
        pip$run(force = TRUE)
        expect_equal(pip$pipeline[["out"]][[2]], 2)
        expect_equal(pip$pipeline[["out"]][[3]], 3)
    })

    test_that("will never re-run locked steps",
    {
        pip <- Pipeline$new("pipe", data = 1) |>
            pipe_add("f1", function(x = 2) x) |>
            pipe_add("f2", function(y = ~f1) y + 1)

        pip$run()
        expect_equal(pip$pipeline[["out"]][[2]], 2)
        expect_equal(pip$pipeline[["out"]][[3]], 3)

        pip$pipeline[2, "out"] <- 0
        pip$pipeline[3, "out"] <- 0
        pip$run()
        expect_equal(pip$pipeline[["out"]][[2]], 0)
        expect_equal(pip$pipeline[["out"]][[3]], 0)
        pip$lock_step("f1")
        pip$lock_step("f2")

        # Set parameter, which outdates step and run again
        pip$run(force = TRUE)
        expect_equal(pip$pipeline[["out"]][[2]], 0)
        expect_equal(pip$pipeline[["out"]][[3]], 0)
    })

    test_that("can clean unkept steps",
    {
        pip <- Pipeline$new("pipe", data = 1) |>
            pipe_add("f1", function(x = 2) x) |>
            pipe_add("f2", function(y = ~f1) y + 1)

        pip$run()
        expect_equal(pip$pipeline[["out"]][[2]], 2)
        expect_equal(pip$pipeline[["out"]][[3]], 3)

        pip$run(cleanUnkept = TRUE)
        expect_true(all(sapply(pip$pipeline[["out"]], is.null)))

        pip$set_keep_out("f1", TRUE)
        pip$run(cleanUnkept = TRUE)
        expect_equal(pip$pipeline[["out"]], list(NULL, 2, NULL))
    })

    test_that("logs warning without interrupting the run",
    {
        pip <- Pipeline$new("pipe", data = 1) |>
            pipe_add("f1", function(x = 2) x) |>
            pipe_add("f2", function(x = ~f1) {
                warning("something might be wrong")
                x
            }) |>
            pipe_add("f3", function(x = ~f2) x)

        log <- utils::capture.output(
            expect_warning(pip$run(), "something might be wrong")
        )

        Filter(log, f = function(x) x |>
            startsWith("WARN")) |>
            grepl(pattern = "something might be wrong") |>
            expect_true()

        wasRunTillEnd <- pip$get_out("f3") == 2
        expect_true(wasRunTillEnd)
    })

    test_that("logs error and stops at failed step",
    {
        pip <- Pipeline$new("pipe", data = 1) |>
            pipe_add("f1", function(x = 2) x) |>
            pipe_add("f2", function(x = ~f1) {
                stop("something went wrong")
                x
            }) |>
            pipe_add("f3", function(x = ~f2) x)

        log <- utils::capture.output(
            expect_error(pip$run(), "something went wrong")
        )

        Filter(log, f = function(x) x |>
            startsWith("ERROR")) |>
            grepl(pattern = "something went wrong") |>
            expect_true()

        wasRunTillEnd <- isTRUE(pip$get_out("f3") == 2)
        expect_false(wasRunTillEnd)
    })

    test_that("can show progress",
    {
        pip <- Pipeline$new("pipe", data = 1) |>
            pipe_add("f1", function(x = 2) x) |>
            pipe_add("f2", function(x = ~f1) x)

        m <- mockery::mock()
        pip$run(progress = m)

        args <- mockery::mock_args(m)

        expect_equal(length(m), pip$length())
        expect_equal(args[[1]][[1]], 1)
        expect_equal(args[[1]][["detail"]], "data")
        expect_equal(args[[2]][[1]], 2)
        expect_equal(args[[2]][["detail"]], "f1")
        expect_equal(args[[3]][[1]], 3)
        expect_equal(args[[3]][["detail"]], "f2")
    })

    test_that("works with Param objects",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add(
                "f1",
                function(
                    x = new("NumericParam", "x", value = 1),
                    y = new("NumericParam", "y", value = 2)
                ) {
                    x + y
                }
            ) |>
            pipe_add(
                "f2",
                function(
                    s1 = new("StringParam", "s1", "Hello"),
                    s2 = new("StringParam", "s2", "World")
                ) {
                    paste(s1, s2)
                }
            )

        pip$run()
        pip$get_out("f1") |> expect_equal(3)
        pip$get_out("f2") |> expect_equal("Hello World")
    })
})



test_that("run_step",
{
    expect_true(is.function(Pipeline$new("pipe")$run_step))

    test_that("pipeline can be run at given step",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("A", function(a = 1) a)

        expect_no_error(pip$run_step("A"))
    })


    test_that("upstream steps are by default run with given step",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("A", function(a = 1) a) |>
            pipe_add("B", function(b = ~A) c(b, 2)) |>
            pipe_add("C", function(c = ~B) c(c, 3))

        pip$run_step("B")

        expect_equal(pip$get_out("A"), 1)
        expect_equal(pip$get_out("B"), c(1, 2))
        expect_true(is.null(pip$get_out("C")))
    })

    test_that("runs upstream steps in correct order",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("A", function(a = 1) a) |>
            pipe_add("B", function(b = ~A) c(b, 2)) |>
            pipe_add("C", function(c = ~B) c(c, 3))

        pip$run_step("C")
        expect_equal(pip$get_out("C"), 1:3)
    })

    test_that("runs downstream steps in correct order",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("A", function(a = 1) a) |>
            pipe_add("B", function(b = ~A) c(b, 2)) |>
            pipe_add("C", function(c = ~B) c(c, 3))

        pip$run_step("A", downstream = TRUE)
        expect_equal(pip$get_out("C"), 1:3)
    })

    test_that("pipeline can be run at given step excluding
        all upstream dependencies",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("A", function(a = 1) a) |>
            pipe_add("B", function(b = ~A) c(b, 2)) |>
            pipe_add("C", function(c = ~B) c(c, 3))

        pip$run_step("B", upstream = FALSE)
        expect_true(is.null(pip$get_out("A")))
        expect_equal(pip$get_out("B"), 2)
        expect_true(is.null(pip$get_out("C")))
    })

    test_that("pipeline can be run at given step excluding upstream
        but including downstream dependencies",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("A", function(a = 1) a) |>
            pipe_add("B", function(b = ~A) c(b, 2)) |>
            pipe_add("C", function(c = ~B) c(c, 3))


        pip$run_step(
            "B",
            upstream = FALSE,
            downstream = TRUE
        )
        expect_true(is.null(pip$get_out("A")))
        expect_equal(pip$get_out("B"), 2)
        expect_equal(pip$get_out("C"), c(2, 3))
    })

    test_that("pipeline can be run at given step including
        up- and downstream dependencies",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("A", function(a = 1) a) |>
            pipe_add("B", function(b = ~A) c(b, 2)) |>
            pipe_add("C", function(c = ~B) c(c, 3))


        pip$run_step(
            "B", upstream = TRUE, downstream = TRUE
        )
        expect_equal(pip$get_out("A"), 1)
        expect_equal(pip$get_out("B"), c(1, 2))
        expect_equal(pip$get_out("C"), c(1, 2, 3))
    })

    test_that("if not marked as keepOut, output of run steps can be cleaned",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("A", function(a = 1) a)


        pip$run_step("A", cleanUnkept = TRUE)
        expect_true(is.null(pip$get_out("A")))

        pip$set_keep_out("A", TRUE)$run_step("A", cleanUnkept = TRUE)
        expect_false(is.null(pip$get_out("A")))
    })

    test_that("up- and downstream steps are marked in log",
    {
        lgr::unsuspend_logging()
        on.exit(lgr::suspend_logging())

        pip <- Pipeline$new("pipe") |>
            pipe_add("A", function(a = 1) a) |>
            pipe_add("B", function(b = ~A) c(b, 2)) |>
            pipe_add("C", function(c = ~B) c(c, 3))

        logOut <- utils::capture.output(
            pip$run_step("B", upstream = TRUE, downstream = TRUE)
        )

        contains <- function(x, pattern) {
            grepl(pattern = pattern, x = x, fixed = TRUE)
        }

        expect_true(logOut[2] |> contains("Step 1/3 A (upstream)"))
        expect_true(logOut[3] |> contains("Step 2/3 B"))
        expect_true(logOut[4] |> contains("Step 3/3 C (downstream)"))
    })

    test_that(
        "updates the timestamp of the run steps",
    {
        pip <- Pipeline$new("pipe", data = 1) |>
            pipe_add("f1", function(x = ~data) x, keepOut = TRUE)

        before <- pip$pipeline[["time"]]
        Sys.sleep(1)

        pip$run_step("f1", upstream = FALSE)
        after <- pip$pipeline[["time"]]

        expect_equal(before[1], after[1])
        expect_true(before[2] < after[2])
    })

    test_that(
        "updates the state of the run steps",
    {
        pip <- Pipeline$new("pipe", data = 1) |>
            pipe_add("f1", function(x = ~data) x, keepOut = TRUE)

        before <- pip$pipeline[["state"]]
        pip$run_step("f1", upstream = FALSE)
        after <- pip$pipeline[["state"]]

        expect_equal(before, c("New", "New"))
        expect_equal(after, c("New", "Done"))
    })

    test_that("will never re-run locked step",
    {
        pip <- Pipeline$new("pipe", data = 1) |>
            pipe_add("f1", function(x = 2) x) |>
            pipe_add("f2", function(y = ~f1) y + 1)

        pip$run()
        expect_equal(pip$pipeline[["out"]][[2]], 2)
        expect_equal(pip$pipeline[["out"]][[3]], 3)

        pip$pipeline[2, "out"] <- 0
        pip$pipeline[3, "out"] <- 0
        pip$lock_step("f1")
        pip$run_step("f1", downstream = TRUE)

        expect_equal(pip$pipeline[["out"]][[2]], 0)
        expect_equal(pip$pipeline[["out"]][[3]], 1)
    })

    test_that("can clean unkept steps",
    {
        pip <- Pipeline$new("pipe", data = 1) |>
            pipe_add("f1", function(x = 2) x) |>
            pipe_add("f2", function(y = ~f1) y + 1)

        pip$run_step("f1", downstream = TRUE, cleanUnkept = TRUE)
        expect_true(all(sapply(pip$pipeline[["out"]], is.null)))

        pip$set_keep_out("f1", TRUE)
        pip$run_step("f1", downstream = TRUE, cleanUnkept = TRUE)
        expect_equal(pip$pipeline[["out"]], list(NULL, 2, NULL))
    })
})



test_that("set_data",
{
    expect_true(is.function(Pipeline$new("pipe")$set_data))

    test_that("data can be set later after pipeline definition",
    {
        dat <- data.frame(a = 1:2, b = 1:2)

        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(x = ~data) x, keepOut = TRUE)

        out <- pip$run()$collect_out()
        expect_equal(out[["f1"]], NULL)

        pip$set_data(dat)

        out <- pip$run()$collect_out()
        expect_equal(out[["f1"]], dat)
    })

    test_that("if data is set, all dependent steps are set to outdated",
    {
        dat <- data.frame(a = 1:2, b = 1:2)

        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(x = ~data) x, keepOut = TRUE) |>
            pipe_add("f2", function(x = ~f1) x, keepOut = TRUE)

        pip$run()

        expect_equal(pip$get_step("f1")$state, "Done")
        expect_equal(pip$get_step("f2")$state, "Done")
        pip$set_data(dat)
        expect_equal(pip$get_step("f1")$state, "Outdated")
        expect_equal(pip$get_step("f2")$state, "Outdated")
    })
})



test_that("set_data_split",
{
    expect_true(is.function(Pipeline$new("pipe")$set_data_split))

    test_that("the new steps have the names of the list attached",
    {
        dataList <- list(A = 1, B = 2)

        pip <- Pipeline$new("pipe") |>
            pipe_add("f1", function(a = 1) a)

        pip$set_data_split(dataList)

        pip$get_step_names() |>
            expect_equal(c("data.A", "f1.A", "data.B", "f1.B"))
    })

    test_that("the separator used in the creation of the new steps
    can be customized",
    {
        dataList <- list(A = 1, B = 2)

        pip <- Pipeline$new("pipe") |>
            pipe_add("f1", function(a = 1) a)

        pip$set_data_split(dataList, sep = "_")

        pip$get_step_names() |>
            expect_equal(c("data_A", "f1_A", "data_B", "f1_B"))
    })


    test_that("simple split pipeline computes results as expected",
    {
        dataList <- list(A = 1, B = 2, C = 3)
        pip <- Pipeline$new("pipe") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(a = ~f1, b = ~data) {
                b + a
            }, keepOut = TRUE)

        pip$set_data_split(dataList)

        out <- pip$run()$collect_out()
        expect_equivalent(
            unlist(out),
            unlist(lapply(dataList, function(x) x + 1))
        )
    })


    test_that(
        "split pipeline by default overrides output groups according to split",
    {
        dataList <- list(A = 1, B = 2)

        pip <- Pipeline$new("pipe") |>
            pipe_add("f0", function(a = 1) a, group = "id") |>
            pipe_add("f1", function(a = 1) a, group = "id") |>
            pipe_add("f2", function(a = 2) a)

        pip$set_data_split(dataList)

        out <- pip$run()$collect_out(all = TRUE)
        expect_equal(names(out), names(dataList))
    })

    test_that("the grouping override can be omitted",
    {
        dataList <- list(A = 1, B = 2)

        pip <- Pipeline$new("pipe") |>
            pipe_add("f0", function(a = 1) a, group = "id") |>
            pipe_add("f1", function(a = 1) a, group = "id") |>
            pipe_add("f2", function(a = 2) a)

        pip$set_data_split(dataList, groupBySplit = FALSE)

        out <- pip$run()$collect_out(all = TRUE)

        expect_equal(
            names(out),
            c("data.A", "id.A", "f2.A", "data.B", "id.B", "f2.B")
        )
    })

    test_that("the separator used in the creation of the groups
        can be customized",
    {
        dataList <- list(A = 1, B = 2)

        pip <- Pipeline$new("pipe") |>
            pipe_add("f0", function(a = 1) a, group = "id") |>
            pipe_add("f1", function(a = 1) a, group = "id") |>
            pipe_add("f2", function(a = 2) a)

        pip$set_data_split(dataList, groupBySplit = FALSE, sep = "_")

        out <- pip$run()$collect_out(all = TRUE)

        expect_equal(
            names(out),
            c("data_A", "id_A", "f2_A", "data_B", "id_B", "f2_B")
        )
    })

    test_that("split pipeline works for list of data frames",
    {
        dat <- data.frame(x = 1:2, y = 1:2, z = 1:2)
        dataList <- list(A = dat, B = dat, C = dat)
        pip <- Pipeline$new("pipe") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(a = ~f1, b = ~data) b, keepOut = TRUE) |>
            pipe_add(
                "f3", function(a = ~f1, b = ~data) b[, 2:3], keepOut = TRUE
            )

        pip$set_data_split(dataList)

        out <- pip$run()$collect_out()

        expect_equal(out[["A"]], c(f2.A = list(dat), f3.A = list(dat[, 2:3])))
        expect_equal(out[["B"]], c(f2.B = list(dat), f3.B = list(dat[, 2:3])))
        expect_equal(out[["C"]], c(f2.C = list(dat), f3.C = list(dat[, 2:3])))
    })


    test_that("if unnamed list of data frames, they are named with numbers",
    {
        dat <- data.frame(x = 1:2, y = 1:2, z = 1:2)
        dataList <- list(dat, dat)
        pip <- Pipeline$new("pipe") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(a = ~f1, b = ~data) b, keepOut = TRUE)

        pip$set_data_split(dataList)

        out <- pip$run()$collect_out()

        expect_equal(out[["1"]], dat)
        expect_equal(out[["2"]], dat)
    })


    test_that(
        "depends are updated correctly, if data split on subset of pipeline",
    {
        dat1 <- data.frame(x = 1:2)
        dat2 <- data.frame(y = 1:2)
        dataList <- list(dat1, dat2)

        pip <- Pipeline$new("pipe") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(a = ~f1, b = ~data) b, keepOut = TRUE) |>
            pipe_add(
                "f3", function(x = ~f1, y = ~f2) list(x, y), keepOut = TRUE
            ) |>
            pipe_add("f4", function(x = ~f3) x[[1]], keepOut = TRUE)

        pip$set_data_split(dataList, toStep = "f2")

        ee = expect_equivalent
        pp = pip$pipeline

        depends <- pip$get_depends()

        expect_equal(depends[["f2.1"]], c(a = "f1.1", b = "data.1"))
        expect_equal(depends[["f2.2"]], c(a = "f1.2", b = "data.2"))

        # Pipeline was not split for f3, which therefore has parameters that
        # each depend on two steps
        expect_equal(
            depends[["f3"]],
            list(x = c("f1.1", "f1.2"), y = c("f2.1", "f2.2"))
        )

        # Pipeline was not split for f4, so just depdends on f3
        ee(depends[["f4"]], c(x = "f3"))


        out <- pip$run()$collect_out()

        expect_equal(out[["1"]], dat1)
        expect_equal(out[["2"]], dat2)
        expected_f3_res = list(
            list("f1.1" = 1, "f1.2" = 1),
            list("f2.1" = dat1, "f2.2" = dat2)
        )
        expect_equal(out[["f3"]], expected_f3_res)
        expect_equal(out[["f4"]], expected_f3_res[[1]])
    })

    test_that("split data set can be created dynamically",
    {
        data = data.frame(a = 1:10, group = c("a", "b"))

        pip <- Pipeline$new("pipe", data = data) |>

            pipe_add("split_data_step",
                function(.self = NULL, data = ~data)
                {
                    splitData = split(data, data[, "group"])

                    .self$remove_step("split_data_step")
                    .self$set_data_split(splitData)
                    .self$name = paste(.self$name, "after data split")
                    .self
                }
            ) |>

            pipe_add("f1", function(data = ~data) {
                data
            }, keepOut = TRUE)

        pip$set_params(list(.self = pip))

        out <- pip$run(recursive = TRUE)$collect_out()

        expect_equivalent(out, split(data, data[, "group"]))
    })
})



test_that("set_keep_out",
{
    expect_true(is.function(Pipeline$new("pipe")$set_keep_out))

    test_that("keep-out state can be set",
    {
        pip <- Pipeline$new("pipe1", data = 0) |>
            pipe_add("f1", function(a = 1) a)

        out <- pip$run()$collect_out()
        expect_false("f1" %in% names(out))

        out <- pip$set_keep_out("f1", keepOut = TRUE)$collect_out()
        expect_true("f1" %in% names(out))

        out <- pip$set_keep_out("f1", keepOut = FALSE)$collect_out()
        expect_false("f1" %in% names(out))
    })

    test_that("step must be a string and exist",
    {
        pip <- Pipeline$new("pipe1")
        pip <- Pipeline$new("pipe1", data = 0) |>
            pipe_add("f1", function(a = 1) a)

        expect_error(
            pip$set_keep_out(1),
            "is_string(step)",
            fixed = TRUE
        )

        expect_error(
            pip$set_keep_out("f2"),
            "step 'f2' does not exist",
            fixed = TRUE
        )
    })

    test_that("state must be logical",
    {
        pip <- Pipeline$new("pipe1", data = 0) |>
            pipe_add("f1", function(a = 1) a)

        expect_error(
            pip$set_keep_out("f1", keepOut = 1),
            "is.logical(keepOut)",
            fixed = TRUE
        )
    })
})




test_that("set_params",
{
    expect_true(is.function(Pipeline$new("pipe")$set_params))

    test_that("parameters can be set commonly on existing pipeline",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(a = 2, b = 3) a) |>
            pipe_add("f3", function(a = 4, b = 5) a)

        before <- pip$get_params()

        after <- pip$set_params(list(a = 9, b = 99))$get_params()
        expect_equal(after, list(
            f1 = list(a = 9),
            f2 = list(a = 9, b = 99),
            f3 = list(a = 9, b = 99)
        ))
    })

    test_that(
        "parameters depending on other steps are protected
        from being overwritten",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(a = 2, b = ~f1) a) |>
            pipe_add("f3", function(a = ~f2, b = 5) a)

        before <- pip$get_params()
        expect_equal(
            before,
            list(
                f1 = list(a = 1),
                f2 = list(a = 2),
                f3 = list(b = 5)
            )
        )

        after <- pip$set_params(list(a = 9, b = 99))$get_params()
        expect_equal(
            after,
            list(
                f1 = list(a = 9),
                f2 = list(a = 9),
                f3 = list(b = 99)
            )
        )
    })


    test_that("trying to set undefined parameters is signaled with a warning",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a)

        expect_warning(
            pip$set_params(list(a = 9, b = 9, c = 9)),
            "Trying to set parameters not defined in the pipeline: b, c",
            fixed = TRUE
        )
    })

    test_that("warning for undefined parameters can be omitted",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a)

        expect_no_warning(
            pip$set_params(list(a = 9, b = 9, c = 9),
                warnUndefined = FALSE
            )
        )
    })

    test_that(
        "after setting a single parameter the params entry is still a list",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a)

        expect_equal(pip$pipeline[["params"]][[2]], list(a = 1))

        pip$set_params(list(a = 9))
        expect_equal(pip$pipeline[["params"]][[2]], list(a = 9))
    })

    test_that(
        "hidden parameters can be set as well",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1, .b = 2) a)

        pip$set_params(list(a = 9, .b = 10))
        pp <- pip$get_params(ignoreHidden = FALSE)
        expect_equal(pp, list(f1 = list(a = 9, .b = 10)))
    })

    test_that(
        "trying to set locked parameters is ignored until they are unlocked",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1, b = 2) a + b) |>
            pipe_add("f2", function(a = 1, b = 2) a + b)

        pip$lock_step("f1")
        expect_message(
            pip$set_params(list(a = 9, b = 99)),
            "skipping setting parameters a, b at locked step 'f1'"
        )

        pip$get_params_at_step("f1") |> expect_equal(list(a = 1, b = 2))
        pip$get_params_at_step("f2") |> expect_equal(list(a = 9, b = 99))

        pip$unlock_step("f1")
        pip$set_params(list(a = 9, b = 99))
        pip$get_params_at_step("f1") |> expect_equal(list(a = 9, b = 99))
    })
})


test_that("set_params_at_step",
{
    expect_true(is.function(Pipeline$new("pipe")$set_params_at_step))

    test_that("parameters can be set at given step",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1, b = 2) a + b) |>
            pipe_add("f2", function(x = 1) x)

        expect_equal(pip$get_params_at_step("f1"), list(a = 1, b = 2))

        pip$set_params_at_step("f1", list(a = 9, b = 99))
        expect_equal(pip$get_params_at_step("f1"), list(a = 9, b = 99))

        pip$set_params_at_step("f2", list(x = 9))
        expect_equal(pip$get_params_at_step("f2"), list(x = 9))
    })

    test_that("step must be passed as a string and params as a list",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1, b = 2) a + b)

        expect_error(
            pip$set_params_at_step(1, list(a = 9, b = 99)),
            "is_string(step) is not TRUE",
            fixed = TRUE
        )

        expect_error(
            pip$set_params_at_step("f1", params = c(a = 9, b = 99)),
            "is.list(params) is not TRUE",
            fixed = TRUE
        )
    })

    test_that("hidden parameters can be set as well",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1, .b = 2) a + b)

        pip$set_params_at_step("f1", list(a = 9, .b = 99))

        expect_equal(
            pip$get_params_at_step("f1", ignoreHidden = FALSE),
            list(a = 9, .b = 99)
        )
    })


    test_that("trying to set undefined parameter signals an error",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1, b = 2) a + b)

        expect_error(
            pip$set_params_at_step("f1", list(a = 9, z = 99)),
            "Unable to set parameter(s) z at step f1 - candidates are a, b",
            fixed = TRUE
        )
    })

    test_that("trying to set locked parameter is ignored until it is unlocked",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1, b = 2) a + b)

        pip$lock_step("f1")
        expect_message(
            pip$set_params_at_step("f1", list(a = 9, b = 99)),
            "skipping setting parameters a, b at locked step 'f1'"
        )

        pip$get_params_at_step("f1") |>
            expect_equal(list(a = 1, b = 2))

        pip$unlock_step("f1")
        pip$set_params_at_step("f1", list(a = 9, b = 99))
        pip$get_params_at_step("f1") |>
            expect_equal(list(a = 9, b = 99))
    })


    test_that("setting values for bound parameters is not allowed",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1, b = 2) a + b) |>
            pipe_add("f2", function(x = 1, y = ~f1) x + y)

        expect_error(
            pip$set_params_at_step("f2", list(x = 9, y = 99)),
            "Unable to set parameter(s) y at step f2 - candidates are x",
            fixed = TRUE
        )
    })


    test_that(
        "states of affected steps are updated once the pipeline was run",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(a = ~f1) a) |>
            pipe_add("f3", function(a = ~f2) a) |>
            pipe_add("f4", function(a = ~data) a)

        pip$set_params_at_step("f1", params = list(a = 2))
        expect_true(all(pip$pipeline[["state"]] == "New"))

        pip$run()
        pip$set_params_at_step("f1", params = list(a = 3))
        expect_equal(pip$get_step("data")$state, "Done")
        expect_equal(pip$get_step("f1")$state, "Outdated")
        expect_equal(pip$get_step("f2")$state, "Outdated")
        expect_equal(pip$get_step("f3")$state, "Outdated")
        expect_equal(pip$get_step("f4")$state, "Done")

        pip$run()
        expect_true(all(pip$pipeline[["state"]] == "Done"))
    })


    test_that("parameters can be set to NULL",
    {
        pip <- Pipeline$new("pipe1") |>
        pipe_add("f1", function(a = NULL, b = 1) a)

        pip$set_params_at_step("f1", list(a = 1, b = NULL))

        expect_equal(
            pip$get_params_at_step("f1"),
            list(a = 1, b = NULL)
        )
    })

    test_that("preserves Param objects",
    {
        pip <- Pipeline$new("pipe1") |>
        pipe_add("f1", function(
            a = 1,
            b = new("NumericParam", "num", 2)) a + b
        )

        pip$set_params_at_step("f1", list(a = 3, b = 4))

        params <- pip$get_params_at_step("f1")
        expect_equal(params$a, 3)
        expect_true(params$b |> is("NumericParam"))
        expect_equal(params$b@value, 4)
    })
})


test_that("split",
{
    expect_true(is.function(Pipeline$new("pipe")$split))

    test_that("pipeline split of initial pipeline gives the expected result",
    {
        pip <- Pipeline$new("pipe")
        res <- pip$split()

        expect_true(is.list(res))
        expect_equal(res[[1]]$name, "pipe1")
        expect_equal(res[[1]]$pipeline, pip$pipeline)
    })


    test_that("pipeline with two indepdendent groups is split correctly",
    {
        pip <- Pipeline$new("pipe")

        pip$add("f1", \(a = ~data) a)
        pip$add("f2", \(a = 1) a)
        pip$add("f3", \(a = ~f2) a)
        pip$add("f4", \(a = ~f1) a)
        pip$run()

        res <- pip$split()
        pip1 <- res[[1]]
        pip2 <- res[[2]]
        expect_equal(pip1$name, "pipe1")
        expect_equal(pip2$name, "pipe2")

        expect_equal(pip1$get_step_names(), c("f2", "f3"))
        expect_equal(pip2$get_step_names(), c("data", "f1", "f4"))

        expect_equal(
            pip1$collect_out(all = TRUE),
            pip$collect_out(all = TRUE)[c("f2", "f3")]
        )
        expect_equal(
            pip2$collect_out(all = TRUE),
            pip$collect_out(all = TRUE)[c("data", "f1", "f4")]
        )
    })

    test_that(
        "split is done correctly for complete data split",
    {
        dat1 <- data.frame(x = 1:2)
        dat2 <- data.frame(y = 1:2)
        dataList <- list(dat1, dat2)

        pip <- Pipeline$new("pipe") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(a = ~f1, b = 2) b) |>
            pipe_add("f3", \(x = ~f1, y = ~f2) x + y)

        pip$set_data_split(dataList)

        res <- pip$split()
        steps <- lapply(res, function(x) x$get_step_names())

        expect_equal(
            steps,
            list(
                "data.1",
                c("f1.1", "f2.1", "f3.1"),
                "data.2",
                c("f1.2", "f2.2", "f3.2")
            )
        )
    })
})



test_that("unlock_step",
{
    expect_true(is.function(Pipeline$new("pipe")$lock_step))

    test_that("sets state to 'unlocked' if it was locked before",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("f1", function(a = 1) a)

        pip$lock_step("f1")
        expect_equal(pip$get_step("f1")[["state"]], "Locked")

        pip$unlock_step("data")
        expect_equal(pip$get_step("data")[["state"]], "New")

        pip$unlock_step("f1")
        expect_equal(pip$get_step("f1")[["state"]], "Unlocked")

        pip
    })
})



# Pipeline logging

test_that("pipeline logging works as expected",
{
    expect_no_error(set_log_layout("json"))
    on.exit(set_log_layout("text"))

    test_that("pipeline logging is done in json format",
    {
        dat <- data.frame(a = 1:2, b = 1:2)
        pip <- Pipeline$new("pipe1", data = dat)

        log <- utils::capture.output(pip$run())
        isValidJSON <- sapply(log, jsonlite::validate)
        expect_true(all(isValidJSON))
    })

    test_that("each step is logged with its name",
    {
        lgr::unsuspend_logging()
        on.exit(lgr::suspend_logging())

        dat <- data.frame(a = 1:2, b = 1:2)
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(x = ~data) x)

        log <- utils::capture.output(pip$run())

        step1 = jsonlite::fromJSON(log[2])
        step2 = jsonlite::fromJSON(log[3])

        expect_equal(step1[["message"]], "Step 1/2 data")
        expect_equal(step2[["message"]], "Step 2/2 f1")
    })

    test_that(
        "upon warning during run, both context and warn msg are logged",
    {
        lgr::unsuspend_logging()
        on.exit(lgr::suspend_logging())

        dat <- data.frame(a = 1:2, b = 1:2)
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function() warning("this is a warning"))

        expect_warning(log <- utils::capture.output(pip$run()))

        log_fields = lapply(head(log, -1), jsonlite::fromJSON)
        warnings = Filter(
            log_fields,
            f = function(x) x[["level"]] == "warn"
        )[[1]]

        expect_equal(warnings[["message"]], "this is a warning")
        expect_equal(warnings[["context"]], "Step 3 ('f2')")
    })


    test_that(
        "upon error during run, both context and error msg are logged",
    {
        lgr::unsuspend_logging()
        on.exit(lgr::suspend_logging())

        dat <- data.frame(a = 1:2, b = 1:2)
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function() stop("this is an error"))

        log <- utils::capture.output({
            tryCatch(pip$run(), error = identity)
        })

        log_fields = lapply(head(log, -1), jsonlite::fromJSON)
        last = tail(log_fields, 1)[[1]]

        expect_equal(last[["message"]], "this is an error")
        expect_equal(last[["context"]], "Step 3 ('f2')")
    })
})

# ---------------
# private methods
# ---------------

test_that("private methods work as expected",
{
    # Helper function to access private fields
    get_private <- function(x) {
        x[[".__enclos_env__"]]$private
    }

    pip <- Pipeline$new("pipe")
    expect_true(is.environment(get_private(pip)))

    test_that(".clean_out_not_kept",
    {
        pip <- expect_no_error(Pipeline$new("pipe"))
        f <- get_private(pip)$.clean_out_not_kept

        test_that("cleans all output if nothing is marked to be kept",
        {
            pip <- Pipeline$new("pipe1", data = 1) |>
                pipe_add("A", function(a = 2) a)

            fclean <- get_private(pip)$.clean_out_not_kept

            fexe <- get_private(pip)$.run_step
            fexe(step = "data")
            fexe(step = "A")

            expect_equal(pip$get_out("data"), 1)
            expect_equal(pip$get_out("A"), 2)
            fclean()

            hasCleanedData <- is.null(pip$get_out("data"))
            expect_true(hasCleanedData)
            hasCleanedA <- is.null(pip$get_out("A"))
            expect_true(hasCleanedA)
        })

        test_that("does not clean if marked to be kept",
        {
            pip <- Pipeline$new("pipe1", data = 1) |>
                pipe_add("A", function(a = 2) a, keepOut = TRUE)

            fclean <- get_private(pip)$.clean_out_not_kept

            fexe <- get_private(pip)$.run_step
            fexe(step = "data")
            fexe(step = "A")

            expect_equal(pip$get_out("data"), 1)
            expect_equal(pip$get_out("A"), 2)
            fclean()

            hasCleanedData <- is.null(pip$get_out("data"))
            expect_true(hasCleanedData)
            hasCleanedA <- is.null(pip$get_out("A"))
            expect_false(hasCleanedA)
        })
    })


    test_that(".create_edge_table",
    {
        pip <- expect_no_error(Pipeline$new("pipe"))
        f <- get_private(pip)$.create_edge_table

        test_that("returns a data frame with the expected columns",
        {
            pip <- Pipeline$new("pipe")
            f <- get_private(pip)$.create_edge_table

            res <- f()
            expect_true(is.data.frame(res))
            expected_columns <- c("from", "to", "arrows")
            expect_equal(colnames(res), expected_columns)
        })

        test_that("if no dependencies are defined, edge table is empty",
        {
            pip <- Pipeline$new("pipe")
            f <- get_private(pip)$.create_edge_table

            res <- f()
            hasEmptyEdgeTable <- nrow(res) == 0
            expect_true(hasEmptyEdgeTable)

            pip$add("f1", function(a = 1) a)
            res <- f()
            hasEmptyEdgeTable <- nrow(res) == 0
            expect_true(hasEmptyEdgeTable)

            pip$add("f2", function(a = ~f1) a)
            res <- f()
            hasEmptyEdgeTable <- nrow(res) == 0
            expect_false(hasEmptyEdgeTable)
        })

        test_that("adds one edge for each dependency",
        {
            pip <- Pipeline$new("pipe")
            f <- get_private(pip)$.create_edge_table

            # step1 is data
            pip$add("step2", function(a = 1) a)
            pip$add("step3", function(a = ~step2) a)
            pip$add("step4", function(a = ~step2, b = ~step3) a)
            pip$add("step5", function(a = ~data, b = ~step4) a)

            res <- f()
            expect_equal(nrow(res), 5)

            expect_equivalent(
                res["step3", c("from", "to")],      # from step2 to step3
                data.frame(from = 2, to = 3)
            )
            expect_equivalent(
                res["step4.a", c("from", "to")],    # from step2 to step4
                data.frame(from = 2, to = 4)
            )
            expect_equivalent(
                res["step4.b", c("from", "to")],   # from step3 to step4
                data.frame(from = 3, to = 4)
            )
            expect_equivalent(
                res["step5.a", c("from", "to")],   # from data to step5
                data.frame(from = 1, to = 5)
            )
            expect_equivalent(
                res["step5.b", c("from", "to")],   # from data to step5
                data.frame(from = 4, to = 5)
            )
        })

        test_that("can be created for certain groups",
        {
            pip <- Pipeline$new("pipe")

            # step1 is data
            pip$add("step2", \(a = ~data) a + 1, group = "add")
            pip$add("step3", \(a = ~step2) 2 * a, group = "mult")
            pip$add("step4", \(a = ~step2, b = ~step3) a + b, group = "add")
            pip$add("step5", \(a = ~data, b = ~step4) a * b, group = "mult")

            f <- get_private(pip)$.create_edge_table
            res.all <- f()

            res.add <- f(groups = "add")
            expect_equal(
                res.add,
                res.all[c("step2", "step4.a", "step4.b"), ]
            )

            res.mult <- f(groups = "mult")
            expect_equal(
                res.mult,
                res.all[c("step5.a", "step3", "step5.b"), ]
            )

            expect_equal(
                f(groups = c("data", "add", "mult")),
                f()
            )
        })

        test_that("signals bad group specification",
        {
            pip <- Pipeline$new("pipe")
            f <- get_private(pip)$.create_edge_table

            expect_error(
                f(groups = "foo"),
                "all(groups %in% self$pipeline[[\"group\"]]) is not TRUE",
                fixed = TRUE
            )
            expect_error(
                f(groups = 1),
                "is.character(groups) is not TRUE",
                fixed = TRUE
            )
        })

        test_that("can be created for parameters with multiple dependencies",
        {
            pip <- Pipeline$new("pipe")

            # step1 is data
            pip$add("step2", \(x = ~data) x + 1)
            pip$add("step3", \(y = ~step2) sum(unlist(y)))

            pip$set_data_split(
                list(data.frame(x = 1:2), data.frame(x = 3:4)),
                toStep = "step2"
            )

            # Step 3 depends on both step2.1 and step2.2
            expect_equal(
                pip$get_depends()$step3$y,
                c("step2.1", "step2.2")
            )

            f <- get_private(pip)$.create_edge_table
            res <- f()

            expectedRes <- data.frame(
                from = 1:4,
                to = c(2, 5, 4, 5),
                arrows = "to"
            )
            expect_equivalent(res, expectedRes)

            expect_equal(
                rownames(res),
                c("step2.1", "step3.y1", "step2.2", "step3.y2")
            )
        })
    })


    test_that(".create_node_table",
    {
        pip <- expect_no_error(Pipeline$new("pipe"))
        f <- get_private(pip)$.create_node_table

        test_that("returns a data frame with the expected columns",
        {
            pip <- Pipeline$new("pipe")
            f <- get_private(pip)$.create_node_table

            res <- f()
            expect_true(is.data.frame(res))
            expected_columns <- c(
                "id", "label", "group", "shape", "color", "title"
            )
            expect_equal(colnames(res), expected_columns)
        })

        test_that("adds one node for each step",
        {
            pip <- Pipeline$new("pipe")
            f <- get_private(pip)$.create_node_table

            res <- f()
            expect_equal(nrow(res), pip$length())

            pip$add("f2", function(a = 1) a)
            res <- f()
            expect_equal(nrow(res), pip$length())

            pip$add("f3", function(a = ~f2) a)
            res <- f()
            expect_equal(nrow(res), pip$length())
        })

        test_that("description is shown in the title",
        {
            pip <- Pipeline$new("pipe") |>
                pipe_add("step2", \(a = 1) a, description = "my 2nd step")

            f <- get_private(pip)$.create_node_table
            res <- f()
            expect_equal(res[2, "title"], "<p>my 2nd step</p>")

        })

        test_that("can be created for certain groups",
        {
            pip <- Pipeline$new("pipe")

            # step1 is data
            pip$add("step2", \(a = ~data) a + 1, group = "add")
            pip$add("step3", \(a = ~step2) 2 * a, group = "mult")
            pip$add("step4", \(a = ~step2, b = ~step3) a + b, group = "add")
            pip$add("step5", \(a = ~data, b = ~step4) a * b, group = "mult")

            f <- get_private(pip)$.create_node_table
            res.all <- f()

            res.add <- f(groups = "add")
            expect_equal(res.add, res.all[c(2, 4), ])

            res.mult <- f(groups = "mult")
            expect_equal(res.mult, res.all[c(3, 5), ])

            expect_equal(
                f(groups = c("data", "add", "mult")),
                f()
            )
        })

        test_that("signals bad group specification",
        {
            pip <- Pipeline$new("pipe")
            f <- get_private(pip)$.create_node_table

            expect_error(
                f(groups = "foo"),
                "all(groups %in% self$pipeline[[\"group\"]]) is not TRUE",
                fixed = TRUE
            )
            expect_error(
                f(groups = 1),
                "is.character(groups) is not TRUE",
                fixed = TRUE
            )
        })
    })


    test_that(".derive_dependencies",
    {
        pip <- expect_no_error(Pipeline$new("pipe"))
        f <- get_private(pip)$.derive_dependencies

        test_that("if no params are defined, dependencies are empty",
        {
            expect_equal(f(params = list(), step = "step"), character(0))
            expect_equal(f(params = list(a = 1), step = "step"), character(0))
        })

        test_that("signals bad input",
        {
            expect_error(
                f(params = list(), step = 1),
                "is_string(step) is not TRUE",
                fixed = TRUE
            )
            expect_error(
                f(params = list(), step = "step", toStep = 2),
                "is_string(toStep) is not TRUE",
                fixed = TRUE
            )
        })

        test_that("extracts all dependencies defined via step name",
        {
            pip <- Pipeline$new("pipe")
            f <- get_private(pip)$.derive_dependencies
            pip$add("f1", function(a = 1) a)
            pip$add("f2", function(b = 2) b)

            expect_equal(
                f(params = list(x = ~f1), step = "step3"),
                c(x = "f1")
            )
            expect_equal(
                f(params = list(x = ~f1, y = ~f2), step = "step3"),
                c(x = "f1", y = "f2")
            )
        })

        test_that("extracts all dependencies defined relative",
        {
            pip <- Pipeline$new("pipe")
            f <- get_private(pip)$.derive_dependencies
            pip$add("f1", function(a = 1) a)
            pip$add("f2", function(b = 2) b)

            expect_equal(
                f(params = list(x = ~-1), step = "step3"),
                c(x = "f2")
            )

            expect_equal(
                f(params = list(x = ~-1, y = ~-2), step = "step3"),
                c(x = "f2", y = "f1")
            )
        })

        test_that("extracts all dependencies if defined both ways",
        {
            pip <- Pipeline$new("pipe")
            f <- get_private(pip)$.derive_dependencies
            pip$add("f1", function(a = 1) a)
            pip$add("f2", function(b = 2) b)

            expect_equal(
                f(params = list(x = ~-1, y = ~f2), step = "step3"),
                c(x = "f2", y = "f2")
            )
        })
    })


    test_that(".extract_dependent_out",
    {
        pip <- expect_no_error(Pipeline$new("pipe"))
        f <- get_private(pip)$.extract_dependent_out

        test_that("if no depends, NULL is returned",
        {
            expect_true(is.null(f(depends = list(), out = list())))
        })

        test_that("signals badly typed input",
        {
            expect_error(
                f(1:2, list()),
                "is.character(depends) || is.list(depends) is not TRUE",
                fixed = TRUE
            )
            expect_error(
                f(list(), 1),
                "is.list(out) is not TRUE",
                fixed = TRUE
            )
        })

        test_that("signals if dependency is not found in out",
        {
            expect_no_error(
                f(depends = "foo", out = list("foo" = 1))
            )
            expect_error(
                f(depends = "foo", out = list()),
                "all(unlist(depends) %in% names(out)) is not TRUE",
                fixed = TRUE
            )

            # Dependencies can be lists of dependencies
            depends <- list(x = c("a", "b"))
            out <- list(a = 1)
            expect_error(f(depends, out))
        })

        test_that("output is extracted as expected",
        {
            out <- list(a = 1, b = 2, c = 3, d = 4)
            expect_equal(
                f(c(x = "a"), out),
                list(x = 1)
            )

            expect_equal(
                f(c(x = "a", y = "c"), out),
                list(x = 1, y = 3)
            )

            expect_equal(
                f(list(x = c("a", "c"), y = c("b", "d")), out),
                list(
                    x = list(a = 1, c = 3),
                    y = list(b = 2, d = 4)
                )
            )
        })
    })


    test_that(".extract_depends_from_param_list",
    {
        pip <- expect_no_error(Pipeline$new("pipe"))
        f <- get_private(pip)$.extract_depends_from_param_list

        test_that("if no params are defined, dependencies are empty",
        {
            expect_equal(f(params = list()), character(0))
        })

        test_that("params must be a list or NULL",
        {
            expect_equal(f(params = NULL), character(0))

            expect_error(
                f(params = c(a = 1)),
                "is.list(params) is not TRUE",
                fixed = TRUE
            )
        })

        test_that("if no dependencies are defined, nothing is extracted",
        {
            expect_equal(f(params = list(a = 1)), character(0))
            expect_equal(f(params = list(a = 1, b = 2)), character(0))
        })

        test_that("if dependencies are defined, they are extracted",
        {
            expect_equal(f(params = list(a = ~x)), c(a = "x"))
            expect_equal(
                f(params = list(a = ~x, b = ~-1)),
                c(a = "x", b = "-1")
            )
            expect_equal(
                f(params = list(a = ~x, b = ~-1, c = 1)),
                c(a = "x", b = "-1")
            )
        })
    })


    test_that(".get_depends_grouped",
    {
        pip <- expect_no_error(Pipeline$new("pipe"))
        f <- get_private(pip)$.get_depends_grouped

        test_that("grouped dependencies are obtained correctly",
        {
            pip <- Pipeline$new("pipe")
            f <- get_private(pip)$.get_depends_grouped

            expect_equal(f(), list("data"))

            pip$add("f1", \(a = ~data) a)
            expect_equal(f(), list(c("data", "f1")))

            pip$add("f2", \(a = 1) a)
            pip$add("f3", \(a = ~f2) a)
            pip$add("f4", \(a = ~f1) a)

            expect_equal(
                f(),
                list(
                    c("f2", "f3"),
                    c("data", "f1", "f4")
                )
            )
        })

        test_that("grouped dependencies are obtained correctly for
            complete data split",
        {
            dat1 <- data.frame(x = 1:2)
            dat2 <- data.frame(y = 1:2)
            dataList <- list(dat1, dat2)

            pip <- Pipeline$new("pipe") |>
                pipe_add("f1", \(a = 1) a) |>
                pipe_add("f2", \(a = ~f1, b = 2) b) |>
                pipe_add("f3", \(x = ~f1, y = ~f2) x + y)


            f <- get_private(pip)$.get_depends_grouped
            pip$set_data_split(dataList, toStep = "f3")

            expect_equal(
                f(),
                list(
                    "data.1",
                    c("f1.1", "f2.1", "f3.1"),
                    "data.2",
                    c("f1.2", "f2.2", "f3.2")
                )
            )
        })

        test_that("grouped dependencies are obtained correctly for
            partial data split",
        {
            dat1 <- data.frame(x = 1:2)
            dat2 <- data.frame(y = 1:2)
            dataList <- list(dat1, dat2)

            pip <- Pipeline$new("pipe") |>
                pipe_add("f1", \(a = 1) a) |>
                pipe_add("f2", \(a = ~f1, b = 2) b) |>
                pipe_add("f3", \(x = ~f1, y = ~f2) x + y)

            f <- get_private(pip)$.get_depends_grouped
            pip$set_data_split(dataList, toStep = "f2")

            expect_equal(
                f(),
                list(
                    "data.1",
                    "data.2",
                    c("f1.1", "f2.1", "f1.2", "f2.2", "f3")
                )
            )
        })

        test_that(
            "if all steps somehow are dependent on each other,
                just one group is returned",
        {
            pip <- Pipeline$new("pipe")
            f <- get_private(pip)$.get_depends_grouped

            pip$add("f1", \(a = ~data) a)
            pip$add("f2", \(a = 1) a)
            pip$add("f3", \(a = ~f2) a)
            pip$add("f4", \(a = ~f1, b = ~f3) a)

            expect_equal(
                unlist(f()),
                pip$get_step_names()
            )
        })
    })


    test_that(".get_downstream_depends",
    {
        pip <- expect_no_error(Pipeline$new("pipe"))
        f <- get_private(pip)$.get_downstream_depends

        test_that("badly typed inputs are signalled",
        {
            expect_error(
                f(step = 1),
                "is_string(step) is not TRUE",
                fixed = TRUE
            )

            expect_error(
                f(step = "foo", depends = c(a = 1)),
                "is.character(depends) || is.list(depends) is not TRUE",
                fixed = TRUE
            )

            expect_error(
                f(step = "foo", depends = list(), recursive = 1),
                "is.logical(recursive)",
                fixed = TRUE
            )
        })

        test_that("if no dependencies, empty character vector is returned",
        {
            expect_equal(f(step = "foo", depends = character()), character(0))
            expect_equal(f(step = "foo", depends = list()), character(0))
        })

        test_that(
            "if no depends, recursive should give same as non-recursiv call",
        {
            expect_equal(
                f(step = "foo", depends = list()),
                f(step = "foo", depends = list(), recursive = FALSE)
            )
        })


        test_that("dependencies by default are determined recursively",
        {
            depends <- list(
                f1 = c(a = "f0"),
                f2 = c(a = "f1"),
                f3 = c(a = "f2")
            )

            expect_equal(f("f3", depends), character(0))
            expect_equal(f("f2", depends), c("f3"))
            expect_equal(f("f1", depends), c("f2", "f3"))
            expect_equal(f("f0", depends), c("f1", "f2", "f3"))
        })

        test_that("returned dependencies are unique",
        {
            depends <- list(
                f1 = c(a = "f0"),
                f2 = c(a = "f0", b = "f1"),
                f3 = c(a = "f0", b = "f1", c = "f2")
            )

            expect_equal(f("f3", depends), character(0))
            expect_equal(f("f2", depends), c("f3"))
            expect_equal(f("f1", depends), c("f2", "f3"))
            expect_equal(f("f0", depends), c("f1", "f2", "f3"))
        })

        test_that("dependencies can be determined non-recursively",
        {
            depends <- list(
                f1 = c(a = "f0"),
                f2 = c(a = "f1"),
                f3 = c(a = "f2")
            )

            expect_equal(f("f3", depends, recursive = FALSE), character(0))
            expect_equal(f("f2", depends, recursive = FALSE), c("f3"))
            expect_equal(f("f1", depends, recursive = FALSE), c("f2"))
            expect_equal(f("f0", depends, recursive = FALSE), c("f1"))
        })

        test_that("works with multiple dependencies given in sublist",
        {
            depends <- list(
                f2 = c(a = "f1", b = "data"),
                f3 = list(
                    x = c("f0", "f1"),
                    y = c("f1", "f2")
                )
            )

            expect_equal(f("f1", depends), c("f2", "f3"))
        })
    })


    test_that(".get_last_step",
    {
        pip <- expect_no_error(Pipeline$new("pipe"))
        f <- get_private(pip)$.get_last_step

        expect_equal(f(), "data")

        pip$add("f1", function(a = 1) a)
        expect_equal(f(), "f1")

        pip$add("f2", function(a = 1) a)
        expect_equal(f(), "f2")

        pip$pop_step()
        expect_equal(f(), "f1")
    })


    test_that(".get_upstream_depends",
    {
        pip <- expect_no_error(Pipeline$new("pipe"))
        f <- get_private(pip)$.get_upstream_depends

        test_that("badly typed inputs are signalled",
        {
            expect_error(
                f(step = 1),
                "is_string(step) is not TRUE",
                fixed = TRUE
            )

            expect_error(
                f(step = "foo", depends = c(a = 1)),
                "is.character(depends) || is.list(depends) is not TRUE",
                fixed = TRUE
            )

            expect_error(
                f(step = "foo", depends = list(), recursive = 1),
                "is.logical(recursive)",
                fixed = TRUE
            )
        })

        test_that("if no dependencies, empty character vector is returned",
        {
            expect_equal(f(step = "foo", depends = character()), character(0))
            expect_equal(f(step = "foo", depends = list()), character(0))
        })

        test_that("dependencies by default are determined recursively",
        {
            depends <- list(
                f1 = c(a = "f0"),
                f2 = c(a = "f1"),
                f3 = c(a = "f2")
            )

            expect_equal(f("f0", depends), character(0))
            expect_equal(f("f1", depends), c("f0"))
            expect_equal(f("f2", depends), c("f1", "f0"))
            expect_equal(f("f3", depends), c("f2", "f1", "f0"))
        })

        test_that("returned dependencies are unique",
        {
            depends <- list(
                f1 = c(a = "f0"),
                f2 = c(a = "f0", b = "f1"),
                f3 = c(a = "f0", b = "f1", c = "f2")
            )

            expect_equal(f("f0", depends), character(0))
            expect_equal(f("f1", depends), c("f0"))
            expect_equal(f("f2", depends), c("f0", "f1"))
            expect_equal(f("f3", depends), c("f0", "f1", "f2"))
        })

        test_that("dependencies can be determined non-recursively",
        {
            depends <- list(
                f1 = c(a = "f0"),
                f2 = c(a = "f1"),
                f3 = c(a = "f2")
            )

            expect_equal(f("f0", depends, recursive = FALSE), character(0))
            expect_equal(f("f1", depends, recursive = FALSE), c("f0"))
            expect_equal(f("f2", depends, recursive = FALSE), c("f1"))
            expect_equal(f("f3", depends, recursive = FALSE), c("f2"))
        })

        test_that("works with multiple dependencies given in sublist",
        {
            depends <- list(
                f1 = c(a = "f0"),
                f2 = c(a = "f1", b = "data"),
                f3 = list(
                    x = c("f1"),
                    y = c("f1", "f2")
                ),
                f4 = list(x = c("f3", "data"))
            )

            expect_equal(f("f2", depends), c("f1", "data", "f0"))
            expect_equal(f("f3", depends), c("f1", "f2", "f0", "data"))
            expect_equal(f("f4", depends), c("f3", "data", "f1", "f2", "f0"))
        })
    })


    test_that(".init_function_and_params",
    {
        pip <- expect_no_error(Pipeline$new("pipe"))
        f <- get_private(pip)$.prepare_and_verify_params

        test_that("fun must be a function",
        {
            expect_error(
                f(fun = 1),
                "is.function(fun) is not TRUE",
                fixed = TRUE
            )
        })

        test_that("funcName must be a string",
        {
            expect_error(
                f(fun = identity, funcName = 1),
                "is_string(funcName) is not TRUE",
                fixed = TRUE
            )
        })

        test_that("params must be a list",
        {
            expect_error(
                f(fun = identity, funcName = "identity", params = 1),
                "is.list(params) is not TRUE",
                fixed = TRUE
            )
        })

        test_that("returns all function args in the parameter list",
        {
            foo <- function(a = 1, b = 2, c = 3) 1
            res <- f(foo, "foo")
            expect_equal(res, list(a = 1, b = 2, c = 3))
        })

        test_that("params given in the list override the function arg",
        {
            foo <- function(a = 1, b = 2, c = 3) 1
            res <- f(foo, "foo", params = list(a = 9, b = 99))
            expect_equal(res, list(a = 9, b = 99, c = 3))
        })

        test_that("signals undefined function args unless they are
            defined in the parameter list",
        {
            foo <- function(a, b = 2, c = 3) 1
            expect_error(
                f(foo, "foo"),
                "'a' parameter(s) must have default values",
                fixed = TRUE
            )

            res <- f(foo, "foo", params = list(a = 9))
            expect_equal(res, list(a = 9, b = 2, c = 3))
        })

        test_that("signals if parameter is not defined in function args,
            unless function is defined with ...",
        {
            foo <- function(a) 1

            expect_error(
                f(foo, "foo", params = list(a = 1, b = 2, c = 3)),
                "'b', 'c' are no function parameters of 'foo'",
                fixed = TRUE
            )

            foo <- function(a, ...) 1
            res <- f(foo, "foo", params = list(a = 1, b = 2, c = 3))
            expect_equal(res, list(a = 1, b = 2, c = 3))
        })
    })



    test_that(".relative_dependency_to_index",
    {
        pip <- expect_no_error(Pipeline$new("pipe"))
        f <- get_private(pip)$.relative_dependency_to_index

        test_that("signals bad input",
        {
            expect_error(
                f(relative_dep = "-1"),
                "is_number(relative_dep) is not TRUE",
                fixed = TRUE
            )
            expect_error(
                f(relative_dep = 1),
                "relative_dep < 0",
                fixed = TRUE
            )
            expect_error(
                f(-1, dependencyName = NULL),
                "is_string(dependencyName) is not TRUE",
                fixed = TRUE
            )
            expect_error(
                f(-1, "dep-name", startIndex = "2"),
                "is_number(startIndex) is not TRUE",
                fixed = TRUE
            )
            expect_error(
                f(-1, "dep-name", startIndex = -2),
                "startIndex > 0 is not TRUE",
                fixed = TRUE
            )
            expect_error(
                f(-1, "dep-name", 2, step = 3),
                "is_string(step) is not TRUE",
                fixed = TRUE
            )
            expect_no_error(
                f(-1, "dep-name", 2, step = "foo"),
            )
        })

        test_that("signals if relative dependency exceeds pipeline",
        {
            expect_error(
                f(
                    relative_dep = -10,
                    dependencyName = "dep-name",
                    startIndex = 1,
                    step = "step-name"
                ),
                paste(
                    "step 'step-name': relative dependency dep-name=-10",
                    "points to outside the pipeline"
                ),
                fixed = TRUE
            )

            expect_error(f(-1, "dep-name", startIndex = 1, "step-name"))
            expect_error(f(-2, "dep-name", startIndex = 2, "step-name"))
        })

        test_that("returns correct index for relative dependency",
        {
            expect_equal(
                f(
                    relative_dep = -1,
                    dependencyName = "dep-name",
                    startIndex = 3,
                    step = "step-name"
                ),
                2
            )
            expect_equal(
                f(
                    relative_dep = -2,
                    dependencyName = "dep-name",
                    startIndex = 3,
                    step = "step-name"
                ),
                1
            )
        })
    })

    test_that(".run_step",
    {
        pip <- expect_no_error(Pipeline$new("pipe"))
        f <- get_private(pip)$.run_step

        test_that("returns the result of the function at the given step",
        {
            pip <- Pipeline$new("pipe") |>
                pipe_add("A", function(a = 1) a) |>
                pipe_add("B", function(b = 2) b)

            f <- get_private(pip)$.run_step

            expect_equal(f(step = "A"), 1)
            expect_equal(f(step = "B"), 2)
        })

        test_that("stores the result at the given step",
        {
            pip <- Pipeline$new("pipe") |>
                pipe_add("A", function(a = 1) a) |>
                pipe_add("B", function(b = 2) b)

            f <- get_private(pip)$.run_step

            expect_equal(f(step = "A"), 1)
            expect_equal(pip$get_out("A"), 1)
            expect_equal(f(step = "B"), 2)
            expect_equal(pip$get_out("B"), 2)
        })

        test_that("uses output of dependent steps",
        {
            pip <- Pipeline$new("pipe") |>
                pipe_add("A", function(a = 1) a) |>
                pipe_add("B", function(b = ~A) b)

            f <- get_private(pip)$.run_step

            expect_equal(f(step = "B"), NULL)

            # Now set output for step 'A'
            i <- match("A", pip$get_step_names())
            pip$pipeline[i, "out"] <- 2

            expect_equal(f(step = "B"), 2)
        })

        test_that("accepts Param object args",
        {
            pip <- Pipeline$new("pipe") |>
                pipe_add(
                    "A",
                    function(a = new("NumericParam", "a", value = 1)) a
                )

            f <- get_private(pip)$.run_step
            expect_no_error(f("A"))
        })

        test_that("if error, the failing step is given in the log",
        {
            lgr::unsuspend_logging()
            on.exit(lgr::suspend_logging())

            pip <- Pipeline$new("pipe") |>
                pipe_add("A", function(a = 1) stop("something went wrong"))

            f <- get_private(pip)$.run_step
            log <- capture.output(expect_error(f("A")))
            hasInfo <- grepl(
                log[1],
                pattern = "something went wrong {\"context\":\"Step 2 ('A')\"}",
                fixed = TRUE
            )
            expect_true(hasInfo)
        })

        test_that(
            "updates the state to 'done' if step was run successfully
            otherwise to 'failed'",
        {
            pip <- Pipeline$new("pipe") |>
                pipe_add("ok", function(x = 1) x) |>
                pipe_add("error", function(x = 2) stop("ups")) |>
                pipe_add("warning", function(x = 3) warning("hm"))

            f <- get_private(pip)$.run_step

            expect_true(all(pip$pipeline[["state"]] == "New"))

            f(step = "ok")
            pip$get_step("ok")$state

            expect_equal(pip$get_step("ok")$state, "Done")

            expect_error(f(step = "error"))
            expect_equal(pip$get_step("error")$state, "Failed")

            expect_warning(f(step = "warning"))
            expect_equal(pip$get_step("warning")$state, "Done")
        })

        test_that("will re-compute if locked step does not keep output",
        {
            pip <- Pipeline$new("pipe") |>
                pipe_add("A", function(a = 1) a) |>
                pipe_add("B", function(b = ~A) b, keepOut = FALSE)

            f <- get_private(pip)$.run_step

            expect_equal(f(step = "A"), 1)
            expect_equal(f(step = "B"), 1)

            pip$set_params_at_step("A", list(a = 2))
            pip$lock_step("B")
            get_private(pip)$.clean_out_not_kept()
            expect_true(is.null(pip$get_out("B")))

            expect_equal(f(step = "A"), 2)
            expect_equal(f(step = "B"), 2)
        })
    })


    test_that(".set_at_step",
    {
        pip <- expect_no_error(Pipeline$new("pipe"))
        f <- get_private(pip)$.set_at_step

        test_that("field must be a string and exist",
        {
            pip <- Pipeline$new("pipe")

            expect_error(
                f("data", field = 1),
                "is_string(field) is not TRUE",
                fixed = TRUE
            )

            expect_error(
                f("data", field = "foo"),
                "field %in% names(self$pipeline) is not TRUE",
                fixed = TRUE
            )
        })

        test_that("sets the value without change data class of the fields",
        {
            pip <- Pipeline$new("pipe") |>
                pipe_add("f1", function(a = 1) a)

            f <- get_private(pip)$.set_at_step
            before <- sapply(pip$pipeline, data.class)

            f("f1", field = "step", value = "f2")
            f("f2", field = "fun", value = mean)
            f("f2", field = "funcName", value = "my fun")
            f("f2", field = "params", value = list(a = 1, b = 2))
            f("f2", field = "out", value = 1)
            f("f2", field = "keepOut", value = TRUE)
            f("f2", field = "group", value = "my group")
            f("f2", field = "description", value = "my description")
            f("f2", field = "time", value = Sys.time())
            f("f2", field = "state", value = "new-state")

            after <- sapply(pip$pipeline, data.class)

            expect_equal(before, after)
        })

        test_that("signals class mismatch unless field is of class list",
        {
            pip <- Pipeline$new("pipe") |>
                pipe_add("f1", function(a = 1) a)

            f <- get_private(pip)$.set_at_step
            ee <- expect_error

            ee(f("f1", field = "step", value = 1))
            ee(f("f2", field = "fun", value = "mean"))
            ee(f("f2", field = "funcName", value = list("my fun")))
            ee(f("f2", field = "params", value = c(a = 1, b = 2)))
            ee(f("f2", field = "keepOut", value = 1))
            ee(f("f2", field = "group", value = list("my group")))
            ee(f("f2", field = "description", value = list("my description")))
            ee(f("f2", field = "time", value = as.character(Sys.time())))
            ee(f("f2", field = "state", value = list("new-state")))
        })
    })


    test_that(".update_states_downstream",
    {
        pip <- expect_no_error(Pipeline$new("pipe"))
        f <- get_private(pip)$.update_states_downstream

        test_that("state must be a string",
        {
            pip <- Pipeline$new("pipe")
            expect_error(
                f("data", state = 1),
                "is_string(state) is not TRUE",
                fixed = TRUE
            )
        })

        test_that("states are updated according to dependencies",
        {
            pip <- Pipeline$new("pipe")
            f <- get_private(pip)$.update_states_downstream

            pip$add("f1", function(a = ~data) a)
            pip$add("f2", function(a = ~f1) a)
            pip$add("f3", function(a = 1) a)
            pip$add("f4", function(a = ~f2) a)

            expect_true(all(pip$pipeline[["state"]] == "New"))
            f("f1", state = "new-state")
            states <- pip$pipeline[["state"]] |>
                stats::setNames(pip$get_step_names())

            expect_equal(
                states,
                c(
                    data = "New",
                    f1 = "New",
                    f2 = "new-state",
                    f3 = "New",
                    f4 = "new-state"
                )
            )

            f("data", state = "another-state")

            states <- pip$pipeline[["state"]] |>
                stats::setNames(pip$get_step_names())

            expect_equal(
                states,
                c(
                    data = "New",
                    f1 = "another-state",
                    f2 = "another-state",
                    f3 = "New",
                    f4 = "another-state"
                )
            )
        })
    })


    test_that(".verify_dependency",
    {
        pip <- expect_no_error(Pipeline$new("pipe"))
        f <- get_private(pip)$.verify_dependency

        test_that("signals badly typed input",
        {
            expect_error(
                f(dep = 1),
                "is_string(dep) is not TRUE",
                fixed = TRUE
            )
            expect_error(
                f(dep = "dep", step = 1),
                "is_string(step) is not TRUE",
                fixed = TRUE
            )
            expect_error(
                f(dep = "dep", step = "step", toStep = 1),
                "is_string(toStep) is not TRUE",
                fixed = TRUE
            )
        })

        test_that("signals non existing toStep",
        {
            expect_error(
                f("dep", "step", toStep = "non-existent"),
                "step 'non-existent' does not exist",
                fixed = TRUE
            )
        })

        test_that("verifies valid depdendencies",
        {
            pip <- expect_no_error(Pipeline$new("pipe"))
            f <- get_private(pip)$.verify_dependency

            pip$add("f1", function(a = 1) a)
            pip$add("f2", function(a = 1) a)

            expect_true(f(dep = "f1", "new-step"))
            expect_true(f(dep = "f2", "new-step"))
        })

        test_that("signals if dependency is not defined",
        {
            pip <- expect_no_error(Pipeline$new("pipe"))
            f <- get_private(pip)$.verify_dependency

            pip$add("f1", function(a = 1) a)
            pip$add("f2", function(a = 1) a)

            expect_error(
                f(dep = "f3", "new-step"),
                "dependency 'f3' not found",
                fixed = TRUE
            )

            expect_error(
                f(dep = "f2", "new-step", toStep = "f1"),
                "dependency 'f2' not found up to step 'f1'",
                fixed = TRUE
            )
        })
    })


    test_that(".verify_from_to",
    {
        pip <- expect_no_error(Pipeline$new("pipe"))
        f <- get_private(pip)$.verify_from_to
        eefix <- function(...) expect_error(..., fixed = TRUE)

        test_that("signals badly typed args",
        {
            pip <- expect_no_error(Pipeline$new("pipe"))
            pip$add("f1", function(a = 1) a)
            f <- get_private(pip)$.verify_from_to

            eefix(
                f(from = "f1", to = 2),
                "is_number(from) is not TRUE"
            )
            eefix(
                f(from = 1, to = "f1"),
                "is_number(to) is not TRUE"
            )
        })

        test_that("signals if from > to",
        {
            eefix(
                f(from = 2, to = 1),
                "from <= to is not TRUE"
            )
        })

        test_that("signals if to > pipeline length",
        {
            pip <- expect_no_error(Pipeline$new("pipe"))
            pip$add("f1", function(a = 1) a)
            f <- get_private(pip)$.verify_from_to

            eefix(
                f(from = 1, to = pip$length() + 1),
                "'to' must not be larger than pipeline length"
            )
        })
    })


    test_that(".verify_fun_params",
    {
        pip <- expect_no_error(Pipeline$new("pipe"))
        f <- get_private(pip)$.verify_fun_params

        test_that("returns TRUE if function has no args",
        {
            fun <- function() 1
            expect_true(f(fun, "funcName"))
        })

        test_that("returns TRUE if args are defined with default values",
        {
            fun <- function(x = 1, y = 2) x + y
            expect_true(f(fun, "funcName"))
        })

        test_that("if not defined with default values, params can be
            passed in addition",
        {
            fun <- function(x, y) x + y
            expect_true(f(fun, "funcName", params = list(x = 1, y = 2)))
        })

        test_that("signals parameters with no default values",
        {
            fun <- function(x, y, z = 1) x + y

            expect_error(
                f(fun, "funcName"),
                "'x', 'y' parameter(s) must have default values",
                fixed = TRUE
            )

            fun <- function(x, y = 1, z) x + y
            expect_error(
                f(fun, "funcName"),
                "'x', 'z' parameter(s) must have default values",
                fixed = TRUE
            )
        })

        test_that("supports ...",
        {
            fun <- function(x, ...) x
            expect_true(f(fun, "funcName", params = list(x = 1)))
        })

        test_that("signals undefined parameters",
        {
            fun <- function(x = 1, y = 1) x + y
            params <- list(x = 1, undef1 = 1, undef2 = 2)
            expect_error(
                f(fun, "funcName", params = params),
                "'undef1', 'undef2' are no function parameters of 'funcName'",
                fixed = TRUE
            )
        })

        test_that(
            "supports additional parameters if function is defined with ...",
        {
            fun <- function(x, ...) x
            params <- list(x = 1, add1 = 1, add2 = 2)
            expect_true(f(fun, "funcName", params = params))
        })


        test_that("signals badly typed input",
        {
            expect_error(
                f("mean"),
                "is.function(fun) is not TRUE",
                fixed = TRUE
            )

            expect_error(
                f(mean, funcName = 1),
                "is_string(funcName) is not TRUE",
                fixed = TRUE
            )

            expect_error(
                f(mean, funcName = "mean", params = 1),
                "is.list(params) is not TRUE",
                fixed = TRUE
            )
        })
    })
})
