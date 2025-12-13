describe("Alias functions",
{
    test_that(
        "alias functions are defined and correct",
    {
        skip_if(Sys.getenv("R_CODECOV_ENV") == "GITHUB_ACTION")

        pip <- Pipeline$new("pipe")
        funs2check <- sapply(names(pip), \(x) is.function(pip[[x]])) |>
            Filter(f = isTRUE) |>
            names() |>
            setdiff("initialize")

        for (fun in funs2check) {
            alias_fun <- paste0("pipe_", fun)
            expect_true(exists(alias_fun), info = fun)

            # Check that the alias body calls the correct method
            alias_body <- body(get(alias_fun))
            body_str <- paste(deparse(alias_body), collapse = "\n")
            expect_true(
                grepl(paste0("pip\\$", fun), body_str),
                info = paste("Alias", alias_fun, "does not call pip$", fun)
            )

            # Verify that all arguments are passed to the member function
            alias_formals <- formals(get(alias_fun))
            nm <- names(alias_formals)
            expected_args <- nm[nm != "pip"] |> setdiff("...")
            for (arg in expected_args) {
                info <- sprintf(
                    "Alias '%s' does not pass arg '%s' correctly to 'pip$%s'",
                    alias_fun, arg, fun
                )
                setsArg <- grepl(
                    pattern = paste0("\\b", arg, "\\s*=\\s*", arg, "\\b"),
                    x = body_str
                )
                expect_true(setsArg, info = info)
            }
        }
    })
})


describe("pipe_new",
{
    test_that("returns a pipeline object",
    {
        expect_true(methods::is(pipe_new("pipe"), "Pipeline"))
    })

    test_that("pipeline name must be a non-empty string",
    {
        expect_no_error(pipe_new("foo"))

        expect_error(
            pipe_new(name = 1),
            "name must be a string"
        )

        expect_error(
            pipe_new(name = ""),
            "name must not be empty"
        )
    })

    test_that("data is added as first step to pipeline",
    {
        pip <- pipe_new("pipe1", data = 1)

        expect_equal(pipe_get_step_names(pip), "data")

        out <- pipe_run(pip) |> pipe_collect_out(all = TRUE)
        expect_equal(pipe_get_out(pip, "data"), 1)
    })

    test_that("the logger can be customized",
    {
        my_logger <- \(level, msg, ...) {
            message("My Logger: ", msg)
        }

        pip <- pipe_new("pipe", logger = my_logger)

        out <- capture.output(
            pipe_run(pip),
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


        logger_with_missing_level_arg <- \(msg, ...) {
            message("My Logger: ", msg)
        }
        expect_error(
            pipe_new("pipe1", logger = logger_with_missing_level_arg),
            expected_error_msg,
            fixed = TRUE
        )


        logger_with_missing_msg_arg <- \(level, ...) {
            message("My Logger: ", ...)
        }
        expect_error(
            pipe_new("pipe1", logger = logger_with_missing_msg_arg),
            expected_error_msg,
            fixed = TRUE
        )


        logger_with_missing_dots <- \(msg, level) {
            message("My Logger: ", msg)
        }
        expect_error(
            pipe_new("pipe1", logger = logger_with_missing_dots),
            expected_error_msg,
            fixed = TRUE
        )


        logger_with_additional_arg <- \(level, msg, foo, ...) {
            message("My Logger: ", msg)
        }
        expect_error(
            pipe_new("pipe1", logger = logger_with_additional_arg),
            expected_error_msg,
            fixed = TRUE
        )
    })
})


describe("pipe_add",
{
    test_that("step must be non-empty string",
    {
        pip <- pipe_new("pipe1")

        foo <- \(a = 0) a

        expect_error(pipe_add(pip, "", foo))
        expect_error(pipe_add(pip, c("a", "b"), foo))
    })

    test_that("fun must be passed as a function or string",
    {
        pip <- pipe_new("pipe")

        expect_error(
            pipe_add(pip, "step1", fun = 1),
            "is.function(fun) || is_string(fun) is not TRUE",
            fixed = TRUE
        )
    })

    test_that("params must be a list",
    {
        pip <- pipe_new("pipe")

        expect_error(
            pipe_add(pip, "step1", fun = \() 1, params = 1),
            "is.list(params)",
            fixed = TRUE
        )
    })

    test_that("description must be (possibly empty) string",
    {
        pip <- pipe_new("pipe")

        expect_error(
            pipe_add(pip, "step1", fun = \() 1, description = 1),
            "is_string(description)",
            fixed = TRUE
        )
        expect_no_error(
            pipe_add(pip, "step1", fun = \() 1, description = "")
        )
    })

    test_that("group must be non-empty string",
    {
        pip <- pipe_new("pipe")

        expect_error(
            pipe_add(pip, "step1", fun = \() 1, group = 1),
            "is_string(group) && nzchar(group) is not TRUE",
            fixed = TRUE
        )
        expect_error(
            pipe_add(pip, "step1", fun = \() 1, group = ""),
            "is_string(group) && nzchar(group) is not TRUE",
            fixed = TRUE
        )
    })

    test_that("keepOut must be logical",
    {
        pip <- pipe_new("pipe")

        expect_error(
            pipe_add(pip, "step1", fun = \() 1, keepOut = 1),
            "is.logical(keepOut) is not TRUE",
            fixed = TRUE
        )
    })

    test_that("duplicated step names are signaled",
    {
        pip <- pipe_new("pipe1")

        foo <- \(a = 0) a

        pipe_add(pip, "f1", foo)
        expect_error(pipe_add(pip, "f1", foo), "step 'f1' already exists")
        expect_error(
            pipe_add(pip, "f1", \(x) x),
            "step 'f1' already exists"
        )
    })

    test_that("missing dependencies are signaled",
    {
        pip <- pipe_new("pipe1")

        foo <- \(a = 0) a

        pipe_add(pip, "f1", foo)
        expect_error(
            pipe_add(pip, "f2", foo, params = list(a = ~undefined)),
            "dependency 'undefined' not found"
        )
    })

    test_that("step can refer to previous step by relative number",
    {
        pip <- pipe_new("pipe1")
        pipe_add(pip, "f1", \(a = 5) a)
        pipe_add(pip, "f2", \(x = ~-1) 2*x, keepOut = TRUE)

        out = pipe_run(pip, ) |> pipe_collect_out()
        expect_equal(out[["f2"]][[1]], 10)

        pipe_add(pip, "f3", \(x = ~-1, a = ~-2) x + a, keepOut = TRUE)
        out = pipe_run(pip) |> pipe_collect_out()
        expect_equal(out[["f3"]][[1]], 10 + 5)
    })

    test_that("a bad relative step referal is signalled",
    {
        pip <- pipe_new("pipe1")
        expect_error(
            pipe_add(pip, "f1", \(x = ~-10) x),
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
        pip <- pipe_new("pipe1", data = data)

        pipe_add(pip, "f1", \(data = ~data) data, keepOut = TRUE)
        a <- 1
        pipe_add(pip, "f2", \(a, b) a + b,
            params = list(a = a, b = ~f1),
            keepOut = TRUE
        )

        expect_equal(
            unlist(pipe_get_step(pip, "f1")[["depends"]]),
            c(data = "data")
        )
        expect_equal(
            unlist(pipe_get_step(pip, "f2")[["depends"]]),
            c(b = "f1")
        )

        out <- pipe_run(pip) |> pipe_collect_out()
        expect_equal(out[["f1"]][[1]], data)
        expect_equal(out[["f2"]][[1]], a + data)
    })


    test_that(
        "supports functions with wildcard arguments",
    {
        my_mean <- \(x, na.rm = FALSE) {
            mean(x, na.rm = na.rm)
        }
        foo <- \(x, ...) {
            my_mean(x, ...)
        }
        v <- c(1, 2, NA, 3, 4)
        pip <- pipe_new("pipe", data = v)

        params <- list(x = ~data, na.rm = TRUE)
        pipe_add(pip, "mean", fun = foo, params = params, keepOut = TRUE)

        out <- pipe_run(pip) |> pipe_collect_out()
        expect_equal(out[["mean"]], mean(v, na.rm = TRUE))

        pipe_set_params_at_step(pip, "mean", list(na.rm = FALSE))
        out <- pipe_run(pip) |> pipe_collect_out()
        expect_equal(out[["mean"]], as.numeric(NA))
    })

    test_that("can have a variable defined outside as parameter default",
    {
        x <- 1

        pip <- pipe_new("pipe") |>
            pipe_add("f1", \(a) a, params = list(a = x))

        expect_equal(pipe_get_params_at_step(pip, "f1")$a, x)

        out <- pipe_run(pip) |> pipe_collect_out(all = TRUE)
        expect_equal(out[["f1"]], x)
    })

    test_that(
        "function can be passed as a string",
    {
        pip <- pipe_new("pipe") |>
            pipe_add("f1", fun = "mean", params = list(x = 1:5))

        out <- pipe_run(pip) |> pipe_collect_out(all = TRUE)
        expect_equal(out[["f1"]], mean(1:5))

        expect_equal(pipe_get_step(pip, "f1")[["funcName"]], "mean")
    })

    test_that(
        "if passed as a function, name is derived from the function",
    {
        pip <- pipe_new("pipe")
        pipe_add(pip, "f1", fun = mean, params = list(x = 1:5))
        expect_equal(pipe_get_step(pip, "f1")[["funcName"]], "mean")

        pip <- pipe_new("pipe") |>
            pipe_add("f1", fun = mean, params = list(x = 1:5))
        expect_equal(pipe_get_step(pip, "f1")[["funcName"]], "mean")
    })

    test_that(
        "lampda functions, are named 'function'",
    {
        pip <- pipe_new("pipe") |> pipe_add("f1", fun = \(x = 1) x)
        expect_equal(pipe_get_step(pip, "f1")[["funcName"]], "function")

        pip <- pipe_new("pipe") |>
            pipe_add("f1", fun = \(x = 1) x)
        expect_equal(pipe_get_step(pip, "f1")[["funcName"]], "function")
    })
})


describe("pipe_append",
{
    test_that("pipelines can be combined even if their steps share names,
        unless tryAutofixNames is FALSE",
    {
        pip1 <- pipe_new("pipe1", data = 1) |>
            pipe_add("f1", \(a = 1) a, keepOut = TRUE) |>
            pipe_add("f2", \(b = ~f1) b)

        pip2 <- pipe_new("pipe2") |>
            pipe_add("f1", \(a = 10) a) |>
            pipe_add("f2", \(b = ~f1) b, keepOut = TRUE)

        expect_error(
            pip1 |> pipe_append(pip2, tryAutofixNames = FALSE),
            paste(
                "combined pipeline would have duplicated step names:",
                "'data', 'f1', 'f2',"
            )
        )

        pp <- pip1 |> pipe_append(pip2)
        expect_equal(
            pp |> pipe_length(),
            pip1 |> pipe_length() + pip2 |> pipe_length()
        )

        out1 <- pip1 |> pipe_run() |> pipe_collect_out()
        out2 <- pip2 |> pipe_run() |> pipe_collect_out()

        out <- pp |> pipe_run() |> pipe_collect_out()

        expect_equivalent(out, c(out1, out2))
    })

    test_that("auto-fixes only the names that need auto-fix",
    {
        pip1 <- pipe_new("pipe1", data = 1) |>
            pipe_add("f1", \(a = 1) a, keepOut = TRUE) |>
            pipe_add("f2", \(b = ~f1) b)

        pip2 <- pipe_new("pipe2") |>
            pipe_add("f3", \(a = 10) a) |>
            pipe_add("f4", \(b = ~f3) b, keepOut = TRUE)

        pp <- pip1 |> pipe_append(pip2)
        expect_equal(
            pp |> pipe_get_step_names(),
            c("data", "f1", "f2", "data.pipe2", "f3", "f4")
        )

        out1 <- pip1 |> pipe_run() |> pipe_collect_out()
        out2 <- pip2 |> pipe_run() |> pipe_collect_out()

        out <- pp |> pipe_run() |> pipe_collect_out()

        expect_equivalent(out, c(out1, out2))
    })

    test_that("the separator used for step names can be customized",
    {
        pip1 <- pipe_new("pipe1", data = 1) |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(b = 2) b)

        pip2 <- pipe_new("pipe2") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(b = 2) b)

        pp <- pip1 |> pipe_append(pip2, sep = "_")

        expect_equal(
            pp |> pipe_get_step_names(),
            c("data", "f1", "f2", "data_pipe2", "f1_pipe2", "f2_pipe2")
        )
    })


    test_that(
        "output of first pipeline can be set as input of appended pipeline",
    {
        pip1 <- pipe_new("pipe1", data = 1)
        pip1 |> pipe_add("f1", \(a = ~data) a * 2)

        pip2 <- pipe_new("pipe2", data = 99) |>
            pipe_add("f1", \(a = ~data) a * 3) |>
            pipe_add("f2", \(a = ~f1) a * 4)

        pp <- pip1 |> pipe_append(pip2, outAsIn = TRUE)

        depends <- pipe_get_depends(pp)
        expect_equal(depends[["data.pipe2"]], c(data = "f1"))

        out <- pp |> pipe_run() |> pipe_collect_out(all = TRUE)
        pipe1_out <- out[["f1"]][["f1"]]
        expect_equal(pipe1_out, 1 * 2)
        expect_equal(out[["data.pipe2"]], pipe1_out)
        expect_equal(out[["f1"]][["f1.pipe2"]], pipe1_out * 3)
        expect_equal(out[["f2"]], out[["f1"]][["f1.pipe2"]] * 4)
    })

    test_that("if duplicated step names would be created, an error is given",
    {
        pip1 <- pipe_new("pipe1")
        pip1 |> pipe_add("f1", \(a = ~data) a + 1)
        pip1 |> pipe_add("f1.pipe2", \(a = ~data) a + 1)

        pip2 <- pipe_new("pipe2")
        pip2 |> pipe_add("f1", \(a = ~data) a + 1)

        expect_error(
            pip1 |> pipe_append(pip2),
            "Cannot auto-fix name clash for step 'f1' in pipeline 'pipe2'",
            fixed = TRUE
        )
    })
})



describe("append_to_step_names",
{
    test_that("postfix can be appended to step names",
    {
        pip <- pipe_new("pipe", data = 1)

        pipe_add(pip, "f1", \(a = ~data) a + 1)
        pipe_add(pip, "f2", \(a = ~data, b = ~f1) a + b)
        pipe_append_to_step_names(pip, "foo")

        expected_names <- c("data.foo", "f1.foo", "f2.foo")
        expect_equal(pipe_get_step_names(pip), expected_names)

        expected_depends <- list(
            data.foo = character(0),
            f1.foo = c(a = "data.foo"),
            f2.foo = c(a = "data.foo", b = "f1.foo")
        )

        expect_equal(pipe_get_depends(pip), expected_depends)
    })
})



describe("pipe_collect_out",
{
    test_that("data is set as first step but not part of output by default",
    {
        dat <- data.frame(a = 1:2, b = 1:2)
        pip <- pipe_new("pipe1", data = dat)
        expect_equal(pip$pipeline[["step"]], "data")

        out <- pipe_run(pip) |> pipe_collect_out()
        expect_equal(out, list())

        pip <- pipe_new("pipe1", data = dat) |>
            pipe_add("f1", \(x = ~data) x, keepOut = TRUE)

        out <- pipe_run(pip) |> pipe_collect_out()
        expect_equal(out[["f1"]], dat)
    })

    test_that("at the end, pipeline can clean output that shall not be kept",
    {
        data <- 9
        pip <- pipe_new("pipe1", data = data)

        foo <-\(a = 1) a
        bar <-\(a, b) a + b

        a <- 5
        pipe_add(pip, "f1", foo, params = list(a = a))
        pipe_add(
            pip, "f2", bar, params = list(a = ~data, b = ~f1), keepOut = TRUE
        )

        pipe_run(pip, cleanUnkept = TRUE)
        expect_equal(pipe_get_out(pip, "f1"), NULL)
        expect_equal(pipe_get_out(pip, "f2"), a + data)
    })

    test_that("output is collected as expected",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a, keepOut = TRUE) |>
            pipe_add("f2", \(a = 2, b = ~f1) a + b) |>
            pipe_add("f3", \(a = 3, b = ~f2) a + b, keepOut = TRUE)

        out <- pipe_run(pip) |> pipe_collect_out()
        expect_equal(length(out), 2)
        expect_equal(names(out), c("f1", "f3"))
    })

    test_that("output can be grouped",
    {
        pip <- pipe_new("pipe") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(a = 1, b = 2) a + b, group = "plus") |>
            pipe_add("f3", \(a = 1, b = 2) a / b) |>
            pipe_add("f4", \(a = 2, b = 2) a + b, group = "plus")

        out <- pipe_run(pip) |> pipe_collect_out(all = TRUE)

        expect_equal(out[["plus"]], list(f2 = 3, f4 = 4))
        expect_equal(out[["f1"]], 1)
        expect_equal(out[["f3"]], 1/2)
    })

    test_that("output is ordered in the order of steps",
    {
        pip <- pipe_new("pipe") |>
            pipe_add("f2", \(a = 1) a, keepOut = TRUE) |>
            pipe_add("f1", \(b = 2) b, keepOut = TRUE)

        out <- pipe_run(pip) |> pipe_collect_out()
        expect_equal(names(out), c("f2", "f1"))
    })

    test_that(
        "grouped output is ordered in the order of group definitions",
    {
        pip <- pipe_new("pipe") |>
            pipe_add("f1", \(x = 1) x, group = "g2") |>
            pipe_add("f2", \(x = 2) x, group = "g1") |>
            pipe_add("f3", \(x = 3) x, group = "g2") |>
            pipe_add("f4", \(x = 4) x, group = "g1")

        out <- pipe_run(pip) |> pipe_collect_out(all = TRUE)

        expect_equal(names(out), c("data", "g2", "g1"))
    })

    test_that(
        "if just one group the output name still will take the group name",
    {
        pip <- pipe_new("pipe") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(a = 1, b = 2) a + b, group = "plus") |>
            pipe_add("f3", \(a = 1, b = 2) a / b, group = "my f3") |>
            pipe_add("f4", \(a = 2, b = 2) a + b, group = "plus")

        out <- pipe_run(pip) |> pipe_collect_out(all = TRUE)

        expect_equal(names(out), c("data", "f1", "plus", "my f3"))
    })

    describe("groupBy option",
    {
        pip <- pipe_new("pipe") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(a = 1, b = 2) a + b, group = "plus") |>
            pipe_add("f3", \(a = 1, b = 2) a / b)

        test_that("column to groupBy can be customized",
        {
            pipe_run_step(pip, "f1")
            pipe_run_step(pip, "f3")
            out <- pipe_collect_out(pip, groupBy = "state", all = TRUE)

            expect_equal(
                out,
                list(
                    New = list(data = NULL, f2 = NULL),
                    Done = list(f1 = 1, f3 = 0.5)
                )
            )
        })

        test_that("signals bad  groupBy input",
        {
            expect_error(
                pipe_collect_out(pip, groupBy = c("not", "a", "string")),
                "groupBy must be a single string"
            )

            expect_error(
                pipe_collect_out(pip, groupBy = "foo"),
                "groupBy column does not exist"
            )

            expect_error(
                pipe_collect_out(pip, groupBy = "time"),
                "groupBy column must be character"
            )
        })
    })
})



describe("pipe_discard_steps",
{
    test_that("pipeline steps can be discarded by pattern",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("calc", \(a = 1) a) |>
            pipe_add("plot1", \(x = ~calc) x) |>
            pipe_add("plot2", \(x = ~plot1) x)

        out <- capture.output(
            pipe_discard_steps(pip, "plot"),
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
        pip <- pipe_new("pipe1") |>
            pipe_add("calc", \(a = 1) a) |>
            pipe_add("plot1", \(x = ~calc) x) |>
            pipe_add("plot2", \(x = ~plot1) x)

        steps_before = pip$pipeline[["step"]]

        expect_silent(pipe_discard_steps(pip, "bla"))
        expect_equal(pip$pipeline[["step"]], steps_before)
    })


    test_that("if step has downstream dependencies, an error is given",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(b = ~f1) b)

        expect_error(
            pipe_discard_steps(pip, "f1"),
            paste(
                "cannot remove step 'f1' because the following",
                "steps depend on it: 'f2'"
            )
        )

        pipe_add(pip, "f3", \(x = ~f1) x)
        expect_error(
            pipe_discard_steps(pip, "f1"),
            paste(
                "cannot remove step 'f1' because the following",
                "steps depend on it: 'f2', 'f3'"
            )
        )
    })
})


describe("pipe_get_data",
{
    test_that(
        "data can be retrieved",
    {
        p <- pipe_new("pipe", data = 1:2)
        expect_equal(p |> pipe_get_data(), 1:2)

        p |> pipe_set_data(3:4)
        expect_equal(p |> pipe_get_data(), 3:4)
    })

    test_that(
        "signals missing data",
    {
        p <- pipe_new("pipe", data = 1:2)
        p |> pipe_pop_step()    # remove data step

        expect_error(
            p |> pipe_get_data(),
            "no data step defined"
        )
    })
})


describe("pipe_get_depends",
{
    test_that(
        "dependencies can be retrieved and are named after the steps",
    {
        pip <- pipe_new("pipe", data = 1)

        pipe_add(pip, "f1", \(a = ~data) a + 1)
        pipe_add(pip, "f2", \(b = ~f1) b + 1)

        depends <- pipe_get_depends(pip)
        expected_depends <- list(
            data = character(0),
            f1 = c(a = "data"),
            f2 = c(b = "f1")
        )

        expect_equal(depends, expected_depends)
        expect_equal(names(depends), pipe_get_step_names(pip))
    })
})



describe("pipe_get_depends_down",
{
    test_that("dependencies can be determined recursively for given step",
    {
        pip <- pipe_new("pipe")

        pipe_add(pip, "f1", \(a = 1) a)
        pipe_add(pip, "f2", \(a = ~f1) a)
        pipe_add(pip, "f3", \(a = ~f1, b = ~f2) a + b)
        pipe_add(pip, "f4", \(a = ~f1, b = ~f2, c = ~f3) a + b + c)

        expect_equal(pipe_get_depends_down(pip, "f3"), c("f4"))
        expect_equal(pipe_get_depends_down(pip, "f2"), c("f3", "f4"))
        expect_equal(pipe_get_depends_down(pip, "f1"), c("f2", "f3", "f4"))
    })

    test_that("if no dependencies an empty character vector is returned",
    {
        pip <- pipe_new("pipe")

        pipe_add(pip, "f1", \(a = 1) a)
        expect_equal(pipe_get_depends_down(pip, "f1"), character(0))
    })

    test_that("step must exist",
    {
        pip <- pipe_new("pipe")

        expect_error(
            pipe_get_depends_down(pip, "f1"),
            "step 'f1' does not exist"
        )
    })

    test_that(
        "works with complex dependencies as created by data splits",
    {
        dat1 <- data.frame(x = 1:2)
        dat2 <- data.frame(y = 1:2)
        dataList <- list(dat1, dat2)

        pip <- pipe_new("pipe") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(a = ~f1, b = 2) b) |>
            pipe_add("f3", \(x = ~f1, y = ~f2) x + y)

        pipe_set_data_split(pip, dataList, toStep = "f2")

        expect_equal(
            pipe_get_depends_down(pip, "f1.1"),
            c("f2.1", "f3")
        )

        expect_equal(
            pipe_get_depends_down(pip, "f1.2"),
            c("f2.2", "f3")
        )

        expect_equal(pipe_get_depends_down(pip, "f2.1"), "f3")
        expect_equal(pipe_get_depends_down(pip, "f2.2"), "f3")
    })
})



describe("pipe_get_depends_up",
{
    test_that("dependencies can be determined recursively for given step",
    {
        pip <- pipe_new("pipe")

        pipe_add(pip, "f1", \(a = 1) a)
        pipe_add(pip, "f2", \(a = ~f1) a)
        pipe_add(pip, "f3", \(a = ~f1, b = ~f2) a + b)
        pipe_add(pip, "f4", \(a = ~f1, b = ~f2, c = ~f3) a + b + c)

        expect_equal(pipe_get_depends_up(pip, "f2"), c("f1"))
        expect_equal(pipe_get_depends_up(pip, "f3"), c("f1", "f2"))
        expect_equal(pipe_get_depends_up(pip, "f4"), c("f1", "f2", "f3"))
    })

    test_that("if no dependencies an empty character vector is returned",
    {
        pip <- pipe_new("pipe")

        pipe_add(pip, "f1", \(a = 1) a)
        expect_equal(pipe_get_depends_up(pip, "f1"), character(0))
    })

    test_that("step must exist",
    {
        pip <- pipe_new("pipe")

        expect_error(
            pipe_get_depends_up(pip, "f1"),
            "step 'f1' does not exist"
        )
    })

    test_that("works with complex dependencies as created by data splits",
    {
        dat1 <- data.frame(x = 1:2)
        dat2 <- data.frame(y = 1:2)
        dataList <- list(dat1, dat2)

        pip <- pipe_new("pipe") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(a = ~f1, b = 2) b) |>
            pipe_add("f3", \(x = ~f1, y = ~f2) x + y)

        pipe_set_data_split(pip, dataList, toStep = "f2")

        expect_equal(pipe_get_depends_up(pip, "f2.1"), c("f1.1"))
        expect_equal(pipe_get_depends_up(pip, "f2.2"), c("f1.2"))

        expect_equivalent(
            pipe_get_depends_up(pip, "f3"),
            c("f1.1", "f2.1", "f1.2", "f2.2")
        )
    })
})


describe("pipe_get_graph",
{
    pip <- pipe_new("pipe") |>
        pipe_add("f1", \(a = 1) a) |>
        pipe_add("f2", \(a = ~f1, b = ~data) a)

    res <- pipe_get_graph(pip)

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
        expect_equal(tab$label, pipe_get_step_names(pip))
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
        pip <- pipe_new("pipe")
        pipe_add(pip, "step2", \(a = ~data) a + 1, group = "add")
        pipe_add(pip, "step3", \(a = ~step2) 2 * a, group = "mult")
        pipe_add(pip, "step4", \(a = ~step2, b = ~step3) a + b, group = "add")
        pipe_add(pip, "step5", \(a = ~data, b = ~step4) a * b, group = "mult")

        res.add <- pipe_get_graph(pip, groups = "add")
        expect_equal(res.add$nodes$label, c("step2", "step4"))

        res.mult <- pipe_get_graph(pip, groups = "mult")
        expect_equal(res.mult$nodes$label, c("step3", "step5"))
    })
})


describe("pipe_get_out",
{
    test_that("output at given step can be retrieved",
    {
        data <- airquality
        pip <- pipe_new("pipe", data = data) |>
            pipe_add("model",
               \(data = ~data) {
                    lm(Ozone ~ Wind, data = data)
                },
            )

        pipe_run(pip)

        expect_equal(pipe_get_out(pip, "data"), data)
        expect_equivalent(
            pipe_get_out(pip, "model"),
            lm(Ozone ~ Wind, data = data)
        )
    })

    test_that("step of requested output must exist",
    {
        pip <- pipe_new("pipe")
        pipe_run(pip)
        expect_error(pipe_get_out(pip, "foo"), "step 'foo' does not exist")
    })
})



describe("pipe_get_params",
{
    test_that("parameters can be retrieved",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a, keepOut = TRUE) |>
            pipe_add("f2", \(a, b = ~f1) a + b,
                params = list(a = 8),
                keepOut = TRUE
            ) |>
            pipe_add("f3", \(a = ~f2, b = 3) a + b, keepOut = TRUE)

        p <- pipe_get_params(pip)
        expect_equal(
            p, list(f1 = list(a = 1), f2 = list(a = 8), f3 = list(b = 3))
        )
    })


    test_that("empty pipeline gives empty list of parameters",
    {
        pip <- pipe_new("pipe1")
        expect_equivalent(pipe_get_params(pip), list())

        pipe_add(pip, "f1", \() 1)
        expect_equivalent(pipe_get_params(pip), list())
    })

    test_that("hidden parameters are filtered out by default",
    {

        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1, .hidden = 2) a)

        p <- pipe_get_params(pip)
        expect_equal(p, list(f1 = list(a = 1)))

        p <- pipe_get_params(pip, ignoreHidden = FALSE)
        expect_equal(p, list(f1 = list(a = 1, .hidden = 2)))
    })
})



describe("pipe_get_params_at_step",
{
    test_that("list of step parameters can be retrieved",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1, b = 2) a + b) |>
            pipe_add("f2", \(x = 1, y = 2) x + y)

        expect_equal(
            pipe_get_params_at_step(pip, "f1"), list(a = 1, b = 2)
        )

        expect_equal(
            pipe_get_params_at_step(pip, "f2"), list(x = 1, y = 2)
        )
    })

    test_that("if no parameters empty list is returned",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \() 1)

        expect_equal(
            pipe_get_params_at_step(pip, "f1"), list()
        )
    })

    test_that(
        "hidden parameters are not returned, unless explicitly requested",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1, .b = 2) a + .b)

        expect_equal(
            pipe_get_params_at_step(pip, "f1"), list(a = 1)
        )

        expect_equal(
            pipe_get_params_at_step(pip, "f1", ignoreHidden = FALSE),
            list(a = 1, .b = 2)
        )
    })

    test_that("bound parameters are never returned",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1, b = ~data) a + b)

        expect_equal(
            pipe_get_params_at_step(pip, "f1"), list(a = 1)
        )

        expect_equal(
            pipe_get_params_at_step(pip, "f1", ignoreHidden = FALSE),
            list(a = 1)
        )
    })

    test_that("step must exist",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1, b = ~data) a + b)

        expect_error(
            pipe_get_params_at_step(pip, "foo"),
            "step 'foo' does not exist"
        )
    })
})


describe("pipe_get_params_unique",
{
    test_that("parameters can be retrieved uniquely and if occuring multiple
        times, the 1st default value is used",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(a = 2, b = 3) a + b) |>
            pipe_add("f3", \(a = 4, b = 5, c = 6) a + b)

        p <- pipe_get_params_unique(pip)
        expect_equivalent(p, list(a = 1, b = 3, c = 6))
    })

    test_that("empty pipeline gives empty list",
    {
        pip <- pipe_new("pipe")
        expect_equivalent(pipe_get_params_unique(pip), list())
    })

    test_that("pipeline with no parameters gives empty list",
    {
        pip <- pipe_new("pipe") |>
            pipe_add("f1", \() 1)
        expect_equivalent(pipe_get_params_unique(pip), list())
    })
})



describe("pipe_get_step",
{
    test_that("single steps can be retrieved",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", identity, params = list(x = 1))

        expect_equal(pipe_get_step(pip, "data"), pip$pipeline[1, ])
        expect_equal(pipe_get_step(pip, "f1"), pip$pipeline[2, ])

        expect_error(pipe_get_step(pip, "foo"), "step 'foo' does not exist")
    })

    test_that("dependencies are recorded as expected",
    {
        pip <- pipe_new("pipe1", data = 9)

        foo <-\(a = 0) a
        bar <-\(a = 1, b = 2) a + b

        pipe_add(pip, "f1", foo)
        pipe_add(pip, "f2", bar, params = list(a = ~data, b = ~f1))

        expect_true(length(unlist(pipe_get_step(pip, "f1")[["depends"]])) == 0)
        expect_equal(
            unlist(pipe_get_step(pip, "f2")[["depends"]]),
            c("a" = "data", b = "f1")
        )
    })
})


describe("pipe_get_step_names",
{
    test_that("step names be retrieved",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \() {}) |>
            pipe_add("f2", \(x = 1) x)

        expect_equal(pipe_get_step_names(pip), pip$pipeline[["step"]])
    })
})



describe("pipe_get_step_number",
{
    test_that("get_step_number works as expected",
    {
        pip <- expect_no_error(pipe_new("pipe"))
        pipe_add(pip, "f1", \(a = 1) a)
        pipe_add(pip, "f2", \(a = 1) a)

        pipe_get_step_number(pip, "f1") |> expect_equal(2)
        pipe_get_step_number(pip, "f2") |> expect_equal(3)
    })

    test_that("signals non-existent step",
    {
        pip <- expect_no_error(pipe_new("pipe"))
        pipe_add(pip, "f1", \(a = 1) a)

        expect_error(
            pipe_get_step_number(pip, "non-existent"),
            "step 'non-existent' does not exist"
        )
    })
})


describe("pipe_has_step",
{
    test_that("it can be checked if pipeline has a step",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a)

        expect_true(pipe_has_step(pip, "f1"))
        expect_false(pipe_has_step(pip, "f2"))
    })
})


describe("pipe_insert_after",
{
    test_that("can insert a step after another step",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a + 1) |>
            pipe_add("f2", \(a = ~f1) a + 1)

        pipe_insert_after(
            pip,
            "f1",
            step = "f3",
            fun =\(a = ~f1) a + 1
        )

        expect_equal(
            pipe_get_step_names(pip),
            c("data", "f1", "f3", "f2")
        )
    })

    test_that("will not insert a step if the step already exists",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a + 1) |>
            pipe_add("f2", \(a = ~f1) a + 1)

        expect_error(
            pipe_insert_after(pip, "f1", step = "f2"),
            "step 'f2' already exists"
        )
    })

    test_that(
        "will not insert a step if the reference step does not exist",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a + 1) |>
            pipe_add("f2", \(a = ~f1) a + 1)

        expect_error(
            pipe_insert_after(pip, "non-existent", step = "f3"),
            "step 'non-existent' does not exist"
        )
    })

    test_that(
        "will not insert a step with bad parameter dependencies",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a + 1) |>
            pipe_add("f2", \(a = ~f1) a + 1)

        expect_error(
            pipe_insert_after(pip, "f1", step = "f3", \(x = ~f2) x),
            "step 'f3': dependency 'f2' not found"
        )
    })

    test_that("will work if insert happens at last position",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a + 1) |>
            pipe_add("f2", \(a = ~f1) a + 1)

        pipe_insert_after(
            pip,
            "f2",
            step = "f3",
            fun =\(a = ~f1) a + 1
        )

        expect_equal(
            pipe_get_step_names(pip),
            c("data", "f1", "f2", "f3")
        )
    })
})


describe("pipe_insert_before",
{
    test_that("can insert a step after another step",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a + 1) |>
            pipe_add("f2", \(a = ~f1) a + 1)

        pipe_insert_before(
            pip,
            "f2",
            step = "f3",
            fun =\(a = ~f1) a + 1
        )

        expect_equal(
            pipe_get_step_names(pip),
            c("data", "f1", "f3", "f2")
        )
    })

    test_that("will not insert a step if the step already exists",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a + 1) |>
            pipe_add("f2", \(a = ~f1) a + 1)

        expect_error(
            pipe_insert_before(pip, "f1", step = "f2"),
            "step 'f2' already exists"
        )
    })

    test_that(
        "will not allow step to be inserted at first position",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a + 1)

        expect_error(
            pipe_insert_before(pip, "data", step = "f2"),
            "cannot insert before first step"
        )
    })

    test_that("will not insert a step if the reference step does not exist",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a + 1) |>
            pipe_add("f2", \(a = ~f1) a + 1)

        expect_error(
            pipe_insert_before(pip, "non-existent", step = "f3"),
            "step 'non-existent' does not exist"
        )
    })

    test_that(
        "will not insert a step with bad parameter dependencies",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a + 1) |>
            pipe_add("f2", \(a = ~f1) a + 1)

        expect_error(
            pipe_insert_before(pip, "f2", step = "f3", \(x = ~f2) x),
            "step 'f3': dependency 'f2' not found"
        )
    })
})


describe("pipe_length",
{
    test_that("returns the number of steps",
    {
        pip <- pipe_new("pipe")
        expect_equal(pipe_length(pip), 1)

        pipe_add(pip, "f1", \(a = 1) a)
        expect_equal(pipe_length(pip), 2)

        pipe_remove_step(pip, "f1")
        expect_equal(pipe_length(pip), 1)
    })
})


describe("pipe_lock_step",
{
    test_that("sets 'locked' flag of step to TRUE",
    {
        pip <- pipe_new("pipe") |>
            pipe_add("f1", \(a = 1) a)

        pipe_lock_step(pip, "f1")
        expect_true(pipe_get_step(pip, "f1")[["locked"]])

        pip
    })
})


describe("pipe_print",
{
    test_that("pipeline can be printed",
    {
        pip <- pipe_new("pipe1", data = 9)

        expect_output(pipe_print(pip))
    })

    test_that("missing function is signaled",
    {
        pip <- pipe_new("pipe1")

        expect_error(
            pipe_add(pip, "f1", "non-existing-function"),
            "object 'non-existing-function' of mode 'function' was not found"
        )
    })

    test_that("if verbose is TRUE, all columns are printed",
    {
        op <- options(width = 1000L)
        on.exit(options(op))
        pip <- pipe_new("pipe1", data = 9)

        out <- capture.output(pipe_print(pip, verbose = TRUE))
        header <- out[1] |> trimws() |> strsplit("\\s+") |> unlist()
        expected_header <- colnames(pip$pipeline)
        expect_equal(header, expected_header)
    })
})


describe("pipe_pop_step",
{
    test_that("last pipeline step can be popped",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a)

        pip_copy = pipe_clone(pip)

        pipe_add(pip, "f2", \(b = 2) b)

        expect_equal(pipe_length(pip), 3)
        expect_equal(pip_copy$length(), 2)

        res = pipe_pop_step(pip)
        expect_equal(res, "f2")

        expect_equal(pipe_length(pip), 2)
        expect_equal(pip, pip_copy)
    })
})



describe("pipe_pop_steps_after",
{
    test_that("all steps after a given step can be removed",
    {

        pip <- pipe_new("pipe1", data = 0) |>
            pipe_add("f1", \(x = 1) x) |>
            pipe_add("f2", \(x = ~f1) x) |>
            pipe_add("f3", \(x = ~f2) x)

        steps = pipe_pop_steps_after(pip, "f1")
        expect_equal(steps, c("f2", "f3"))

        hasAllStepsRemoved = !any(steps %in% pip$pipeline[["name"]])
        expect_true(hasAllStepsRemoved)
    })

    test_that("if given step does not exist, an error is signalled",
    {

        pip <- pipe_new("pipe1", data = 0) |>
            pipe_add("f1", \(x = 1) x)

        expect_error(
            pipe_pop_steps_after(pip, "bad_step"),
            "step 'bad_step' does not exist"
        )
    })

    test_that("if given step is the last step, nothing gets removed",
    {

        pip <- pipe_new("pipe1", data = 0) |>
            pipe_add("f1", \(x = 1) x) |>
            pipe_add("f2", \(x = ~f1) x)

        length_before = pipe_length(pip)

        res = pipe_pop_steps_after(pip, "f2")

        expect_equal(res, character(0))
        hasNothingRemoved = pipe_length(pip) == length_before
        expect_true(hasNothingRemoved)
    })
})



describe("pipe_pop_steps_from",
{
    test_that("all steps from a given step can be removed",
    {

        pip <- pipe_new("pipe1", data = 0) |>
            pipe_add("f1", \(x = 1) x) |>
            pipe_add("f2", \(x = ~f1) x) |>
            pipe_add("f3", \(x = ~f2) x)

        steps = pipe_pop_steps_from(pip, "f2")
        expect_equal(steps, c("f2", "f3"))

        hasAllStepsRemoved = !any(steps %in% pip$pipeline[["name"]])
        expect_true(hasAllStepsRemoved)
    })

    test_that("if given step does not exist, an error is signalled",
    {

        pip <- pipe_new("pipe1", data = 0) |>
            pipe_add("f1", \(x = 1) x)

        expect_error(
            pipe_pop_steps_from(pip, "bad_step"),
            "step 'bad_step' does not exist"
        )
    })

    test_that("if given step is the last step, one step removed",
    {

        pip <- pipe_new("pipe1", data = 0) |>
            pipe_add("f1", \(x = 1) x) |>
            pipe_add("f2", \(x = ~f1) x)

        length_before = pipe_length(pip)

        res = pipe_pop_steps_from(pip, "f2")

        expect_equal(res, "f2")
        hasOneStepRemoved = pipe_length(pip) == length_before - 1
        expect_true(hasOneStepRemoved)
    })
})



describe("pipe_remove_step",
{
    test_that("pipeline step can be removed",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(b = 1) b)

        pipe_remove_step(pip, "f1")

        expect_equal(pipe_get_step_names(pip), c("data", "f2"))
    })

    test_that("step must exist",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a)

        expect_error(
            pipe_remove_step(pip, "non-existent-step"),
            "step 'non-existent-step' does not exist"
        )
    })

    test_that("if step has downstream dependencies, an error is given",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(b = ~f1) b)

        expect_error(
            pipe_remove_step(pip, "f1"),
            paste(
                "cannot remove step 'f1' because the following",
                "steps depend on it: 'f2'"
            )
        )

        pipe_add(pip, "f3", \(x = ~f1) x)
        expect_error(
            pipe_remove_step(pip, "f1"),
            paste(
                "cannot remove step 'f1' because the following",
                "steps depend on it: 'f2', 'f3'"
            )
        )
    })

    test_that(
        "if error, only the direct downstream dependencies are reported",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(b = ~f1) b) |>
            pipe_add("f3", \(c = ~f2) c) |>
            pipe_add("f4", \(d = ~f1) d)

        expect_error(
            pipe_remove_step(pip, "f1"),
            paste(
                "cannot remove step 'f1' because the following",
                "steps depend on it: 'f2', 'f4'"
            )
        )
    })

    test_that(
        "step can be removed together with is downstream dependencies",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(b = ~f1) b) |>
            pipe_add("f3", \(c = ~f2) c) |>
            pipe_add("f4", \(d = ~f1) d) |>
            pipe_add("f5", \(x = ~data) x)

        out <- utils::capture.output(
            pipe_remove_step(pip, "f1", recursive = TRUE),
            type = "message"
        )

        expect_equal(pipe_get_step_names(pip), c("data", "f5"))
        expect_equal(
            out,
            paste(
                "Removing step 'f1' and its downstream dependencies:",
                "'f2', 'f3', 'f4'"
            )
        )
    })
})


describe("pipe_remove_step",
{
    test_that("pipeline step can be renamed",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(b = 2) b)

        pipe_rename_step(pip, from = "f1", to = "first")

        pipe_get_step_names(pip) |> expect_equal(c("data", "first", "f2"))
    })

    test_that("signals name clash",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(b = 2) b)

        expect_error(
            pipe_rename_step(pip, from = "f1", to = "f2"),
            "step 'f2' already exists"
        )
    })

    test_that("renames dependencies as well",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(b = ~f1) b) |>
            pipe_add("f3", \(a = ~f1, b = ~f2) a + b)

        pipe_rename_step(pip, from = "f1", to = "first")

        expect_equal(
            pipe_get_depends(pip),
            list(
                data = character(0),
                first = character(0),
                f2 = c(b = "first"),
                f3 = c(a = "first", b = "f2")
            )
        )
    })
})


describe("pipe_replace_step",
{
    test_that("pipeline steps can be replaced",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(b = 2) b) |>
            pipe_add("f3", \(c = ~f2) c, keepOut = TRUE)

        out = unname(unlist(pipe_run(pip) |> pipe_collect_out()))
        expect_equal(out, 2)

        pipe_replace_step(pip, "f2", \(z = 4) z)
        out = unname(unlist(pipe_run(pip) |> pipe_collect_out()))
        expect_equal(out, 4)
    })

    test_that("fun must be passed as a function or string",
    {
        pip <- pipe_new("pipe") |>
            pipe_add("step1", \(a = 1) a)

        expect_error(
            pipe_replace_step(pip, "step1", fun = 1),
            "is.function(fun) || is_string(fun) is not TRUE",
            fixed = TRUE
        )
    })

    test_that("params must be a list",
    {
        pip <- pipe_new("pipe") |>
            pipe_add("step1", \(a = 1) a)

        expect_error(
            pipe_replace_step(pip, "step1", fun =\() 1, params = 1),
            "is.list(params)",
            fixed = TRUE
        )
    })

    test_that("description must be (possibly empty) string",
    {
        pip <- pipe_new("pipe") |>
            pipe_add("step1", \(a = 1) a)

        expect_error(
            pipe_replace_step(
                pip, "step1", fun =\() 1, description = 1
            ),
            "is_string(description)",
            fixed = TRUE
        )
        expect_no_error(
            pipe_replace_step(
                pip, "step1", fun =\() 1, description = ""
            )
        )
    })

    test_that("group must be non-empty string",
    {
        pip <- pipe_new("pipe") |>
            pipe_add("step1", \(a = 1) a)

        expect_error(
            pipe_replace_step(pip, "step1", fun =\() 1, group = 1),
            "is_string(group) && nzchar(group) is not TRUE",
            fixed = TRUE
        )
        expect_error(
            pipe_replace_step(pip, "step1", fun =\() 1, group = ""),
            "is_string(group) && nzchar(group) is not TRUE",
            fixed = TRUE
        )
    })

    test_that("keepOut must be logical",
    {
        pip <- pipe_new("pipe") |>
            pipe_add("step1", \(a = 1) a)

        expect_error(
            pipe_replace_step(pip, "step1", fun =\() 1, keepOut = 1),
            "is.logical(keepOut) is not TRUE",
            fixed = TRUE
        )
    })

    test_that("the replacing function can be passed as a string",
    {

        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(x = 3) x, keepOut = TRUE)

        out = unname(unlist(pipe_run(pip) |> pipe_collect_out()))
        expect_equal(out, 3)

        .my_func <-\(x = 3) {
            2 * x
        }
        assign(".my_func", .my_func, envir = globalenv())
        on.exit(rm(".my_func", envir = globalenv()))

        pipe_replace_step(pip, "f1", fun = ".my_func", keepOut = TRUE)

        out = unname(unlist(pipe_run(pip) |> pipe_collect_out()))
        expect_equal(out, 6)
    })

    test_that(
        "when replacing function, default parameters can be overridden",
    {

        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(x = 1:3) x, keepOut = TRUE)

        .my_func <-\(x = 3) {
            2 * x
        }
        assign(".my_func", .my_func, envir = globalenv())
        on.exit(rm(".my_func", envir = globalenv()))

        pipe_replace_step(
            pip,
            "f1",
            fun = ".my_func",
            params = list(x = 10),
            keepOut = TRUE
        )

        out = unname(unlist(pipe_run(pip) |> pipe_collect_out()))
        expect_equal(out, 20)
    })

    test_that("the pipeline step that is being replaced must exist",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(b = 2) b) |>
            pipe_add("f3", \(c = ~f2) c, keepOut = TRUE)

        expect_error(pipe_replace_step(pip, "non-existent", \(z = 4) z))
    })


    test_that(
        "if replacing a pipeline step, dependencies are verified correctly",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(b = 2) b) |>
            pipe_add("f3", \(c = ~f2) c, keepOut = TRUE)

        expect_error(
            pipe_replace_step(pip, "f2", \(z = ~foo) z),
            "dependency 'foo' not found up to step 'f1'"
        )

        expect_error(
            pipe_replace_step(pip, "f2", \(z = ~f2) z),
            "dependency 'f2' not found up to step 'f1'"
        )

        expect_error(
            pipe_replace_step(pip, "f2", \(z = ~f3) z),
            "dependency 'f3' not found up to step 'f1'"
        )
    })

    test_that(
        "states are updated correctly",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(a = 2) a) |>
            pipe_add("f3", \(a = ~f2) a, keepOut = TRUE) |>
            pipe_add("f4", \(a = ~f3) a, keepOut = TRUE)

        pipe_run(pip)

        pipe_replace_step(pip, "f2", \(a = 2) 2* a)
        expect_equal(pipe_get_step(pip, "f1")$state, "Done")
        expect_equal(pipe_get_step(pip, "f2")$state, "New")
        expect_equal(pipe_get_step(pip, "f3")$state, "Outdated")
        expect_equal(pipe_get_step(pip, "f4")$state, "Outdated")
    })


    it("can have a variable defined outside as parameter default",
    {
        x <- 3
        pip <- pipe_new("pipe") |> pipe_add("f1", \(x = 1) x)
        pip |> pipe_replace_step("f1", fun = \(a) a, params = list(a = x))

        out <- pipe_run(pip) |> pipe_get_out("f1")
        expect_equal(out, x)
    })


    it("function can be passed as a string",
    {
        pip <- pipe_new("pipe") |> pipe_add("f1", \(x = 1) x)
        pip |> pipe_replace_step("f1", fun = "mean", params = list(x = 1:5))

        out <- pipe_run(pip) |> pipe_get_out("f1")
        expect_equal(out, mean(1:5))
        expect_equal(pipe_get_step(pip, "f1")[["funcName"]], "mean")
    })

    it("if passed as a function, name is derived from the function",
    {
        pip <- pipe_new("pipe") |> pipe_add("f1", \(x = 1) x)
        pip |> pipe_replace_step("f1", fun = mean, params = list(x = 1:5))

        out <- pipe_run(pip) |> pipe_get_out("f1")
        expect_equal(out, mean(1:5))
        expect_equal(pipe_get_step(pip, "f1")[["funcName"]], "mean")
    })

    it("lampda functions, are named 'function'",
    {
        pip <- pipe_new("pipe") |> pipe_add("f1", \(x = 1) x)
        pip |> pipe_replace_step("f1", fun = \(x = 1) x)
        expect_equal(pipe_get_step(pip, "f1")[["funcName"]], "function")
    })
})


describe("pipe_reset",
{
    test_that(
        "after reset pipeline is the same as before the run",
    {
        p <- pipe_new("pipe", data = 1:2)
        p |> pipe_add("f1", \(x = 1) x)
        p |> pipe_add("f2", \(y = 1) y)

        p |> pipe_run()
        expect_equal(
            p |> pipe_collect_out(all = TRUE),
            list(data = 1:2, f1 = 1, f2 = 1)
        )
        expect_true(all(p$pipeline[["state"]] == "Done"))


        p |> pipe_reset()
        expect_equal(
            p |> pipe_collect_out(all = TRUE),
            list(data = NULL, f1 = NULL, f2 = NULL)
        )
        expect_true(all(p$pipeline[["state"]] == "New"))
    })
})


describe("pipe_run",
{
    test_that("empty pipeline can be run",
    {
        expect_no_error(pipe_new("pipe1") |> pipe_run())
    })

    test_that("returns the pipeline object",
    {
        pip <- pipe_new("pipe1") |> pipe_run()
        expect_equal(pip$name, "pipe1")
    })

    test_that("if function result is a list, all names are preserved",
    {
        # Result list length == 1 - the critical case
        resultList = list(foo = 1)

        pip <- pipe_new("pipe") |>
            pipe_add("f1", \() resultList, keepOut = TRUE)

        out = pipe_run(pip) |> pipe_collect_out()
        expect_equal(out[["f1"]], resultList)

        # Result list length > 1
        resultList = list(foo = 1, bar = 2)

        pip <- pipe_new("pipe") |>
            pipe_add("f1", \() resultList, keepOut = TRUE)

        out = pipe_run(pip) |> pipe_collect_out()
        expect_equal(out[["f1"]], resultList)
    })

    test_that("pipeline execution is correct",
    {
        data <- 9
        pip <- pipe_new("pipe1", data = data)

        foo <-\(a = 1) a
        bar <-\(a, b) a + b

        a <- 5
        pipe_add(pip, "f1", foo, params = list(a = a), keepOut = TRUE)
        pipe_add(
            pip, "f2", bar, params = list(a = ~data, b = ~f1), keepOut = TRUE
        )

        pipe_run(pip)

        expect_equal(pip$pipeline[["out"]][[2]], a)
        expect_equal(pip$pipeline[["out"]][[3]], a + data)
    })


    test_that("pipeline execution can cope with void functions",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) {}, keepOut = TRUE) |>
            pipe_add("f2", \(b = 2) b, keepOut = TRUE)

        out <- pipe_run(pip) |> pipe_collect_out()
        expect_equal(out, list(f1 = NULL, f2 = 2))
    })

    test_that(
        "if pipeline execution fails, the error message is returned as error",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(b = ~f1) stop("something went wrong"))

        expect_error(pipe_run(pip), "something went wrong")
    })

    test_that(
        "can handle 'NULL' results",
    {
        pip <- pipe_new("pipe", data = 1) |>
            pipe_add("f1", \(x = ~data) x, keepOut = TRUE)

        out <- pipe_run(pip) |> pipe_collect_out()
        expect_equal(out[["f1"]], 1)

        pipe_set_data(pip, NULL)
        out <- pipe_run(pip) |> pipe_collect_out()
        expect_equal(out[["f1"]], NULL)
    })

    test_that(
        "can be run recursively to dynamically create and run pipelines",
    {
        pip <- pipe_new("pipe", data = 1) |>
            pipe_add(
                "f1",
                fun =\(data = 10) {
                    pip <- pipe_new("2nd pipe", data = data) |>
                        pipe_add("step1", \(x = ~data) x) |>
                        pipe_add("step2", \(x = ~step1) {
                            2 * x
                        }, keepOut = TRUE)
                }
            )

        pip2 <- pipe_run(pip, recursive = TRUE)
        expect_equal(pip2 |> pipe_get_step_names(), c("data", "step1", "step2"))

        out <- pip2 |> pipe_collect_out()
        expect_equal(out[["step2"]], 20)
    })

    test_that("will not re-run steps that are already done unless forced",
    {
        pip <- pipe_new("pipe", data = 1) |>
            pipe_add("f1", \(x = 2) x) |>
            pipe_add("f2", \(y = ~f1) y + 1)

        pipe_run(pip)
        expect_equal(pipe_get_step(pip, "f1")$state, "Done")
        expect_equal(pipe_get_step(pip, "f2")$state, "Done")
        expect_equal(pip$pipeline[["out"]][[2]], 2)
        expect_equal(pip$pipeline[["out"]][[3]], 3)

        pip$pipeline[2, "out"] <- 0
        pip$pipeline[3, "out"] <- 0
        pipe_run(pip)
        expect_equal(pip$pipeline[["out"]][[2]], 0)
        expect_equal(pip$pipeline[["out"]][[3]], 0)

        # Set parameter, which outdates step and run again
        pipe_run(pip, force = TRUE)
        expect_equal(pip$pipeline[["out"]][[2]], 2)
        expect_equal(pip$pipeline[["out"]][[3]], 3)
    })

    test_that("will never re-run locked steps",
    {
        pip <- pipe_new("pipe", data = 1) |>
            pipe_add("f1", \(x = 2) x) |>
            pipe_add("f2", \(y = ~f1) y + 1)

        pipe_run(pip)
        expect_equal(pip$pipeline[["out"]][[2]], 2)
        expect_equal(pip$pipeline[["out"]][[3]], 3)

        pip$pipeline[2, "out"] <- 0
        pip$pipeline[3, "out"] <- 0
        pipe_run(pip)
        expect_equal(pip$pipeline[["out"]][[2]], 0)
        expect_equal(pip$pipeline[["out"]][[3]], 0)
        pipe_lock_step(pip, "f1")
        pipe_lock_step(pip, "f2")

        # Set parameter, which outdates step and run again
        pipe_run(pip, force = TRUE)
        expect_equal(pip$pipeline[["out"]][[2]], 0)
        expect_equal(pip$pipeline[["out"]][[3]], 0)
    })

    test_that("can clean unkept steps",
    {
        pip <- pipe_new("pipe", data = 1) |>
            pipe_add("f1", \(x = 2) x) |>
            pipe_add("f2", \(y = ~f1) y + 1)

        pipe_run(pip)
        expect_equal(pip$pipeline[["out"]][[2]], 2)
        expect_equal(pip$pipeline[["out"]][[3]], 3)

        pipe_run(pip, cleanUnkept = TRUE)
        expect_true(all(sapply(pip$pipeline[["out"]], is.null)))

        pipe_set_keep_out(pip, "f1", TRUE)
        pipe_run(pip, cleanUnkept = TRUE)
        expect_equal(pip$pipeline[["out"]], list(NULL, 2, NULL))
    })

    test_that("logs warning without interrupting the run",
    {
        pip <- pipe_new("pipe", data = 1) |>
            pipe_add("f1", \(x = 2) x) |>
            pipe_add("f2", \(x = ~f1) {
                warning("something might be wrong")
                x
            }) |>
            pipe_add("f3", \(x = ~f2) x)

        lgr::with_logging(
            log <- utils::capture.output(
                expect_warning(pipe_run(pip), "something might be wrong")
            )
        )

        Filter(log, f =\(x) x |>
            startsWith("WARN")) |>
            grepl(pattern = "something might be wrong") |>
            expect_true()

        wasRunTillEnd <- pipe_get_out(pip, "f3") == 2
        expect_true(wasRunTillEnd)
    })

    test_that("logs error and stops at failed step",
    {
        pip <- pipe_new("pipe", data = 1) |>
            pipe_add("f1", \(x = 2) x) |>
            pipe_add("f2", \(x = ~f1) {
                stop("something went wrong")
                x
            }) |>
            pipe_add("f3", \(x = ~f2) x)

        lgr::with_logging(
            log <- utils::capture.output(
                expect_error(pipe_run(pip), "something went wrong")
            )
        )

        Filter(log, f =\(x) x |>
            startsWith("ERROR")) |>
            grepl(pattern = "something went wrong") |>
            expect_true()

        wasRunTillEnd <- isTRUE(pipe_get_out(pip, "f3") == 2)
        expect_false(wasRunTillEnd)
    })

    test_that("can show progress",
    {
        pip <- pipe_new("pipe", data = 1) |>
            pipe_add("f1", \(x = 2) x) |>
            pipe_add("f2", \(x = ~f1) x)

        m <- mockery::mock()
        pipe_run(pip, progress = m)

        args <- mockery::mock_args(m)

        expect_equal(length(m), pipe_length(pip))
        expect_equal(args[[1]][[1]], 1)
        expect_equal(args[[1]][["detail"]], "data")
        expect_equal(args[[2]][[1]], 2)
        expect_equal(args[[2]][["detail"]], "f1")
        expect_equal(args[[3]][[1]], 3)
        expect_equal(args[[3]][["detail"]], "f2")
    })
})



describe("pipe_run_step",
{
    test_that("pipeline can be run at given step",
    {
        pip <- pipe_new("pipe") |>
            pipe_add("A", \(a = 1) a)

        expect_no_error(pipe_run_step(pip, "A"))
    })


    test_that("upstream steps are by default run with given step",
    {
        pip <- pipe_new("pipe") |>
            pipe_add("A", \(a = 1) a) |>
            pipe_add("B", \(b = ~A) c(b, 2)) |>
            pipe_add("C", \(c = ~B) c(c, 3))

        pipe_run_step(pip, "B")

        expect_equal(pipe_get_out(pip, "A"), 1)
        expect_equal(pipe_get_out(pip, "B"), c(1, 2))
        expect_true(is.null(pipe_get_out(pip, "C")))
    })

    test_that("runs upstream steps in correct order",
    {
        pip <- pipe_new("pipe") |>
            pipe_add("A", \(a = 1) a) |>
            pipe_add("B", \(b = ~A) c(b, 2)) |>
            pipe_add("C", \(c = ~B) c(c, 3))

        pipe_run_step(pip, "C")
        expect_equal(pipe_get_out(pip, "C"), 1:3)
    })

    test_that("runs downstream steps in correct order",
    {
        pip <- pipe_new("pipe") |>
            pipe_add("A", \(a = 1) a) |>
            pipe_add("B", \(b = ~A) c(b, 2)) |>
            pipe_add("C", \(c = ~B) c(c, 3))

        pipe_run_step(pip, "A", downstream = TRUE)
        expect_equal(pipe_get_out(pip, "C"), 1:3)
    })

    test_that("pipeline can be run at given step excluding
        all upstream dependencies",
    {
        pip <- pipe_new("pipe") |>
            pipe_add("A", \(a = 1) a) |>
            pipe_add("B", \(b = ~A) c(b, 2)) |>
            pipe_add("C", \(c = ~B) c(c, 3))

        pipe_run_step(pip, "B", upstream = FALSE)
        expect_true(is.null(pipe_get_out(pip, "A")))
        expect_equal(pipe_get_out(pip, "B"), 2)
        expect_true(is.null(pipe_get_out(pip, "C")))
    })

    test_that("pipeline can be run at given step excluding upstream
        but including downstream dependencies",
    {
        pip <- pipe_new("pipe") |>
            pipe_add("A", \(a = 1) a) |>
            pipe_add("B", \(b = ~A) c(b, 2)) |>
            pipe_add("C", \(c = ~B) c(c, 3))


        pipe_run_step(
            pip,
            "B",
            upstream = FALSE,
            downstream = TRUE
        )
        expect_true(is.null(pipe_get_out(pip, "A")))
        expect_equal(pipe_get_out(pip, "B"), 2)
        expect_equal(pipe_get_out(pip, "C"), c(2, 3))
    })

    test_that("pipeline can be run at given step including
        up- and downstream dependencies",
    {
        pip <- pipe_new("pipe") |>
            pipe_add("A", \(a = 1) a) |>
            pipe_add("B", \(b = ~A) c(b, 2)) |>
            pipe_add("C", \(c = ~B) c(c, 3))


        pipe_run_step(
            pip,
            "B",
            upstream = TRUE,
            downstream = TRUE
        )
        expect_equal(pipe_get_out(pip, "A"), 1)
        expect_equal(pipe_get_out(pip, "B"), c(1, 2))
        expect_equal(pipe_get_out(pip, "C"), c(1, 2, 3))
    })

    test_that("if not marked as keepOut, output of run steps can be cleaned",
    {
        pip <- pipe_new("pipe") |>
            pipe_add("A", \(a = 1) a)


        pipe_run_step(pip, "A", cleanUnkept = TRUE)
        expect_true(is.null(pipe_get_out(pip, "A")))

        pipe_set_keep_out(pip, "A", TRUE) |>
            pipe_run_step("A", cleanUnkept = TRUE)
        expect_false(is.null(pipe_get_out(pip, "A")))
    })

    test_that("up- and downstream steps are marked in log",
    {
        lgr::unsuspend_logging()
        on.exit(lgr::suspend_logging())

        pip <- pipe_new("pipe") |>
            pipe_add("A", \(a = 1) a) |>
            pipe_add("B", \(b = ~A) c(b, 2)) |>
            pipe_add("C", \(c = ~B) c(c, 3))

        logOut <- utils::capture.output(
            pipe_run_step(pip, "B", upstream = TRUE, downstream = TRUE)
        )

        contains <-\(x, pattern) {
            grepl(pattern = pattern, x = x, fixed = TRUE)
        }

        expect_true(logOut[2] |> contains("Step 1/3 A (upstream)"))
        expect_true(logOut[3] |> contains("Step 2/3 B"))
        expect_true(logOut[4] |> contains("Step 3/3 C (downstream)"))
    })

    test_that(
        "updates the timestamp of the run steps",
    {
        pip <- pipe_new("pipe", data = 1) |>
            pipe_add("f1", \(x = ~data) x, keepOut = TRUE)

        before <- pip$pipeline[["time"]]
        Sys.sleep(1)

        pipe_run_step(pip, "f1", upstream = FALSE)
        after <- pip$pipeline[["time"]]

        expect_equal(before[1], after[1])
        expect_true(before[2] < after[2])
    })

    test_that(
        "updates the state of the run steps",
    {
        pip <- pipe_new("pipe", data = 1) |>
            pipe_add("f1", \(x = ~data) x, keepOut = TRUE)

        before <- pip$pipeline[["state"]]
        pipe_run_step(pip, "f1", upstream = FALSE)
        after <- pip$pipeline[["state"]]

        expect_equal(before, c("New", "New"))
        expect_equal(after, c("New", "Done"))
    })

    test_that("will never re-run locked step",
    {
        pip <- pipe_new("pipe", data = 1) |>
            pipe_add("f1", \(x = 2) x) |>
            pipe_add("f2", \(y = ~f1) y + 1)

        pipe_run(pip)
        expect_equal(pip$pipeline[["out"]][[2]], 2)
        expect_equal(pip$pipeline[["out"]][[3]], 3)

        pip$pipeline[2, "out"] <- 0
        pip$pipeline[3, "out"] <- 0
        pipe_lock_step(pip, "f1")
        pipe_run_step(pip, "f1", downstream = TRUE)

        expect_equal(pip$pipeline[["out"]][[2]], 0)
        expect_equal(pip$pipeline[["out"]][[3]], 1)
    })

    test_that("can clean unkept steps",
    {
        pip <- pipe_new("pipe", data = 1) |>
            pipe_add("f1", \(x = 2) x) |>
            pipe_add("f2", \(y = ~f1) y + 1)

        pipe_run_step(pip, "f1", downstream = TRUE, cleanUnkept = TRUE)
        expect_true(all(sapply(pip$pipeline[["out"]], is.null)))

        pipe_set_keep_out(pip, "f1", TRUE)
        pipe_run_step(pip, "f1", downstream = TRUE, cleanUnkept = TRUE)
        expect_equal(pip$pipeline[["out"]], list(NULL, 2, NULL))
    })
})



describe("pipe_set_data",
{
    test_that("data can be set later after pipeline definition",
    {
        dat <- data.frame(a = 1:2, b = 1:2)

        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(x = ~data) x, keepOut = TRUE)

        out <- pipe_run(pip) |> pipe_collect_out()
        expect_equal(out[["f1"]], NULL)

        pipe_set_data(pip, dat)

        out <- pipe_run(pip) |> pipe_collect_out()
        expect_equal(out[["f1"]], dat)
    })

    test_that("if data is set, all dependent steps are set to outdated",
    {
        dat <- data.frame(a = 1:2, b = 1:2)

        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(x = ~data) x, keepOut = TRUE) |>
            pipe_add("f2", \(x = ~f1) x, keepOut = TRUE)

        pipe_run(pip)

        expect_equal(pipe_get_step(pip, "f1")$state, "Done")
        expect_equal(pipe_get_step(pip, "f2")$state, "Done")
        pipe_set_data(pip, dat)
        expect_equal(pipe_get_step(pip, "f1")$state, "Outdated")
        expect_equal(pipe_get_step(pip, "f2")$state, "Outdated")
    })
})



describe("pipe_set_data_split",
{
    test_that("the new steps have the names of the list attached",
    {
        dataList <- list(A = 1, B = 2)

        pip <- pipe_new("pipe") |>
            pipe_add("f1", \(a = 1) a)

        pipe_set_data_split(pip, dataList)

        pipe_get_step_names(pip) |>
            expect_equal(c("data.A", "f1.A", "data.B", "f1.B"))
    })

    test_that("the separator used in the creation of the new steps
    can be customized",
    {
        dataList <- list(A = 1, B = 2)

        pip <- pipe_new("pipe") |>
            pipe_add("f1", \(a = 1) a)

        pipe_set_data_split(pip, dataList, sep = "_")

        pipe_get_step_names(pip) |>
            expect_equal(c("data_A", "f1_A", "data_B", "f1_B"))
    })


    test_that("simple split pipeline computes results as expected",
    {
        dataList <- list(A = 1, B = 2, C = 3)
        pip <- pipe_new("pipe") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(a = ~f1, b = ~data) {
                b + a
            }, keepOut = TRUE)

        pipe_set_data_split(pip, dataList)

        out <- pipe_run(pip) |> pipe_collect_out()
        expect_equivalent(
            unlist(out),
            unlist(lapply(dataList, \(x) x + 1))
        )
    })


    test_that(
        "split pipeline by default overrides output groups according to split",
    {
        dataList <- list(A = 1, B = 2)

        pip <- pipe_new("pipe") |>
            pipe_add("f0", \(a = 1) a, group = "id") |>
            pipe_add("f1", \(a = 1) a, group = "id") |>
            pipe_add("f2", \(a = 2) a)

        pipe_set_data_split(pip, dataList)

        out <- pipe_run(pip) |> pipe_collect_out(all = TRUE)
        expect_equal(names(out), names(dataList))
    })

    test_that("the grouping override can be omitted",
    {
        dataList <- list(A = 1, B = 2)

        pip <- pipe_new("pipe") |>
            pipe_add("f0", \(a = 1) a, group = "id") |>
            pipe_add("f1", \(a = 1) a, group = "id") |>
            pipe_add("f2", \(a = 2) a)

        pipe_set_data_split(pip, dataList, groupBySplit = FALSE)

        out <- pipe_run(pip) |> pipe_collect_out(all = TRUE)

        expect_equal(
            names(out),
            c("data.A", "id.A", "f2.A", "data.B", "id.B", "f2.B")
        )
    })

    test_that("the separator used in the creation of the groups
        can be customized",
    {
        dataList <- list(A = 1, B = 2)

        pip <- pipe_new("pipe") |>
            pipe_add("f0", \(a = 1) a, group = "id") |>
            pipe_add("f1", \(a = 1) a, group = "id") |>
            pipe_add("f2", \(a = 2) a)

        pipe_set_data_split(pip, dataList, groupBySplit = FALSE, sep = "_")

        out <- pipe_run(pip) |> pipe_collect_out(all = TRUE)

        expect_equal(
            names(out),
            c("data_A", "id_A", "f2_A", "data_B", "id_B", "f2_B")
        )
    })

    test_that("split pipeline works for list of data frames",
    {
        dat <- data.frame(x = 1:2, y = 1:2, z = 1:2)
        dataList <- list(A = dat, B = dat, C = dat)
        pip <- pipe_new("pipe") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(a = ~f1, b = ~data) b, keepOut = TRUE) |>
            pipe_add("f3", \(a = ~f1, b = ~data) b[, 2:3], keepOut = TRUE)

        pipe_set_data_split(pip, dataList)

        out <- pipe_run(pip) |> pipe_collect_out()

        expect_equal(out[["A"]], c(f2.A = list(dat), f3.A = list(dat[, 2:3])))
        expect_equal(out[["B"]], c(f2.B = list(dat), f3.B = list(dat[, 2:3])))
        expect_equal(out[["C"]], c(f2.C = list(dat), f3.C = list(dat[, 2:3])))
    })


    test_that("if unnamed list of data frames, they are named with numbers",
    {
        dat <- data.frame(x = 1:2, y = 1:2, z = 1:2)
        dataList <- list(dat, dat)
        pip <- pipe_new("pipe") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(a = ~f1, b = ~data) b, keepOut = TRUE)

        pipe_set_data_split(pip, dataList)

        out <- pipe_run(pip) |> pipe_collect_out()

        expect_equal(out[["1"]], dat)
        expect_equal(out[["2"]], dat)
    })


    test_that(
        "depends are updated correctly, if data split on subset of pipeline",
    {
        dat1 <- data.frame(x = 1:2)
        dat2 <- data.frame(y = 1:2)
        dataList <- list(dat1, dat2)

        pip <- pipe_new("pipe") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(a = ~f1, b = ~data) b, keepOut = TRUE) |>
            pipe_add("f3", \(x = ~f1, y = ~f2) list(x, y), keepOut = TRUE) |>
            pipe_add("f4", \(x = ~f3) x[[1]], keepOut = TRUE)

        pipe_set_data_split(pip, dataList, toStep = "f2")

        ee = expect_equivalent
        pp = pip$pipeline

        depends <- pipe_get_depends(pip)

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


        out <- pipe_run(pip) |> pipe_collect_out()

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

        pip <- pipe_new("pipe", data = data) |>
            pipe_add("split_data_step",
               \(.self = NULL, data = ~data)
                {
                    splitData = split(data, data[, "group"])

                    .self$remove_step("split_data_step")
                    .self$set_data_split(splitData)
                    .self$name = paste(.self$name, "after data split")
                    .self
                }
            ) |>
            pipe_add("f1", \(data = ~data) {
                data
            }, keepOut = TRUE)

        pipe_set_params(pip, list(.self = pip))

        out <- pipe_run(pip, recursive = TRUE) |> pipe_collect_out()

        expect_equivalent(out, split(data, data[, "group"]))
    })
})



describe("pipe_set_keep_out",
{
    test_that("keep-out state can be set",
    {
        pip <- pipe_new("pipe1", data = 0) |>
            pipe_add("f1", \(a = 1) a)

        out <- pipe_run(pip) |> pipe_collect_out()
        expect_false("f1" %in% names(out))

        out <- pipe_set_keep_out(pip, "f1", keepOut = TRUE) |>
            pipe_collect_out()
        expect_true("f1" %in% names(out))

        out <- pipe_set_keep_out(pip, "f1", keepOut = FALSE) |>
            pipe_collect_out()
        expect_false("f1" %in% names(out))
    })

    test_that("step must be a string and exist",
    {
        pip <- pipe_new("pipe1")
        pip <- pipe_new("pipe1", data = 0) |>
            pipe_add("f1", \(a = 1) a)

        expect_error(
            pipe_set_keep_out(pip, 1),
            "is_string(step)",
            fixed = TRUE
        )

        expect_error(
            pipe_set_keep_out(pip, "f2"),
            "step 'f2' does not exist",
            fixed = TRUE
        )
    })

    test_that("state must be logical",
    {
        pip <- pipe_new("pipe1", data = 0) |>
            pipe_add("f1", \(a = 1) a)

        expect_error(
            pipe_set_keep_out(pip, "f1", keepOut = 1),
            "is.logical(keepOut)",
            fixed = TRUE
        )
    })
})




describe("pipe_set_params",
{
    test_that("parameters can be set commonly on existing pipeline",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(a = 2, b = 3) a) |>
            pipe_add("f3", \(a = 4, b = 5) a)

        before <- pipe_get_params(pip)

        after <- pipe_set_params(pip, list(a = 9, b = 99)) |> pipe_get_params()
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
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(a = 2, b = ~f1) a) |>
            pipe_add("f3", \(a = ~f2, b = 5) a)

        before <- pipe_get_params(pip)
        expect_equal(
            before,
            list(
                f1 = list(a = 1),
                f2 = list(a = 2),
                f3 = list(b = 5)
            )
        )

        after <- pipe_set_params(pip, list(a = 9, b = 99)) |> pipe_get_params()
        expect_equal(
            after,
            list(
                f1 = list(a = 9),
                f2 = list(a = 9),
                f3 = list(b = 99)
            )
        )
    })

    test_that("an error is given if params argument is not a list",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a)

        expect_error(
            pipe_set_params(pip, c(a = 9)),
            "params must be a list",
            fixed = TRUE
        )
    })

    test_that("trying to set undefined parameters is signaled with a warning",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a)

        expect_warning(
            pipe_set_params(pip, list(a = 9, b = 9, c = 9)),
            "Trying to set parameters not defined in the pipeline: b, c",
            fixed = TRUE
        )
    })

    test_that("warning for undefined parameters can be omitted",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a)

        expect_no_warning(
            pipe_set_params(pip, list(a = 9, b = 9, c = 9),
                warnUndefined = FALSE
            )
        )
    })

    test_that(
        "after setting a single parameter the params entry is still a list",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1) a)

        expect_equal(pip$pipeline[["params"]][[2]], list(a = 1))

        pipe_set_params(pip, list(a = 9))
        expect_equal(pip$pipeline[["params"]][[2]], list(a = 9))
    })

    test_that(
        "hidden parameters can be set as well",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1, .b = 2) a)

        pipe_set_params(pip, list(a = 9, .b = 10))
        pp <- pipe_get_params(pip, ignoreHidden = FALSE)
        expect_equal(pp, list(f1 = list(a = 9, .b = 10)))
    })

    test_that(
        "trying to set locked parameters is ignored until they are unlocked",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1, b = 2) a + b) |>
            pipe_add("f2", \(a = 1, b = 2) a + b)

        pipe_lock_step(pip, "f1")
        expect_message(
            pipe_set_params(pip, list(a = 9, b = 99)),
            "skipping setting parameters a, b at locked step 'f1'"
        )

        pipe_get_params_at_step(pip, "f1") |> expect_equal(list(a = 1, b = 2))
        pipe_get_params_at_step(pip, "f2") |> expect_equal(list(a = 9, b = 99))

        pipe_unlock_step(pip, "f1")
        pipe_set_params(pip, list(a = 9, b = 99))
        pipe_get_params_at_step(pip, "f1") |> expect_equal(list(a = 9, b = 99))
    })
})


describe("pipe_set_params_at_step",
{
    test_that("parameters can be set at given step",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1, b = 2) a + b) |>
            pipe_add("f2", \(x = 1) x)

        expect_equal(pipe_get_params_at_step(pip, "f1"), list(a = 1, b = 2))

        pipe_set_params_at_step(pip, "f1", list(a = 9, b = 99))
        expect_equal(pipe_get_params_at_step(pip, "f1"), list(a = 9, b = 99))

        pipe_set_params_at_step(pip, "f2", list(x = 9))
        expect_equal(pipe_get_params_at_step(pip, "f2"), list(x = 9))
    })

    test_that("step must be passed as a string and params as a list",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1, b = 2) a + b)

        expect_error(
            pipe_set_params_at_step(pip, 1, list(a = 9, b = 99)),
            "is_string(step) is not TRUE",
            fixed = TRUE
        )

        expect_error(
            pipe_set_params_at_step(pip, "f1", params = c(a = 9, b = 99)),
            "is.list(params) is not TRUE",
            fixed = TRUE
        )
    })

    test_that("hidden parameters can be set as well",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1, .b = 2) a + b)

        pipe_set_params_at_step(pip, "f1", list(a = 9, .b = 99))

        expect_equal(
            pipe_get_params_at_step(pip, "f1", ignoreHidden = FALSE),
            list(a = 9, .b = 99)
        )
    })


    test_that("trying to set undefined parameter signals an error",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1, b = 2) a + b)

        expect_error(
            pipe_set_params_at_step(pip, "f1", list(a = 9, z = 99)),
            "Unable to set parameter(s) z at step f1 - candidates are a, b",
            fixed = TRUE
        )
    })

    test_that("trying to set locked parameter is ignored until it is unlocked",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1, b = 2) a + b)

        pipe_lock_step(pip, "f1")
        expect_message(
            pipe_set_params_at_step(pip, "f1", list(a = 9, b = 99)),
            "skipping setting parameters a, b at locked step 'f1'"
        )

        pipe_get_params_at_step(pip, "f1") |>
            expect_equal(list(a = 1, b = 2))

        pipe_unlock_step(pip, "f1")
        pipe_set_params_at_step(pip, "f1", list(a = 9, b = 99))
        pipe_get_params_at_step(pip, "f1") |>
            expect_equal(list(a = 9, b = 99))
    })


    test_that("setting values for bound parameters is not allowed",
    {
        pip <- pipe_new("pipe1") |>
            pipe_add("f1", \(a = 1, b = 2) a + b) |>
            pipe_add("f2", \(x = 1, y = ~f1) x + y)

        expect_error(
            pipe_set_params_at_step(pip, "f2", list(x = 9, y = 99)),
            "Unable to set parameter(s) y at step f2 - candidates are x",
            fixed = TRUE
        )
    })


    test_that(
        "states of affected steps are updated once the pipeline was run",
    {
        pip <- pipe_new("pipe") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(a = ~f1) a) |>
            pipe_add("f3", \(a = ~f2) a) |>
            pipe_add("f4", \(a = ~data) a)

        pipe_set_params_at_step(pip, "f1", params = list(a = 2))
        expect_true(all(pip$pipeline[["state"]] == "New"))

        pipe_run(pip)
        pipe_set_params_at_step(pip, "f1", params = list(a = 3))
        expect_equal(pipe_get_step(pip, "data")$state, "Done")
        expect_equal(pipe_get_step(pip, "f1")$state, "Outdated")
        expect_equal(pipe_get_step(pip, "f2")$state, "Outdated")
        expect_equal(pipe_get_step(pip, "f3")$state, "Outdated")
        expect_equal(pipe_get_step(pip, "f4")$state, "Done")

        pipe_run(pip)
        expect_true(all(pip$pipeline[["state"]] == "Done"))
    })


    test_that("parameters can be set to NULL",
    {
        pip <- pipe_new("pipe1") |>
        pipe_add("f1", \(a = NULL, b = 1) a)

        pipe_set_params_at_step(pip, "f1", list(a = 1, b = NULL))

        expect_equal(
            pipe_get_params_at_step(pip, "f1"),
            list(a = 1, b = NULL)
        )
    })
})


describe("pipe_split",
{
    test_that("pipeline split of initial pipeline gives the expected result",
    {
        pip <- pipe_new("pipe")
        res <- pipe_split(pip)

        expect_true(is.list(res))
        expect_equal(res[[1]]$name, "pipe1")
        expect_equal(res[[1]]$pipeline, pip$pipeline)
    })


    test_that("pipeline with two indepdendent groups is split correctly",
    {
        pip <- pipe_new("pipe")

        pipe_add(pip, "f1", \(a = ~data) a)
        pipe_add(pip, "f2", \(a = 1) a)
        pipe_add(pip, "f3", \(a = ~f2) a)
        pipe_add(pip, "f4", \(a = ~f1) a)
        pipe_run(pip)

        res <- pipe_split(pip)
        pip1 <- res[[1]]
        pip2 <- res[[2]]
        expect_equal(pip1$name, "pipe1")
        expect_equal(pip2$name, "pipe2")

        expect_equal(pip1$get_step_names(), c("f2", "f3"))
        expect_equal(pip2$get_step_names(), c("data", "f1", "f4"))

        expect_equal(
            pip1$collect_out(all = TRUE),
            pipe_collect_out(pip, all = TRUE)[c("f2", "f3")]
        )
        expect_equal(
            pip2$collect_out(all = TRUE),
            pipe_collect_out(pip, all = TRUE)[c("data", "f1", "f4")]
        )
    })

    test_that(
        "split is done correctly for complete data split",
    {
        dat1 <- data.frame(x = 1:2)
        dat2 <- data.frame(y = 1:2)
        dataList <- list(dat1, dat2)

        pip <- pipe_new("pipe") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_add("f2", \(a = ~f1, b = 2) b) |>
            pipe_add("f3", \(x = ~f1, y = ~f2) x + y)

        pipe_set_data_split(pip, dataList)

        res <- pipe_split(pip)
        steps <- lapply(res, \(x) x$get_step_names())

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



describe("pipe_unlock_step",
{
    test_that("sets 'locked' flag to FALSE if it was TRUE before",
    {
        pip <- pipe_new("pipe") |>
            pipe_add("f1", \(a = 1) a) |>
            pipe_lock_step("f1")

        expect_true(pipe_get_step(pip, "f1")[["locked"]])

        pipe_unlock_step(pip, "f1")
        expect_false(pipe_get_step(pip, "f1")[["locked"]])

        pip
    })
})
