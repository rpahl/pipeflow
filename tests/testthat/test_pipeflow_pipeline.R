
# Pipeline

test_that("initialize",
{
    expect_true(is.function(Pipeline$new("pipe")$initialize))

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
})



test_that("add",
{
    expect_true(is.function(Pipeline$new("pipe")$add))
    test_that("name must be non-empty string",
    {
        pip <- Pipeline$new("pipe1")

        foo <- function(a = 0) a

        expect_error(pip$add("", foo))
        expect_error(pip$add(c("a", "b"), foo))
    })

    test_that("duplicated names are signaled",
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

    test_that("non-existing function parameters are signaled",
    {
        pip <- Pipeline$new("pipe1")
        foo <- function(a = 0) a

        expect_error(
            pip$add("f1", foo, params = list(b = 1, c = 2)),
            "'b', 'c' are no function parameters of 'foo'"
        )
    })

    test_that("undefined function parameters are signaled",
    {
        pip <- Pipeline$new("pipe1")

        foo <- function(a, b = 1) a + b

        expect_error(
            pip$add("f1", foo),
            "'a' parameter(s) must have default values",
            fixed = TRUE
        )

        expect_error(pip$add("f1", foo))
    })

    test_that("step can refer to previous step by relative number",
    {
        pip <- Pipeline$new("pipe1")
        pip$add("f1", function(a = 5) a)
        pip$add("f2", function(x = ~-1) 2*x, keepOut = TRUE)

        out = pip$execute()$collect_out()
        expect_equal(out[["f2"]][[1]], 10)

        pip$add("f3", function(x = ~-1, a = ~-2) x + a, keepOut = TRUE)
        out = pip$execute()$collect_out()
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

        pip$add("f1", function(data = ~.data) data, keepOut = TRUE)
        a <- 1
        pip$add("f2", function(a, b) a + b,
            params = list(a = a, b = ~f1),
            keepOut = TRUE
        )

        expect_equal(unlist(pip$get_step("f1")[["deps"]]), c(data = ".data"))
        expect_equal(unlist(pip$get_step("f2")[["deps"]]), c(b = "f1"))

        out <- pip$execute()$collect_out()
        expect_equal(out[["f1"]][[1]], data)
        expect_equal(out[["f2"]][[1]], a + data)
    })
})


test_that("append",
{
    expect_true(is.function(Pipeline$new("pipe")$append))

    test_that("combined pipelines must not have the same names",
    {
        pip1 <- Pipeline$new("pipe1")
        pip2 <- Pipeline$new("pipe2")

        expect_no_error(pip1$append(pip2))

        pip2$name = pip1$name
        expect_error(
            pip1$append(pip2),
            "pipelines cannot have the same name ('pipe1')",
            fixed = TRUE
        )
    })

    test_that(
        "pipelines with identical steps can be combined without name clash",
    {
        pip1 <- Pipeline$new("pipe1")
        pip2 <- Pipeline$new("pipe2")

        pip1$add("f1", function(a = ~.data) a + 1)
        pip2$add("f1", function(a = ~.data) a + 1)

        pp <- pip1$append(pip2)

        expected_step_names <- c(".data", "f1", ".data.pipe2", "f1.pipe2")
        expect_equal(pp$get_step_names(), expected_step_names)
    })

    test_that(
        "output of first pipeline can be set as input of appended pipeline",
    {
        pip1 <- Pipeline$new("pipe1")
        pip2 <- Pipeline$new("pipe2")

        pip1$add("f1", function(a = ~.data) a + 1)
        pip2$add("f1", function(a = ~.data) a + 1)
        pip2$add("f2", function(a = ~.data) a + 2)

        pp <- pip1$append(pip2, outAsIn = TRUE)

        deps <- pp$get_dependencies()
        expect_equal(deps[["f1.pipe2"]], c(a = "f1"))
        expect_equal(deps[["f2.pipe2"]], c(a = "f1"))

        pp$keep_all_out()
        out <- pp$set_data(1)$execute()$collect_out()
        expect_equal(out[["f2"]][["f2.pipe2"]], 1 + 1 + 2)
    })

    test_that(
        "output of first pipeline can be set as input of appended pipeline",
    {
        pip1 <- Pipeline$new("pipe1")
        pip2 <- Pipeline$new("pipe2")

        pip1$add("f1", function(a = ~.data) a + 1)
        pip2$add("f1", function(a = ~.data) a + 1)
        pip2$add("f2", function(a = ~.data) a + 2)

        pp <- pip1$append(pip2, outAsIn = TRUE)

        deps <- pp$get_dependencies()
        expect_equal(deps[["f1.pipe2"]], c(a = "f1"))
        expect_equal(deps[["f2.pipe2"]], c(a = "f1"))

        pp$keep_all_out()
        out <- pp$set_data(1)$execute()$collect_out()
        expect_equal(out[["f2"]][["f2.pipe2"]], 1 + 1 + 2)
    })


    test_that("if duplicated step names would be created, an error is given",
    {
        pip1 <- Pipeline$new("pipe1")
        pip2 <- Pipeline$new("pipe2")

        mockery::stub(
            where = pip1$append,
            what = "duplicated",
            how = mockery::mock(TRUE, cycle = TRUE)
        )

        pip1$add("f1", function(a = ~.data) a + 1)
        pip2$add("f1", function(a = ~.data) a + 1)

        expect_error(
            pip1$append(pip2),
            "Combined pipeline has duplicated step names",
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

        pip$add("f1", function(a = ~.data) a + 1)
        pip$add("f2", function(a = ~.data, b = ~f1) a + b)
        pip$append_to_step_names("foo")

        expected_names <- c(".data.foo", "f1.foo", "f2.foo")
        expect_equal(pip$get_step_names(), expected_names)

        expected_deps <- list(
            .data.foo = character(0),
            f1.foo = c(a = ".data.foo"),
            f2.foo = c(a = ".data.foo", b = "f1.foo")
        )

        deps <- pip$get_dependencies()
        expect_equal(pip$get_dependencies(), expected_deps)
    })


    test_that("clean_out_at_step",
    {
        expect_true(is.function(Pipeline$new("pipe")$clean_out_at_step))

        test_that("output at given step can be cleaned",
        {
            pip <- Pipeline$new("pipe") |>
                pipe_add("A", function(a = 1) a)

            expect_false(pip$has_out_at_step("A"))

            pip$keep_all_out()$execute()
            expect_true(pip$has_out_at_step("A"))

            pip$clean_out_at_step("A")
            expect_false(pip$has_out_at_step("A"))
        })
    })
})



test_that("collect_out",
{
    expect_true(is.function(Pipeline$new("pipe")$collect_out))

    test_that("pipelines can be combined even if their steps share names",
    {
        pip1 <- Pipeline$new("pipe1", data = 1) |>
            pipe_add("f1", function(a = 1) a, keepOut = TRUE) |>
            pipe_add("f2", function(b = ~f1, c = ~.data, keepOut = FALSE) b + c)

        pip2 <- Pipeline$new("pipe2") |>
            pipe_add("f1", function(a = 10) a, keepOut = FALSE) |>
            pipe_add("f2", function(b = ~f1) b, keepOut = TRUE)

        pp <- pip1$append(pip2)
        expect_equal(pp$length(), pip1$length() + pip2$length())

        out1 <- pip1$execute()$collect_out()
        out2 <- pip2$execute()$collect_out()

        out <- pp$execute()$collect_out()

        expect_equivalent(out, c(out1, out2))
    })

    test_that("data is set as first step but not part of output by default",
    {
        dat <- data.frame(a = 1:2, b = 1:2)
        pip <- Pipeline$new("pipe1", data = dat)
        expect_equal(pip$pipeline[["step"]], ".data")

        out <- pip$execute()$collect_out()
        expect_equal(out, list())

        pip <- Pipeline$new("pipe1", data = dat) |>
            pipe_add("f1", function(x = ~.data) x, keepOut = TRUE)

        out <- pip$execute()$collect_out()
        expect_equal(out[["f1"]][[1]], dat)
    })


    test_that("output of first can be used as data input of appended pipeline",
    {

        pip1 <- Pipeline$new("pipe1", data = 1) |>
            pipe_add("f1", function(a = ~.data) a, keepOut = TRUE) |>
            pipe_add("f2", function(b = 2) b + b, keepOut = TRUE)

        pip2 <- Pipeline$new("pipe2") |>
            pipe_add("f3", function(a2 = ~.data) 2 * a2, keepOut = TRUE) |>
            pipe_add("f4", function(b = 3, c = 4) b + c, keepOut = TRUE) |>
            pipe_add("f5", function(x = ~.data, y = ~f4) x * y, keepOut = TRUE)

        pp <- pip1$append(pip2, outAsIn = TRUE)

        out <- pp$execute()$collect_out()

        expect_equal(out[["f3"]][[1]], 2 * out[["f2"]][[1]])
        expect_equal(out[["f5"]][[1]], out[["f2"]][[1]] * out[["f4"]][[1]])
    })

    test_that("out of previous can be used as input of appended pipelines",
    {

        pip1 <- Pipeline$new("pipe1", data = 1) |>
            pipe_add("f1", function(a = ~.data) a + a)

        pip2 <- Pipeline$new("pipe2") |>
            pipe_add("f1", function(a = ~.data) a + a)

        pip3 <- Pipeline$new("pipe3") |>
            pipe_add("f1", function(a = ~.data) a + a, keepOut = TRUE)

        pp <- pip1$append(pip2, outAsIn = TRUE)$append(pip3, outAsIn = TRUE)

        out <- pp$execute()$collect_out()

        a = 1
        f1 = a + a
        f2 = f1 + f1
        f3 = f2 + f2

        expect_equal(out[["f1"]][["f1.pipe3"]], f3)
    })


    test_that("at the end, pipeline will clean output that shall not be kept",
    {
        data <- 9
        pip <- Pipeline$new("pipe1", data = data)

        foo <- function(a = 1) a
        bar <- function(a, b) a + b

        a <- 5
        pip$add("f1", foo, params = list(a = a))
        pip$add("f2", bar, params = list(a = ~.data, b = ~f1), keepOut = TRUE)

        pip$execute()
        expect_equal(pip$pipeline[["out"]][[2]], list())
        expect_equal(pip$pipeline[["out"]][[3]], a + data)
    })

    test_that("output is collected as expected",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a, keepOut = TRUE) |>
            pipe_add("f2", function(a = 2, b = ~f1) a + b) |>
            pipe_add("f3", function(a = 3, b = ~f2) a + b, keepOut = TRUE)

        out <- pip$execute()$collect_out()
        expect_equal(length(out), 2)
        expect_equal(names(out), c("f1", "f3"))
    })

    test_that("output can be grouped",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a, keepOut = TRUE) |>
            pipe_add("f2", function(a = 2, b = ~f1) a + b, keepOut = TRUE,
                group = "plus") |>
            pipe_add("f3", function(a = 3, b = ~f2) a / b, keepOut = TRUE) |>
            pipe_add("f4", function(a = 3, b = ~f2) a + b, keepOut = TRUE,
                group = "plus")

        pip$execute()

        out_grouped = pip$collect_out()
        expect_true(
            setequal(
                names(out_grouped),
                unique(pip$pipeline[["group"]])[-1]
            )
        )

        out_ungrouped = pip$collect_out(groupBy = "step")
        expect_equal(names(out_ungrouped), pip$pipeline[["step"]][-1])
        names(out_ungrouped)

        hasSameOutValues = setequal(unlist(out_grouped), unlist(out_ungrouped))
        expect_true(hasSameOutValues)
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

        expect_equal(pip$pipeline[["step"]], c(".data", "calc"))
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



test_that("execute",
{
    expect_true(is.function(Pipeline$new("pipe")$execute))
    test_that("empty pipeline can be executed",
    {
        expect_no_error(Pipeline$new("pipe1")$execute())
    })

    test_that("returns the pipeline object",
    {
        pip <- Pipeline$new("pipe1")$execute()
        expect_equal(pip$name, "pipe1")
    })

    test_that("end point of pipeline execution can be customized",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("A", function(a = 1) a) |>
            pipe_add("B", function(b = ~A) b)

        pip$keep_all_out()

        out <- pip$execute(to = 2)$collect_out()

        hasExecutedStepA <- identical(out[["A"]][[1]], 1)
        expect_true(hasExecutedStepA)

        hasExecutedStepB <- identical(out[["B"]][[1]], 1)
        expect_false(hasExecutedStepB)
    })

    test_that("start point of pipeline execution can be customized",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("A", function(a = 1) a) |>
            pipe_add("B", function(b = ~A) b)

        pip$keep_all_out()

        out <- pip$execute(from = 3)$collect_out()
        expect_equal(out[["B"]][[1]], NULL)
    })

    test_that("if function result is a list, all names are preserved",
    {
        # Result list length == 1 - the critical case
        resultList = list(foo = 1)

        pip <- Pipeline$new("pipe") |>
            pipe_add("f1", function() resultList, keepOut = TRUE)

        out = pip$execute()$collect_out()
        res = out[["f1"]][[1]]
        expect_equal(names(res), names(resultList))

        # Result list length > 1
        resultList = list(foo = 1, bar = 2)

        pip <- Pipeline$new("pipe") |>
            pipe_add("f1", function() resultList, keepOut = TRUE)

        out = pip$execute()$collect_out()
        res = out[["f1"]][[1]]
        expect_equal(names(res), names(resultList))
    })

    test_that("pipeline execution is correct",
    {
        data <- 9
        pip <- Pipeline$new("pipe1", data = data)

        foo <- function(a = 1) a
        bar <- function(a, b) a + b

        a <- 5
        pip$add("f1", foo, params = list(a = a), keepOut = TRUE)
        pip$add("f2", bar, params = list(a = ~.data, b = ~f1), keepOut = TRUE)

        pip$execute()

        expect_equal(pip$pipeline[["out"]][[2]], a)
        expect_equal(pip$pipeline[["out"]][[3]], a + data)
    })


    test_that("pipeline execution can cope with void functions",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) {}, keepOut = TRUE) |>
            pipe_add("f2", function(b = 2) b, keepOut = TRUE)

        out <- pip$execute()$collect_out()
        expect_equal(out, list(f1 = list(f1 = NULL), f2 = list(f2 = 2)))
    })

    test_that(
        "if pipeline execution fails, the error message is returned as error",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(b = ~f1) stop("something went wrong"))

        expect_error(pip$execute(), "something went wrong")
    })
})



test_that("execute_step",
{
    expect_true(is.function(Pipeline$new("pipe")$execute_step))

    test_that("pipeline can be executed at given step",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("A", function(a = 1) a)

        pip$keep_all_out()$execute_step("A")
        expect_equal(pip$get_out_at_step("A"), 1)
    })


    test_that("upstream steps are by default executed with given step",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("A", function(a = 1) a) |>
            pipe_add("B", function(b = ~A) c(b, 2)) |>
            pipe_add("C", function(c = ~B) c(c, 3))

        pip$keep_all_out()$execute_step("B")

        expect_equal(pip$get_out_at_step("A"), 1)
        expect_equal(pip$get_out_at_step("B"), c(1, 2))
        expect_false(pip$has_out_at_step("C"))
    })

    test_that("pipeline can be executed at given step excluding
        all upstream dependencies",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("A", function(a = 1) a) |>
            pipe_add("B", function(b = ~A) c(b, 2)) |>
            pipe_add("C", function(c = ~B) c(c, 3))

        pip$keep_all_out()$execute_step("B", upstream = FALSE)
        expect_false(pip$has_out_at_step("A"))
        expect_equal(pip$get_out_at_step("B"), 2)
        expect_false(pip$has_out_at_step("C"))
    })

    test_that("pipeline can be executed at given step excluding upstream
        but including downstream dependencies",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("A", function(a = 1) a) |>
            pipe_add("B", function(b = ~A) c(b, 2)) |>
            pipe_add("C", function(c = ~B) c(c, 3))


        pip$keep_all_out()$execute_step(
            "B",
            upstream = FALSE,
            downstream = TRUE
        )
        expect_false(pip$has_out_at_step("A"))
        expect_equal(pip$get_out_at_step("B"), 2)
        expect_equal(pip$get_out_at_step("C"), c(2, 3))
    })

    test_that("pipeline can be executed at given step including
        up- and downstream dependencies",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("A", function(a = 1) a) |>
            pipe_add("B", function(b = ~A) c(b, 2)) |>
            pipe_add("C", function(c = ~B) c(c, 3))


        pip$keep_all_out()$execute_step(
            "B", upstream = TRUE, downstream = TRUE
        )
        expect_equal(pip$get_out_at_step("A"), 1)
        expect_equal(pip$get_out_at_step("B"), c(1, 2))
        expect_equal(pip$get_out_at_step("C"), c(1, 2, 3))
    })

    test_that("if not marked as keepOut, output of executed steps is discarded",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("A", function(a = 1) a)


        pip$execute_step("A")
        expect_false(pip$has_out_at_step("A"))

        pip$set_keep_out("A", TRUE)$execute_step("A")
        expect_true(pip$has_out_at_step("A"))
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
            pip$execute_step("B", upstream = TRUE, downstream = TRUE)
        )

        contains <- function(x, pattern) {
            grepl(pattern = pattern, x = x, fixed = TRUE)
        }

        expect_true(logOut[2] |> contains("Step 1/3 A (upstream)"))
        expect_true(logOut[3] |> contains("Step 2/3 B"))
        expect_true(logOut[4] |> contains("Step 3/3 C (downstream)"))
    })
})



test_that("get_data",
{
    expect_true(is.function(Pipeline$new("pipe")$get_data))

    test_that("data can be retrieved",
    {
        pip <- Pipeline$new("pipe1", data = 9)

        expect_equal(pip$get_data(), 9)
    })
})



test_that("get_dependencies",
{
    expect_true(is.function(Pipeline$new("pipe")$get_dependencies))

    test_that(
        "dependencies can be retrieved and are named after the steps",
    {
        pip <- Pipeline$new("pipe", data = 1)

        pip$add("f1", function(a = ~.data) a + 1)
        pip$add("f2", function(b = ~f1) b + 1)

        deps <- pip$get_dependencies()
        expected_deps <- list(
            .data = character(0),
            f1 = c(a = ".data"),
            f2 = c(b = "f1")
        )

        expect_equal(deps, expected_deps)
        expect_equal(names(deps), pip$get_step_names())
    })
})



test_that("get_downstream_dependencies",
{
    expect_true(is.function(Pipeline$new("pipe")$get_downstream_dependencies))

    test_that("dependencies can be determined recursively for given step",
    {
        pip <- Pipeline$new("pipe")

        pip$add("f1", function(a = 1) a)
        pip$add("f2", function(a = ~f1) a)
        pip$add("f3", function(a = ~f1, b = ~f2) a + b)
        pip$add("f4", function(a = ~f1, b = ~f2, c = ~f3) a + b + c)

        expect_equal(pip$get_downstream_dependencies("f3"), c("f4"))
        expect_equal(pip$get_downstream_dependencies("f2"), c("f3", "f4"))
        expect_equal(pip$get_downstream_dependencies("f1"), c("f2", "f3", "f4"))
    })

    test_that("if no dependencies an empty character vector is returned",
    {
        pip <- Pipeline$new("pipe")

        pip$add("f1", function(a = 1) a)
        expect_equal(pip$get_downstream_dependencies("f1"), character(0))
    })

    test_that("step must exist",
    {
        pip <- Pipeline$new("pipe")

        expect_error(
            pip$get_downstream_dependencies("f1"),
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

        pip$set_data_split(dataList, to_step = "f2")

        expect_equal(
            pip$get_downstream_dependencies("f1.1"),
            c("f2.1", "f3")
        )

        expect_equal(
            pip$get_downstream_dependencies("f1.2"),
            c("f2.2", "f3")
        )

        expect_equal(pip$get_downstream_dependencies("f2.1"), "f3")
        expect_equal(pip$get_downstream_dependencies("f2.2"), "f3")
    })
})



test_that("get_out_at_step",
{
    expect_true(is.function(Pipeline$new("pipe")$get_out_at_step))

    test_that("output at given step can be retrieved",
    {
        pip <- Pipeline$new("pipe") |>
            pipe_add("A", function(a = 1) a) |>
            pipe_add("B", function(b = 2) b)

        expect_equal(pip$get_out_at_step("A"), NULL)
        expect_equal(pip$get_out_at_step("B"), NULL)

        pip$keep_all_out()$execute()
        expect_equal(pip$get_out_at_step("A"), 1)
        expect_equal(pip$get_out_at_step("B"), 2)
    })
})



test_that("get_parameters",
{
    expect_true(is.function(Pipeline$new("pipe")$get_parameters))

    test_that("parameters can be retrieved",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a, keepOut = TRUE) |>
            pipe_add("f2", function(a, b = ~f1) a + b,
                params = list(a = 8),
                keepOut = TRUE
            ) |>
            pipe_add("f3", function(a = ~f2, b = 3) a + b, keepOut = TRUE)

        p <- pip$get_parameters()
        expect_equal(
            p, list(f1 = list(a = 1), f2 = list(a = 8), f3 = list(b = 3))
        )
    })


    test_that("empty pipeline gives empty list of parameters",
    {
        pip <- Pipeline$new("pipe1")
        expect_equivalent(pip$get_parameters(), list())

        pipe_add(pip, "f1", function() 1)
        expect_equivalent(pip$get_parameters(), list())
    })

    test_that("hidden parameters are filtered out by default",
    {

        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1, .hidden = 2) a)

        p <- pip$get_parameters()
        expect_equal(p, list(f1 = list(a = 1)))

        p <- pip$get_parameters(ignoreHidden = FALSE)
        expect_equal(p, list(f1 = list(a = 1, .hidden = 2)))
    })
})



test_that("get_parameters_at_step",
{
    expect_true(is.function(Pipeline$new("pipe")$get_parameters_at_step))

    test_that("list of step parameters can be retrieved",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1, b = 2) a + b) |>
            pipe_add("f2", function(x = 1, y = 2) x + y)

        expect_equal(
            pip$get_parameters_at_step("f1"), list(a = 1, b = 2)
        )

        expect_equal(
            pip$get_parameters_at_step("f2"), list(x = 1, y = 2)
        )
    })

    test_that("if no parameters empty list is returned",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function() 1)

        expect_equal(
            pip$get_parameters_at_step("f1"), list()
        )
    })

    test_that(
        "hidden parameters are not returned, unless explicitly requested",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1, .b = 2) a + .b)

        expect_equal(
            pip$get_parameters_at_step("f1"), list(a = 1)
        )

        expect_equal(
            pip$get_parameters_at_step("f1", ignoreHidden = FALSE),
            list(a = 1, .b = 2)
        )
    })

    test_that("bound parameters are never returned",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1, b = ~.data) a + b)

        expect_equal(
            pip$get_parameters_at_step("f1"), list(a = 1)
        )

        expect_equal(
            pip$get_parameters_at_step("f1", ignoreHidden = FALSE),
            list(a = 1)
        )
    })

    test_that("step must exist",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1, b = ~.data) a + b)

        expect_error(
            pip$get_parameters_at_step("foo"),
            "step 'foo' not found"
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

        expect_equal(pip$get_step(".data"), pip$pipeline[1, ])
        expect_equal(pip$get_step("f1"), pip$pipeline[2, ])

        expect_error(pip$get_step("foo"), "step 'foo' not found")
    })

    test_that("dependencies are recorded as expected",
    {
        pip <- Pipeline$new("pipe1", data = 9)

        foo <- function(a = 0) a
        bar <- function(a = 1, b = 2) a + b

        pip$add("f1", foo)
        pip$add("f2", bar, params = list(a = ~.data, b = ~f1))

        expect_true(length(unlist(pip$get_step("f1")[["deps"]])) == 0)
        expect_equal(
            unlist(pip$get_step("f2")[["deps"]]),
            c("a" = ".data", b = "f1")
        )
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



test_that("get_unique_parameters",
{
    expect_true(is.function(Pipeline$new("pipe")$get_unique_parameters))

    test_that("parameters can be retrieved uniquely",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a, keepOut = TRUE) |>
            pipe_add("f2", function(a, b = ~f1) a + b,
                params = list(a = 8), keepOut = TRUE
            ) |>
            pipe_add(
                "f3", function(a = ~f2, b = 3, c = 4) a + b, keepOut = TRUE
            )

        p <- pip$get_unique_parameters()
        expect_equivalent(p, list(
            f1 = list(a = 1),
            f3 = list(b = 3, c = 4)
        ))
    })

    test_that(
        "if parameter occurs multiple times, the 1st default value is used",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(a = 2) a)

        p <- pip$get_unique_parameters()
        expect_equal(p, list(f1 = list(a = 1)))

        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 2) a) |>
            pipe_add("f2", function(a = 1) a)

        p <- pip$get_unique_parameters()
        expect_equal(p, list(f1 = list(a = 2)))
    })

    test_that("empty pipeline gives empty list of unique parameters",
    {
        pip <- Pipeline$new("pipe1")
        expect_equivalent(pip$get_unique_parameters(), list())

        pipe_add(pip, "f1", function() 1)
        expect_equivalent(pip$get_unique_parameters(), list())
    })
})



test_that("get_unique_parameters_json",
{
    expect_true(is.function(Pipeline$new("pipe")$get_unique_parameters_json))

    test_that("the elements are not named",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a, keepOut = TRUE)

        p <- pip$get_unique_parameters_json()
        l <- jsonlite::fromJSON(p, simplifyVector = FALSE)
        expect_equal(names(l), NULL)


        pip <- Pipeline$new("pipe1") |>
            pipe_add(
                "f1", function(x = new("StringParam", "my x", "some x")) x
            ) |>
            pipe_add(
                "f2", function(y = new("StringParam", "my y", "some y")) y
            )

        p <- pip$get_unique_parameters_json()
        l <- jsonlite::fromJSON(p, simplifyVector = FALSE)
        expect_equal(names(l), NULL)
    })


    test_that("standard parameters are returned as name-value pairs",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a, keepOut = TRUE) |>
            pipe_add("f2", function(a, b = ~f1) a + b, params = list(a = 8)) |>
            pipe_add("f3", function(a = ~f2, b = 3, c = 4) a + b)

        p <- pip$get_unique_parameters_json()
        expect_true(methods::is(p, "json"))
        out <- utils::capture.output(cat(p))

        expect_equal(
            out,
            c(
                "[",
                "  {",
                "    \"name\": \"a\",",
                "    \"value\": 1",
                "  },",
                "  {",
                "    \"name\": \"b\",",
                "    \"value\": 3", "  },",
                "  {",
                "    \"name\": \"c\",",
                "    \"value\": 4",
                "  }",
                "]"
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

        p <- pip$get_unique_parameters_json()
        out <- utils::capture.output(cat(p))
        expect_equal(
            out,
            c(
                "[",
                "  {",
                "    \"value\": \"some x\",",
                "    \"name\": \"x\",",
                "    \"advanced\": false,",
                "    \"label\": \"my x\",",
                "    \"description\": \"\",",
                "    \"source\": \"internal\",",
                "    \"class\": \"StringParam\"",
                "  },",
                "  {",
                "    \"value\": \"some y\",",
                "    \"name\": \"y\",",
                "    \"advanced\": false,",
                "    \"label\": \"my y\",",
                "    \"description\": \"\",",
                "    \"source\": \"internal\",",
                "    \"class\": \"StringParam\"",
                "  }",
                "]"
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

        p <- pip$get_unique_parameters_json()
        l <- jsonlite::fromJSON(p)

        expect_true(l[["label"]][[1]] == "my x")
        expect_false(l[["name"]][[1]] == "my x")
        hasArgName <- l[["name"]][[1]] == "x"

        expect_true(l[["label"]][[2]] == "my y")
        expect_false(l[["name"]][[2]] == "my y")
        hasArgName <- l[["name"]][[2]] == "y"
    })
})



test_that("get_upstream_dependencies",
{
    expect_true(is.function(Pipeline$new("pipe")$get_upstream_dependencies))

    test_that("dependencies can be determined recursively for given step",
    {
        pip <- Pipeline$new("pipe")

        pip$add("f1", function(a = 1) a)
        pip$add("f2", function(a = ~f1) a)
        pip$add("f3", function(a = ~f1, b = ~f2) a + b)
        pip$add("f4", function(a = ~f1, b = ~f2, c = ~f3) a + b + c)

        expect_equal(pip$get_upstream_dependencies("f2"), c("f1"))
        expect_equal(pip$get_upstream_dependencies("f3"), c("f1", "f2"))
        expect_equal(pip$get_upstream_dependencies("f4"), c("f1", "f2", "f3"))
    })

    test_that("if no dependencies an empty character vector is returned",
    {
        pip <- Pipeline$new("pipe")

        pip$add("f1", function(a = 1) a)
        expect_equal(pip$get_upstream_dependencies("f1"), character(0))
    })

    test_that("step must exist",
    {
        pip <- Pipeline$new("pipe")

        expect_error(
            pip$get_upstream_dependencies("f1"),
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

        pip$set_data_split(dataList, to_step = "f2")

        expect_equal(pip$get_upstream_dependencies("f2.1"), c("f1.1"))
        expect_equal(pip$get_upstream_dependencies("f2.2"), c("f1.2"))

        expect_equivalent(
            pip$get_upstream_dependencies("f3"),
            c("f1.1", "f2.1", "f1.2", "f2.2")
        )
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



test_that("has_out_at_step",
{
    expect_true(is.function(Pipeline$new("pipe")$set_keep_out))

    test_that("it can be checked if pipeline has output at a step",
    {
        pip <- Pipeline$new("pipe1", data = 0) |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(a = 1) a, keepOut = TRUE)

        pip$execute()

        expect_false(pip$has_out_at_step("f1"))
        expect_true(pip$has_out_at_step("f2"))
    })

    test_that("step must be a string and exist",
    {
        pip <- Pipeline$new("pipe1")
        pip <- Pipeline$new("pipe1", data = 0) |>
            pipe_add("f1", function(a = 1) a)

        expect_error(
            pip$has_out_at_step(1),
            "is_string(step)",
            fixed = TRUE
        )

        expect_error(
            pip$has_out_at_step("f2"),
            "step 'f2' does not exist",
            fixed = TRUE
        )
    })
})


test_that("keep_all_out",
{
    expect_true(is.function(Pipeline$new("pipe")$keep_all_out))

    test_that("pipeline can be set to keep all output",
    {
        pip <- Pipeline$new("pipe1", data = 0) |>
            pipe_add("f1", function(a = 1) a)

        expect_false(pip$has_out_at_step(".data"))
        expect_false(pip$has_out_at_step("f1"))

        pip$keep_all_out()$execute()
        expect_true(pip$has_out_at_step(".data"))
        expect_true(pip$has_out_at_step("f1"))
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

        expect_equal(pip$get_step_names(), c(".data", "f2"))
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
            pipe_add("f5", function(x = ~.data) x)

        out <- utils::capture.output(
            pip$remove_step("f1", recursive = TRUE),
            type = "message"
        )

        expect_equal(pip$get_step_names(), c(".data", "f5"))
        expect_equal(
            out,
            paste(
                "Removing step 'f1' and its downstream dependencies:",
                "'f2', 'f3', 'f4'"
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

        out = unname(unlist(pip$execute()$collect_out()))
        expect_equal(out, 2)

        pip$replace_step("f2", function(z = 4) z)
        out = unname(unlist(pip$execute()$collect_out()))
        expect_equal(out, 4)
    })

    test_that("the replacing function can be passed as a string",
    {

        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(x = 3) x, keepOut = TRUE)

        out = unname(unlist(pip$execute()$collect_out()))
        expect_equal(out, 3)

        .my_func <- function(x = 3) {
            2 * x
        }
        assign(".my_func", .my_func, envir = globalenv())
        on.exit(rm(".my_func", envir = globalenv()))

        pip$replace_step("f1", fun = ".my_func", keepOut = TRUE)

        out = unname(unlist(pip$execute()$collect_out()))
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

        out = unname(unlist(pip$execute()$collect_out()))
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
})



test_that("set_data",
{
    expect_true(is.function(Pipeline$new("pipe")$set_data))

    test_that("data can be set later after pipeline definition",
    {
        dat <- data.frame(a = 1:2, b = 1:2)

        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(x = ~.data) x, keepOut = TRUE)
        out <- pip$execute()$collect_out()
        expect_equal(out[[1]], list(f1 = NULL))

        pip$set_data(dat)

        out <- pip$execute()$collect_out()
        expect_equal(out[["f1"]][[1]], dat)
    })
})



test_that("set_data_split",
{
    expect_true(is.function(Pipeline$new("pipe")$set_data_split))

    test_that("simple split pipeline works",
    {
        dataList <- list(A = 1, B = 2, C = 3, D = 4)
        pip <- Pipeline$new("pipe") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(a = ~f1, b = ~.data) {
                a + b
            }, keepOut = TRUE)

        pip$set_data_split(dataList)

        out <- pip$execute()$collect_out()
        expect_equivalent(
            unlist(out),
            unlist(lapply(dataList, function(x) x + 1))
        )
    })


    test_that(
        "split pipeline by default groups output according to split",
    {
        dataList <- list(A = 1, B = 2, C = 3)
        pip <- Pipeline$new("pipe") |>
            pipe_add("f0", function(a = 1) a, keepOut = TRUE, group = "id") |>
            pipe_add("f1", function(a = 1) a, keepOut = TRUE, group = "id") |>
            pipe_add("f2", function(a = ~f1, b = ~.data) {
                a + b
            }, keepOut = TRUE)

        pip$set_data_split(dataList)

        out <- pip$execute()$collect_out()
        expect_equal(names(out), names(dataList))
    })

    test_that("grouping by data split can be omitted",
    {
        dataList <- list(A = 1, B = 2, C = 3)
        pip <- Pipeline$new("pipe") |>
            pipe_add("f0", function(a = 1) a, keepOut = TRUE, group = "id") |>
            pipe_add("f1", function(a = 1) a, keepOut = TRUE, group = "id") |>
            pipe_add("f2", function(a = ~f1, b = ~.data) {
                a + b
            }, keepOut = TRUE)

        pip$set_data_split(dataList, groupBySplit = FALSE)

        out <- pip$execute()$collect_out()
        expect_equal(
            names(out),
            paste0(rep(names(dataList), each = 2), ".", c("f2", "id"))
        )

        expect_equal(names(out[["A.id"]]), c("f0.A", "f1.A"))
    })

    test_that("split pipeline works for list of data frames",
    {
        dat <- data.frame(x = 1:2, y = 1:2, z = 1:2)
        dataList <- list(A = dat, B = dat, C = dat)
        pip <- Pipeline$new("pipe") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(a = ~f1, b = ~.data) b, keepOut = TRUE) |>
            pipe_add(
                "f3", function(a = ~f1, b = ~.data) b[, 2:3], keepOut = TRUE
            )

        pip$set_data_split(dataList)

        out <- pip$execute()$collect_out()

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
            pipe_add("f2", function(a = ~f1, b = ~.data) b, keepOut = TRUE)

        pip$set_data_split(dataList)

        out <- pip$execute()$collect_out()

        expect_equal(out[["1"]], c(`f2.1` = list(dat)))
        expect_equal(out[["2"]], c(`f2.2` = list(dat)))
    })


    test_that("deps are updated correctly, if data split on subset of pipeline",
    {
        dat1 <- data.frame(x = 1:2)
        dat2 <- data.frame(y = 1:2)
        dataList <- list(dat1, dat2)

        pip <- Pipeline$new("pipe") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(a = ~f1, b = ~.data) b, keepOut = TRUE) |>
            pipe_add(
                "f3", function(x = ~f1, y = ~f2) list(x, y), keepOut = TRUE
            ) |>
            pipe_add("f4", function(x = ~f3) x[[1]], keepOut = TRUE)

        pip$set_data_split(dataList, to_step = "f2")

        ee = expect_equivalent
        pp = pip$pipeline

        deps <- pip$get_dependencies()

        expect_equal(deps[["f2.1"]], c(a = "f1.1", b = ".data.1"))
        expect_equal(deps[["f2.2"]], c(a = "f1.2", b = ".data.2"))

        # Pipeline was not split for f3, which therefore has parameters that
        # each depend on two steps
        expect_equal(
            deps[["f3"]],
            list(x = c("f1.1", "f1.2"), y = c("f2.1", "f2.2"))
        )

        # Pipeline was not split for f4, so just depdends on f3
        ee(deps[["f4"]], c(x = "f3"))


        out <- pip$execute()$collect_out()

        expect_equal(out[["1"]][["f2.1"]], dat1)
        expect_equal(out[["2"]][["f2.2"]], dat2)
        expected_f3_res = list(
            list("f1.1" = 1, "f1.2" = 1),
            list("f2.1" = dat1, "f2.2" = dat2)
        )
        expect_equal(out[["f3"]][["f3"]], expected_f3_res)
        expect_equal(out[["f4"]][["f4"]], expected_f3_res[[1]])
    })

    test_that("split data set can be created dynamically",
    {

        data = data.frame(a = 1:10, group = c("a", "b"))

        pip <- Pipeline$new("pipe", data = data) |>

            pipe_add("split_data_step",
                function(.self = NULL, data = ~.data)
                {
                    splitData = split(data, data[, "group"])

                    .self$remove_step("split_data_step")
                    .self$set_data_split(splitData)
                    .self$name = paste(.self$name, "after data split")
                    .self
                }
            ) |>

            pipe_add("f1", function(data = ~.data) {
                data
            }, keepOut = TRUE)

        pip$set_parameters(list(.self = pip))

        out <- pip$execute()$collect_out()

        expect_equivalent(unlist1(out), split(data, data[, "group"]))
    })
})



test_that("set_keep_out",
{
    expect_true(is.function(Pipeline$new("pipe")$set_keep_out))

    test_that("keep-out status can be set",
    {
        pip <- Pipeline$new("pipe1", data = 0) |>
            pipe_add("f1", function(a = 1) a)

        pip$execute()
        expect_false(pip$has_out_at_step("f1"))

        pip$set_keep_out("f1", status = TRUE)$execute()
        expect_true(pip$has_out_at_step("f1"))

        pip$clean_out_at_step("f1")
        pip$set_keep_out("f1", status = FALSE)$execute()
        expect_false(pip$has_out_at_step("f1"))
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

    test_that("status must be logical",
    {
        pip <- Pipeline$new("pipe1", data = 0) |>
            pipe_add("f1", function(a = 1) a)

        expect_error(
            pip$set_keep_out("f1", status = 1),
            "is.logical(status)",
            fixed = TRUE
        )
    })
})




test_that("set_parameters",
{
    expect_true(is.function(Pipeline$new("pipe")$set_parameters))

    test_that("parameters can be set commonly on existing pipeline",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a, keepOut = TRUE) |>
            pipe_add("f2", function(a, b = ~f1) a + b,
                params = list(a = 8),
                keepOut = TRUE
            ) |>
            pipe_add("f3", function(a = ~f2, b = 3) a + b, keepOut = TRUE)

        before <- pip$get_parameters()

        after <- pip$set_parameters(list(a = 9, b = 99))$get_parameters()
        expect_equal(after, list(
            f1 = list(a = 9),
            f2 = list(a = 9, b = 99),
            f3 = list(a = 9, b = 99)
        ))
    })

    test_that("trying to set undefined parameters is signaled with a warning",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a)

        expect_warning(
            pip$set_parameters(list(a = 9, b = 9, c = 9)),
            "Trying to set parameters not defined in the pipeline: b, c",
            fixed = TRUE
        )
    })

    test_that("warning for undefined parameters can be omitted",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a)

        expect_no_warning(
            pip$set_parameters(list(a = 9, b = 9, c = 9),
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

        pip$set_parameters(list(a = 9))
        expect_equal(pip$pipeline[["params"]][[2]], list(a = 9))
    })
})



test_that("set_parameters_at_step",
{
    expect_true(is.function(Pipeline$new("pipe")$set_parameters_at_step))

    test_that("parameters can be set at given step",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1, b = 2) a + b) |>
            pipe_add("f2", function(x = 1) x)

        expect_equal(pip$get_parameters_at_step("f1"), list(a = 1, b = 2))

        pip$set_parameters_at_step("f1", list(a = 9, b = 99))
        expect_equal(pip$get_parameters_at_step("f1"), list(a = 9, b = 99))

        pip$set_parameters_at_step("f2", list(x = 9))
        expect_equal(pip$get_parameters_at_step("f2"), list(x = 9))
    })

    test_that("step must be passed as a string and params as a list",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1, b = 2) a + b)

        expect_error(
            pip$set_parameters_at_step(1, list(a = 9, b = 99)),
            "is_string(step) is not TRUE",
            fixed = TRUE
        )

        expect_error(
            pip$set_parameters_at_step("f1", params = c(a = 9, b = 99)),
            "is.list(params) is not TRUE",
            fixed = TRUE
        )
    })

    test_that("hidden parameters can be set as well",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1, .b = 2) a + b)

        pip$set_parameters_at_step("f1", list(a = 9, .b = 99))

        expect_equal(
            pip$get_parameters_at_step("f1", ignoreHidden = FALSE),
            list(a = 9, .b = 99)
        )
    })


    test_that("trying to set undefined parameter signals an error",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1, b = 2) a + b)

        expect_error(
            pip$set_parameters_at_step("f1", list(a = 9, z = 99)),
            "Unable to set parameter(s) z at step f1 - candidates are a, b",
            fixed = TRUE
        )
    })

    test_that("setting bound parameters will not be allowed",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1, b = 2) a + b) |>
            pipe_add("f2", function(x = 1, y = ~f1) x + y)

        expect_error(
            pip$set_parameters_at_step("f2", list(x = 9, y = 99)),
            "Unable to set parameter(s) y at step f2 - candidates are x",
            fixed = TRUE
        )
    })


    test_that(
        "pipeline can be defined and executed with Param class parameters",
    {
        pip <- Pipeline$new("pipe1") |>
            pipe_add(
                "f1", function(xCol = new("StringParam", "xCol", "x")) xCol,
                keepOut = TRUE
            ) |>
            pipe_add("f2", function(yCol, b = ~f1) paste(yCol, b),
                params = list(yCol = new("StringParam", "yCol", "y")),
                keepOut = TRUE
            ) |>
            pipe_add("f3", function(a = ~f2, b = 3) paste(a, b), keepOut = TRUE)

        p <- pip$get_parameters()
        expect_equal(p[["f1"]][["xCol"]], new("StringParam", "xCol", "x"))
        expect_equal(p[["f2"]][["yCol"]], new("StringParam", "yCol", "y"))

        res <- pip$execute()$collect_out() |>
            unlist(recursive = FALSE)
        expect_equal(res, list(f1.f1 = "x", f2.f2 = "y x", f3.f3 = "y x 3"))
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

        log <- utils::capture.output(pip$execute())
        isValidJSON <- sapply(log, jsonlite::validate)
        expect_true(all(isValidJSON))
    })

    test_that("each step is logged with its name",
    {
        lgr::unsuspend_logging()
        on.exit(lgr::suspend_logging())

        dat <- data.frame(a = 1:2, b = 1:2)
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(x = ~.data) x)

        log <- utils::capture.output(pip$execute())

        step1 = jsonlite::fromJSON(log[2])
        step2 = jsonlite::fromJSON(log[3])

        expect_equal(step1[["message"]], "Step 1/2 .data")
        expect_equal(step2[["message"]], "Step 2/2 f1")
    })

    test_that(
        "upon warning during execute, both context and warn msg are logged",
    {
        lgr::unsuspend_logging()
        on.exit(lgr::suspend_logging())

        dat <- data.frame(a = 1:2, b = 1:2)
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function() warning("this is a warning"))

        expect_warning(log <- utils::capture.output(pip$execute()))

        log_fields = lapply(head(log, -1), jsonlite::fromJSON)
        warnings = Filter(
            log_fields,
            f = function(x) x[["level"]] == "warn"
        )[[1]]

        expect_equal(
            warnings[["message"]],
            "Context: pipeline at step 3 ('f2'), this is a warning"
        )
    })


    test_that(
        "upon error during execute, both context and error msg are logged",
    {
        lgr::unsuspend_logging()
        on.exit(lgr::suspend_logging())

        dat <- data.frame(a = 1:2, b = 1:2)
        pip <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function() stop("technical error message"))

        log <- utils::capture.output({
            tryCatch(pip$execute(), error = identity)
        })

        log_fields = lapply(head(log, -1), jsonlite::fromJSON)
        last = tail(log_fields, 1)[[1]]

        expect_equal(last[["message"]],
            "Context: pipeline at step 3 ('f2'), technical error message")
    })

    test_that("pipeline start is marked in the log and has the pipeline's name",
    {
        lgr::unsuspend_logging()
        on.exit(lgr::suspend_logging())

        dat <- data.frame(a = 1:2, b = 1:2)
        pipeName = "pipe1"
        pip <- Pipeline$new(pipeName, data = dat)

        log <- utils::capture.output(pip$execute())
        logStart = log[[1]]
        log_fields = jsonlite::fromJSON(logStart)

        expect_equal(log_fields[["type"]], "start_pipeline")
        expect_equal(log_fields[["pipeline_name"]], pipeName)
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


    # .derive_dependencies
    test_that("private .derive_dependencies works as expected",
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
                f(params = list(), step = "step", to_step = 2),
                "is_string(to_step) is not TRUE",
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

    # .execute_step
    test_that("private .execute_step works as expected",
    {
        pip <- expect_no_error(Pipeline$new("pipe"))
        f <- get_private(pip)$.execute_step

        test_that("returns the result of the function at the given step",
        {
            pip <- Pipeline$new("pipe") |>
                pipe_add("A", function(a = 1) a) |>
                pipe_add("B", function(b = 2) b)

            f <- get_private(pip)$.execute_step

            expect_equal(f(step = "A"), 1)
            expect_equal(f(step = "B"), 2)
        })

        test_that("stores the result at the given step",
        {
            pip <- Pipeline$new("pipe") |>
                pipe_add("A", function(a = 1) a) |>
                pipe_add("B", function(b = 2) b)

            f <- get_private(pip)$.execute_step

            expect_equal(f(step = "A"), 1)
            expect_equal(pip$get_out_at_step("A"), 1)
            expect_equal(f(step = "B"), 2)
            expect_equal(pip$get_out_at_step("B"), 2)
        })

        test_that("uses output of dependent steps",
        {
            pip <- Pipeline$new("pipe") |>
                pipe_add("A", function(a = 1) a) |>
                pipe_add("B", function(b = ~A) b)

            f <- get_private(pip)$.execute_step

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

            f <- get_private(pip)$.execute_step
        })

        test_that("if error, the failing step is given in the log",
        {
            lgr::unsuspend_logging()
            on.exit(lgr::suspend_logging())

            pip <- Pipeline$new("pipe") |>
                pipe_add("A", function(a = 1) stop("something went wrong"))

            f <- get_private(pip)$.execute_step
            log <- capture.output(expect_error(f("A")))
            hasInfo <- grepl(
                log[1],
                pattern = "pipeline at step 2 ('A'), something went wrong",
                fixed = TRUE
            )
            expect_true(hasInfo)
        })

        test_that("handles Param object args",
        {
            skip("for now: enable that the default can be an expression?")
            x <- 1
            pip <- Pipeline$new("pipe") |>
                pipe_add("A", function(a = x) a)

            f <- get_private(pip)$.execute_step
        })

        test_that("handles Param object args",
        {
            skip("for now: enable that the default can be an expression?")
            p <- new("NumericParam", "a", value = 1)
            pip <- Pipeline$new("pipe") |>
                pipe_add("A", function(a = p) a)

            f <- get_private(pip)$.execute_step
        })
    })


    # .extract_dependent_out
    test_that("private .extract_dependent_out works as expected",
    {
        pip <- expect_no_error(Pipeline$new("pipe"))
        f <- get_private(pip)$.extract_dependent_out

        test_that("if no deps, NULL is returned",
        {
            expect_true(is.null(f(deps = list(), out = list())))
        })

        test_that("signals badly typed input",
        {
            expect_error(
                f(1:2, list()),
                "is.character(deps) || is.list(deps) is not TRUE",
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
                f(deps = "foo", out = list("foo" = 1))
            )
            expect_error(
                f(deps = "foo", out = list()),
                "all(unlist(deps) %in% names(out)) is not TRUE",
                fixed = TRUE
            )

            # Dependencies can be lists of dependencies
            deps <- list(x = c("a", "b"))
            out <- list(a = 1)
            expect_error(f(deps, out))
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

    # .extract_deps_from_param_list
    test_that("private .extract_deps_from_param_list works as expected",
    {
        pip <- expect_no_error(Pipeline$new("pipe"))
        f <- get_private(pip)$.extract_deps_from_param_list

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

    # .get_downstream_deps
    test_that("private .get_downstream_deps works as expected",
    {
        pip <- expect_no_error(Pipeline$new("pipe"))
        f <- get_private(pip)$.get_downstream_deps

        test_that("badly typed inputs are signalled",
        {
            expect_error(
                f(step = 1),
                "is_string(step) is not TRUE",
                fixed = TRUE
            )

            expect_error(
                f(step = "foo", deps = c(a = 1)),
                "is.character(deps) || is.list(deps) is not TRUE",
                fixed = TRUE
            )

            expect_error(
                f(step = "foo", deps = list(), recursive = 1),
                "is.logical(recursive)",
                fixed = TRUE
            )
        })

        test_that("if no dependencies, empty character vector is returned",
        {
            expect_equal(f(step = "foo", deps = character()), character(0))
            expect_equal(f(step = "foo", deps = list()), character(0))
        })

        test_that(
            "if no deps, recursive should give same as non-recursiv call",
        {
            expect_equal(
                f(step = "foo", deps = list()),
                f(step = "foo", deps = list(), recursive = FALSE)
            )
        })


        test_that("dependencies by default are determined recursively",
        {
            deps <- list(
                f1 = c(a = "f0"),
                f2 = c(a = "f1"),
                f3 = c(a = "f2")
            )

            expect_equal(f("f3", deps), character(0))
            expect_equal(f("f2", deps), c("f3"))
            expect_equal(f("f1", deps), c("f2", "f3"))
            expect_equal(f("f0", deps), c("f1", "f2", "f3"))
        })

        test_that("returned dependencies are unique",
        {
            deps <- list(
                f1 = c(a = "f0"),
                f2 = c(a = "f0", b = "f1"),
                f3 = c(a = "f0", b = "f1", c = "f2")
            )

            expect_equal(f("f3", deps), character(0))
            expect_equal(f("f2", deps), c("f3"))
            expect_equal(f("f1", deps), c("f2", "f3"))
            expect_equal(f("f0", deps), c("f1", "f2", "f3"))
        })

        test_that("dependencies can be determined non-recursively",
        {
            deps <- list(
                f1 = c(a = "f0"),
                f2 = c(a = "f1"),
                f3 = c(a = "f2")
            )

            expect_equal(f("f3", deps, recursive = FALSE), character(0))
            expect_equal(f("f2", deps, recursive = FALSE), c("f3"))
            expect_equal(f("f1", deps, recursive = FALSE), c("f2"))
            expect_equal(f("f0", deps, recursive = FALSE), c("f1"))
        })

        test_that("works with multiple dependencies given in sublist",
        {
            deps <- list(
                f2 = c(a = "f1", b = ".data"),
                f3 = list(
                    x = c("f0", "f1"),
                    y = c("f1", "f2")
                )
            )

            expect_equal(f("f1", deps), c("f2", "f3"))
        })
    })


    # .get_last_step
    test_that("private .get_last_step works as expected",
    {
        pip <- expect_no_error(Pipeline$new("pipe"))
        f <- get_private(pip)$.get_last_step

        expect_equal(f(), ".data")

        pip$add("f1", function(a = 1) a)
        expect_equal(f(), "f1")

        pip$add("f2", function(a = 1) a)
        expect_equal(f(), "f2")

        pip$pop_step()
        expect_equal(f(), "f1")
    })

    # .get_upstream_deps
    test_that("private .get_upstream_deps works as expected",
    {
        pip <- expect_no_error(Pipeline$new("pipe"))
        f <- get_private(pip)$.get_upstream_deps

        test_that("badly typed inputs are signalled",
        {
            expect_error(
                f(step = 1),
                "is_string(step) is not TRUE",
                fixed = TRUE
            )

            expect_error(
                f(step = "foo", deps = c(a = 1)),
                "is.character(deps) || is.list(deps) is not TRUE",
                fixed = TRUE
            )

            expect_error(
                f(step = "foo", deps = list(), recursive = 1),
                "is.logical(recursive)",
                fixed = TRUE
            )
        })

        test_that("if no dependencies, empty character vector is returned",
        {
            expect_equal(f(step = "foo", deps = character()), character(0))
            expect_equal(f(step = "foo", deps = list()), character(0))
        })

        test_that("dependencies by default are determined recursively",
        {
            deps <- list(
                f1 = c(a = "f0"),
                f2 = c(a = "f1"),
                f3 = c(a = "f2")
            )

            expect_equal(f("f0", deps), character(0))
            expect_equal(f("f1", deps), c("f0"))
            expect_equal(f("f2", deps), c("f1", "f0"))
            expect_equal(f("f3", deps), c("f2", "f1", "f0"))
        })

        test_that("returned dependencies are unique",
        {
            deps <- list(
                f1 = c(a = "f0"),
                f2 = c(a = "f0", b = "f1"),
                f3 = c(a = "f0", b = "f1", c = "f2")
            )

            expect_equal(f("f0", deps), character(0))
            expect_equal(f("f1", deps), c("f0"))
            expect_equal(f("f2", deps), c("f0", "f1"))
            expect_equal(f("f3", deps), c("f0", "f1", "f2"))
        })

        test_that("dependencies can be determined non-recursively",
        {
            deps <- list(
                f1 = c(a = "f0"),
                f2 = c(a = "f1"),
                f3 = c(a = "f2")
            )

            expect_equal(f("f0", deps, recursive = FALSE), character(0))
            expect_equal(f("f1", deps, recursive = FALSE), c("f0"))
            expect_equal(f("f2", deps, recursive = FALSE), c("f1"))
            expect_equal(f("f3", deps, recursive = FALSE), c("f2"))
        })

        test_that("works with multiple dependencies given in sublist",
        {
            deps <- list(
                f1 = c(a = "f0"),
                f2 = c(a = "f1", b = ".data"),
                f3 = list(
                    x = c("f1"),
                    y = c("f1", "f2")
                ),
                f4 = list(x = c("f3", ".data"))
            )

            expect_equal(f("f2", deps), c("f1", ".data", "f0"))
            expect_equal(f("f3", deps), c("f1", "f2", "f0", ".data"))
            expect_equal(f("f4", deps), c("f3", ".data", "f1", "f2", "f0"))
        })
    })



    # .relative_dependency_to_index
    test_that("private .relative_dependency_to_index works as expected",
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
                f(-1, dependency_name = NULL),
                "is_string(dependency_name) is not TRUE",
                fixed = TRUE
            )
            expect_error(
                f(-1, "dep-name", start_index = "2"),
                "is_number(start_index) is not TRUE",
                fixed = TRUE
            )
            expect_error(
                f(-1, "dep-name", start_index = -2),
                "start_index > 0 is not TRUE",
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
                    dependency_name = "dep-name",
                    start_index = 1,
                    step = "step-name"
                ),
                paste(
                    "step 'step-name': relative dependency dep-name=-10",
                    "points to outside the pipeline"
                ),
                fixed = TRUE
            )

            expect_error(f(-1, "dep-name", start_index = 1, "step-name"))
            expect_error(f(-2, "dep-name", start_index = 2, "step-name"))
        })

        test_that("returns correct index for relative dependency",
        {
            expect_equal(
                f(
                    relative_dep = -1,
                    dependency_name = "dep-name",
                    start_index = 3,
                    step = "step-name"
                ),
                2
            )
            expect_equal(
                f(
                    relative_dep = -2,
                    dependency_name = "dep-name",
                    start_index = 3,
                    step = "step-name"
                ),
                1
            )
        })
    })

    # .verify_dependency
    test_that("private .verify_dependency works as expected",
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
                f(dep = "dep", step = "step", to_step = 1),
                "is_string(to_step) is not TRUE",
                fixed = TRUE
            )
        })

        test_that("signals non existing to_step",
        {
            expect_error(
                f("dep", "step", to_step = "non-existent"),
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
                f(dep = "f2", "new-step", to_step = "f1"),
                "dependency 'f2' not found up to step 'f1'",
                fixed = TRUE
            )
        })
    })

    # .verify_fun_params
    test_that(".verify_fun_params works as expected",
    {
        pip <- expect_no_error(Pipeline$new("pipe"))
        f <- get_private(pip)$.verify_fun_params

        test_that("returns TRUE if args are defined with default values",
        {
            fun <- function(x = 1, y = 2) x + y
            expect_true(f(fun, "funcName", params = list()))
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



# pipe_add

test_that("pipe_add",
{
    expect_true(is.function(pipe_add))

    test_that("pipeline can be constructed with a pipe operator",
    {
        pip1 <- Pipeline$new("pipe1") |>
            pipe_add("f1", function(a = 1) a) |>
            pipe_add("f2", function(b = ~f1) b)

        pip2 <- Pipeline$new("pipe1")$
            add("f1", function(a = 1) a)$
            add("f2", function(b = ~f1) b)

        expect_equal(pip1, pip2)
    })
})
