
describe(".empty_pipeline",
{
    it("returns an empty data.table",
    {
        dt <- .empty_pipeline()
        expect_true(data.table::is.data.table(dt))
        expect_equal(nrow(dt), 0)
    })
})


describe(".new_step",
{
    step <- .new_step(
        step = "step2",
        group = "group1",
        fun = function(x) x^2,
        params = list(x = 1, y = "step1"),
        depends = c(y = "step1"),
        tags = c("t1", "t2"),
        .nodeId = 0
    )

    it("contains the expected elements",
    {
        expect_equal(step$step, "step2")
        expect_equal(step$group, "group1")
        expect_equal(step$fun[[1]](2), 4)
        expect_equal(step$params[[1]], list(x = 1, y = "step1"))
        expect_equal(step$signature, "(x)")
        expect_equal(step$depends, list(c(y = "step1")))
        expect_equal(step$out, list(NULL))
        expect_equal(step$tags, list(c("t1", "t2")))
        expect_equal(step$state, "new")
        expect_true(inherits(step$time, "POSIXct"))
        expect_equal(step$locked, FALSE)
        expect_equal(step$meta, list(NULL))
        expect_equal(step$.nodeId, 0)
    })

    it("aligns with the empty pipeline",
    {
        expect_equal(names(step), names(.empty_pipeline()))
    })

    it("can be appended to the empty pipeline",
    {
        dt <- data.table::rbindlist(list(.empty_pipeline(), step))
        expect_equal(nrow(dt), 1)
    })
})


describe(".extract_fun_params",
{
    it("returns TRUE if function has no args",
    {
        expect_equal(
            .extract_fun_params(function() 1),
            list()
        )
    })

    it("returns default values for function arguments",
    {
        expect_equal(
            .extract_fun_params(function(x = 1) x),
            list(x = 1)
        )
        expect_equal(
            .extract_fun_params(function(x = 1, y = 2) x + y),
            list(x = 1, y = 2)
        )
    })

    it("signals parameters with no default values",
    {
        expect_error(
            .extract_fun_params(function(x, y = 1) x + y),
            "'x' has no default value",
            fixed = TRUE
        )

        expect_error(
            .extract_fun_params(function(x, y) x + y),
            "'x', 'y' have no default value",
            fixed = TRUE
        )
    })

    it("supports ...",
    {
        expect_equal(
            .extract_fun_params(function(x = 1, ...) x),
            list(x = 1)
        )
    })

    it("supports formula",
    {
        expect_equivalent(
            .extract_fun_params(function(x = 1, y = ~foo) x),
            list(x = 1, y = ~foo)
        )
    })
})


describe(".extract_depends",
{
    steps <- c("s1", "s2", "s3")

    it("returns an empty character vector if no params are defined",
    {
        expect_equal(
            .extract_depends(list(), steps),
            character(0)
        )
    })

    it("returns an empty character vector if no dependencies are defined",
    {
        expect_equal(
            .extract_depends(list(a = 1), steps),
            character(0)
        )
    })

    it("extracts all dependencies defined via step name",
    {
        expect_equal(
            .extract_depends(list(a = ~s1, b = 2), steps),
            c(a = "s1")
        )
        expect_equal(
            .extract_depends(list(a = ~s1, b = ~s2), steps),
            c(a = "s1", b = "s2")
        )
    })

    it("extracts all dependencies defined relative",
    {
        expect_equal(
            .extract_depends(list(x = ~-1), steps = c("f1", "f2")),
            c(x = "f1")
        )
        expect_equal(
            .extract_depends(list(a = ~-1, b = 2), steps),
            c(a = "s2")
        )
        expect_equal(
            .extract_depends(list(a = ~-2, b = ~-1), steps),
            c(a = "s1", b = "s2")
        )
    })

    it("signals bad steps input",
    {
        expect_error(
            .extract_depends(list(a = ~-1), character(0)),
            "startPos must be at least 1"
        )
    })
    it("signals relative index out of bound",
    {
        expect_error(
            .extract_depends(list(a = ~-1), "s1"),
            "relative index -1 points outside pipeline"
        )
        expect_error(
            .extract_depends(list(a = ~-4), steps),
            "relative index -4 points outside pipeline"
        )
    })

    it("extracts all dependencies if defined both ways",
    {
        expect_equal(
            .extract_depends(list(a = ~-1, b = ~s1, c = 3), steps),
            c(a = "s2", b = "s1")
        )
    })

    it("signals toPos exceeding number of steps",
    {
        expect_error(
            .extract_depends(list(), steps, 4L),
            "toPos exceeds number of steps"
        )
    })

    it("signals bad arg types",
    {
        f <- .extract_depends
        expect_error(
            f("not a list", steps),
            "params must be a list"
        )
        expect_error(
            f(list(), steps = list("not a character")),
            "steps must be a character vector"
        )
        expect_error(
            f(list(), steps, toPos = 2.0),
            "toPos must be an integer"
        )
    })
})
