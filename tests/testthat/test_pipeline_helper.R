
describe(".extract_fun_args",
{
    it("returns TRUE if function has no args",
    {
        expect_equal(
            .extract_fun_args(function() 1),
            list()
        )
    })

    it("returns default values for function arguments",
    {
        expect_equal(
            .extract_fun_args(function(x = 1) x),
            list(x = 1)
        )
        expect_equal(
            .extract_fun_args(function(x = 1, y = 2) x + y),
            list(x = 1, y = 2)
        )
    })

    it("signals parameters with no default values",
    {
        expect_error(
            .extract_fun_args(function(x, y = 1) x + y),
            "'x' has no default value",
            fixed = TRUE
        )

        expect_error(
            .extract_fun_args(function(x, y) x + y),
            "'x', 'y' have no default value",
            fixed = TRUE
        )
    })

    it("supports ...",
    {
        expect_equal(
            .extract_fun_args(function(x = 1, ...) x),
            list(x = 1)
        )
    })

    it("supports formula",
    {
        expect_equivalent(
            .extract_fun_args(function(x = 1, y = ~foo) x),
            list(x = 1, y = ~foo)
        )
    })
})


describe(".filter_dependencies",
{
    it("returns an empty character vector if no fargs are provided",
    {
        expect_equal(.filter_dependencies(fargs = list()), character(0))
    })

    it("returns an empty character vector if no dependencies are defined",
    {
        expect_equal(
            .filter_dependencies(fargs = list(a = 1)),
            character(0)
        )
        expect_equal(
            .filter_dependencies(fargs = list(a = 1, b = 2)),
            character(0)
        )
    })

    it("returns the dependencies if they are defined",
    {
        expect_equal(
            .filter_dependencies(fargs = list(a = ~x)),
            c(a = "x")
        )
        expect_equal(
            .filter_dependencies(fargs = list(a = ~x, b = ~-1)),
            c(a = "x", b = "-1")
        )
        expect_equal(
            .filter_dependencies(fargs = list(a = ~x, b = ~-1, c = 1)),
            c(a = "x", b = "-1")
        )
    })
})
