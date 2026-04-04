
describe(".extract_default_values",
{
    it("returns TRUE if function has no args",
    {
        expect_equal(
            .extract_default_values(function() 1),
            list()
        )
    })

    it("returns default values for function arguments",
    {
        expect_equal(
            .extract_default_values(function(x = 1) x),
            list(x = 1)
        )
        expect_equal(
            .extract_default_values(function(x = 1, y = 2) x + y),
            list(x = 1, y = 2)
        )
    })

    it("signals parameters with no default values",
    {
        expect_error(
            .extract_default_values(function(x, y = 1) x + y),
            "'x' has no default value",
            fixed = TRUE
        )

        expect_error(
            .extract_default_values(function(x, y) x + y),
            "'x', 'y' have no default value",
            fixed = TRUE
        )
    })

    it("supports ...",
    {
        expect_equal(
            .extract_default_values(function(x = 1, ...) x),
            list(x = 1)
        )
    })

    it("supports formula",
    {
        expect_equivalent(
            .extract_default_values(function(x = 1, y = ~foo) x),
            list(x = 1, y = ~foo)
        )
    })
})
