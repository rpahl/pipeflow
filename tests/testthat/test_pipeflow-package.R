
describe(".this_package_name",
{
    it("returns the expected package name",
    {
        expect_equal(.this_package_name(), "pipeflow")
    })
})


describe(".this_package_path",
{
    it("constructs the package path as expected",
    {
        dir <- system.file(package = .this_package_name())

        res <- .this_package_path("some", "path")
        expected_path <- file.path(dir, "some", "path") |>
            normalizePath(winslash = "/", mustWork = FALSE)

        expect_equal(res, expected_path)
    })
})
