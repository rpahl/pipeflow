
# .this_package_name

test_that("package name is correct",
{
    expect_equal(.this_package_name(), "pipeflow")
})

# .this_package_path

test_that("path is constructed as expected",
{
    dir <- system.file(package = .this_package_name())

    res <- .this_package_path("some", "path")
    expected_path <- file.path(dir, "some", "path") |>
        normalizePath(winslash = "/", mustWork = FALSE)

    expect_equal(res, expected_path)

})
