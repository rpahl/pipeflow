
test_that("Param class",
{
    expect_no_error(getClass("Param"))

    test_that("Param is a virtual class that cannot be instantiated",
    {
        expect_true(getClass("Param")@virtual)
        expect_error(new("Param"), "virtual class")
    })
})

test_that("it can be tested if an object is a Param",
{
    expect_true(methods::is(new("StringParam", "foo"), "Param"))
    expect_true(methods::is(new("BooleanParam", "foo"), "Param"))

    expect_false(methods::is(1, "Param"))
    expect_false(methods::is(NULL, "Param"))
    expect_false(methods::is(NA, "Param"))
})



test_that("BooleanParam class",
{
    expect_no_error(getClass("BooleanParam"))

    test_that("value of Boolean must be of length 1",
    {
        name = "my bool"

        p = new("BooleanParam", name = name, value = TRUE)
        expect_true(is.logical(p@value))

        p = new("BooleanParam", name = name, value = FALSE)
        expect_true(is.logical(p@value))

        ee <- expect_equal
        ee(new("BooleanParam", name = name, value = "TRUE")@value, TRUE)
        ee(new("BooleanParam", name = name, value = "true")@value, TRUE)
        ee(new("BooleanParam", name = name, value = "FALSE")@value, FALSE)
        ee(new("BooleanParam", name = name, value = "false")@value, FALSE)
        ee(new("BooleanParam", name = name, value = 1)@value, TRUE)
        ee(new("BooleanParam", name = name, value = 0)@value, FALSE)

        expect_error(new("BooleanParam", name = name, value = c(TRUE, FALSE)))
    })

    test_that("alternative constructor works",
    {
        name = "my bool"

        p1 = new("BooleanParam", name = name, value = TRUE)
        p2 = BooleanParam(name = name, value = TRUE)
        expect_equal(p1, p2)
    })
})



test_that("CategoricalParam class",
{
    expect_no_error(getClass("CategoricalParam"))

    test_that(
        "value of Categorical parameter must be a string from the choices",
    {
        name = "my param"
        choices = c("a", "b", "c")

        f = function(value) {
            new(
                "CategoricalParam",
                name = name,
                value = value,
                choices = choices
            )
        }

        p = f("a")
        expect_true(is_string(p@value))
        expect_equal(p@value, "a")
        expect_equal(p@choices, choices)

        expect_no_error(f("b"))
        expect_no_error(f("c"))

        expect_error(f(NULL))
        expect_error(f(as.character(NA)))
        expect_error(f("d"))
        expect_error(f(c("a", "b")))
        expect_error(CategoricalParam(name = name, value = "a", choices = "x"))
    })

    test_that("choices must be a character vector with length > 0",
    {
        ee = expect_error
        ee(CategoricalParam(name = "foo", value = "a", choices = character(0)))
        ee(CategoricalParam(name = "foo", value = "1", choices = 1:3))
        ee(CategoricalParam(name = "foo", value = "a", factor("a")))
    })

    test_that("alternative constructor works",
    {
        p1 = new("CategoricalParam", name = "foo", value = "a", choices = "a")
        p2 = CategoricalParam(name = "foo", value = "a", choices = "a")
        expect_equal(p1, p2)
    })
})



test_that("DataframeParam class",
{
    expect_no_error(getClass("DataframeParam"))

    test_that("value of DataframeParam is the passed data.frame",
    {
        daf = data.frame(x = 1:2, y = 1:2)
        p = new("DataframeParam", name = "my df", value = daf)
        expect_equal(p@value, daf)
    })

    test_that("alternative constructor works",
    {
        daf = data.frame(x = 1:2, y = 1:2)
        p1 = new("DataframeParam", name = "my df", value = daf)
        p2 = DataframeParam(name = "my df", value = daf)
        expect_equal(p1, p2)
    })
})



test_that("ListParam class",
{
    expect_no_error(getClass("ListParam"))

    test_that("value of ListParam must be a list",
    {
        name = "my list"

        expect_error(new("ListParam", name = name, value = 1:2))
        expect_error(new("ListParam", name = name, value = "bla"))

        value = list(a = 1, b = 2)
        p = new("ListParam", name = name, value = value)
        expect_equal(p@value, value)
    })

    test_that("NULL or undefined is transformed to empty list",
    {
        name = "my list"

        p = new("ListParam", name = name, value = NULL)
        expect_equal(p@value, list())

        p = new("ListParam", name = name, value = NA)
        expect_equal(p@value, list())
    })


    test_that("alternative constructor works",
    {
        name = "my list"
        value = list(1, 2)

        p1 = new("ListParam", name = name, value = value)
        p2 = ListParam(name = name, value = value)
        expect_equal(p1, p2)
    })
})



test_that("NumericParam class",
{
    expect_no_error(getClass("NumericParam"))

    test_that("value of NumericParam is converted to number",
    {
        name = "my num"
        f = function(value)
            new("NumericParam", name = name, value = value)

        p = f(0.1)
        expect_true(is_number(p@value))
        expect_equal(p@value, 0.1)

        p = f(NULL)
        expect_equal(p@value, as.numeric(NA))

        p = f(as.numeric(NA))
        expect_equal(p@value, as.numeric(NA))

        expect_equal(f("1")@value, 1)
        expect_equal(f(as.factor(1))@value, 1)
    })

    test_that("alternative constructor works",
    {
        p1 = new("NumericParam", name = "foo", value = 1.1)
        p2 = NumericParam(name = "foo", value = 1.1)
        expect_equal(p1, p2)
    })

    test_that("value of NumericParam must be of length 1",
    {
        expect_error(new("NumericParam", name = name, value = 1:2))
    })


    test_that("value must be within min/max",
    {
        f = function(...) new("NumericParam", name = "foo", ...)

        expect_no_error(f(value = 0, min = 0, max = 0))
        expect_error(f(value = 0, min = 1))
        expect_error(f(value = 1, max = 0))
    })

    test_that("min must be >= max",
    {
        f = function(...) new("NumericParam", name = "foo", value = 0, ...)

        expect_no_error(f(min = 0, max = 0))
        expect_error(f(min = 1, max = 0))
    })
})



test_that("StringParam class",
{
    expect_no_error(getClass("StringParam"))

    test_that("name sets the name and value the value",
    {
        name = "my string"
        value = "foo"
        p = new("StringParam", name = name, value = value)

        expect_equal(p@name, name)
        expect_equal(p@value, value)
    })

    test_that("name of parameter must be a non-empty, defined string",
    {
        name = "my string"
        expect_no_error(new("StringParam", name = name))

        expect_error(new("StringParam", name = ""))
        expect_error(new("StringParam", name = as.character(NA)))

        expect_error(new("StringParam", name = as.factor(name)))
        expect_error(new("StringParam", name = 1))
        expect_error(new("StringParam", rep(name, 2)))
    })


    test_that("parameter is non-advanced by default",
    {
        p = new("StringParam", name = "my string", value = "my value")
        expect_false(p@advanced)
    })

    test_that("parameter can be set advanced",
    {
        p = StringParam(name = "my string", value = "my value", advanced = TRUE)
        expect_true(p@advanced)
    })

    test_that("advanced flag must be be boolean of length 1",
    {
        expect_error(
            StringParam(
                "my string", "my value",
                advanced = c(TRUE, FALSE)
            )
        )
        expect_error(StringParam("my string", "my value", advanced = "no"))
    })

    test_that("parameter source is 'internal' by default",
    {
        p = new("StringParam", name = "my string", value = "my value")
        expect_equal(p@source, "internal")
    })


    test_that("parameter has a customizable label by default set to it's name",
    {
        name = "my string"
        value = "my value"
        p = new("StringParam", name, value)
        expect_equal(p@label, p@name)

        p = new("StringParam", name, value, label = "my custom label")
        expect_equal(p@label, "my custom label")
    })

    test_that(
        "parameter has a customizable description which is empty by default",
    {
        name = "my string"
        value = "my value"
        p = StringParam(name, value)

        expected_default_description = ""
        expect_equal(p@description, "")

        p = StringParam(name, value, description = "This is a nice string.")
        expect_equal(p@description, "This is a nice string.")
    })

    test_that("value of StringParam must be a string",
    {
        name = "my string"
        value = "my value"

        expect_error(new("StringParam", name = name, value = rep(value, 2)))

        p = new("StringParam", name = name, value = value)
        expect_true(is_string(p@value))

        p = new("StringParam", name = name, value = as.character(NA))
        expect_true(is_string(p@value))
    })

    test_that("NULL or numeric is transformed to string",
    {
        name = "my string"
        value = "my value"

        p = new("StringParam", name = name, value = NULL)
        expect_true(is_string(p@value))
        expect_equal(p@value, as.character(NA))

        p = new("StringParam", name = name, value = 1)
        expect_equal(p@value, "1")
    })


    test_that("alternative StringParam constructor works",
    {
        name = "my string"
        value = "my value"

        p1 = new("StringParam", name = name, value = value)
        p2 = StringParam(name = name, value = value)
        expect_equal(p1, p2)
    })
})




# Param list to and from json

test_that("when transforming param list to json, list names are taken over",
{
    p1 = StringParam(name = "my string", value = "hello")
    p2 = NumericParam(name = "my number", value = 5, min = 0, max = 10)
    p3 = DataframeParam(name = "p3", value = data.frame(x = 1:4, y = 1:4))

    l = list(p1 = p3, p2 = p2)
    ll = param_list_from_json(param_list_to_json(l))
    expect_equal(names(ll), names(l))
})


test_that("Param list can be converted back and forth to and from json",
{
    p1 = StringParam(name = "p1", value = "hello")
    p2 = NumericParam(name = "p2", value = 5, min = 0, max = 10)
    p3 = DataframeParam(name = "p3", value = data.frame(x = 1:4, y = 1:4))

    l = list(p1 = p1, p2 = p2, p3 = p3)
    json = param_list_to_json(l[1:2])
    expect_true(is(json, "json"))

    ll = param_list_from_json(json)
    expect_equal(l[1:2], ll)


    ll = param_list_from_json(param_list_to_json(l))
    expect_equal(l, ll)
})
