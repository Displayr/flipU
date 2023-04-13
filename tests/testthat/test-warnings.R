context("warnings")

test_that("Warnings",
{
    expect_silent(ExpectWarning(warning("we have a problem"), "problem"))
    expect_silent(ExpectNoWarning(warning("we have a problem"), "issue"))
    expect_error(ExpectNoWarning(message("this is just a message, not a warning"), "this"), NA)
})

test_that("Collect Warnings",
{
    foo <- function()
    {
        for(cc in letters[1:10])
            as.numeric(cc)
        return(NULL)
    }
    expect_warning(CollectWarnings(foo()), "NAs introduced by coercion")
    expect_error(res <- CollectWarnings(foo(), return.list = TRUE), NA)
    expect_equal(length(res[[2]]), 1)
})

test_that("Intercept exceptions",
{
    swapAToB <- function(warn)
    {
        if (warn$message == "A")
            warning("B", call. = FALSE)
    }
    swapXToY <- function(error)
    {
        if (error$message == "X")
            stop("Y", call. = FALSE)
    }
    expect_silent({
        expect_error({
            ExpectWarning({
                InterceptExceptions({
                    warning("A", call. = FALSE)
                    stop("X", call. = FALSE)
                }, warning.handler = swapAToB, error.handler = swapXToY)
                }, "B")
        },"Y")
    })
})

test_that("Bar test",
{
    expect_error(warning("Foo warning"), NA)
})

test_that("Foo test",
{
    skip("Skipping just because'")
    expect_error(stop("Foo warning"), "Foo warning")
})
