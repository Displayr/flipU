# Testing warnings

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

test_that("Checking for multiple data sets in the environment", {

    # Clear global environment for this test, to be restored at the
    # end of this test.
    global.as.list <- as.list(.GlobalEnv)
    temp.env <- environment()
    list2env(global.as.list, temp.env)
    remove(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)

    expect_warning(WarnIfVariablesSelectedFromMultipleDataSets(), NA)
    v1 <- c(1,2,3,4)
    attr(v1, "dataset") <- "File1.sav"
    v2 <- c(5,6,7,8)
    attr(v2, "dataset") <- "File2.sav"
    assign("v1", v1, envir = .GlobalEnv)
    assign("v2", v2, envir = .GlobalEnv)
    expected.warn <- paste0("The selected data come from more than one Data Set. ",
            "The data sets may have different lengths, and the cases ",
            "may not be in the same order. The data sets used are: ",
            paste0(c("File1.sav", "File2.sav"), collapse = ", "))
    expect_warning(WarnIfVariablesSelectedFromMultipleDataSets(), expected.warn)
    list2env(global.as.list, .GlobalEnv)
})
